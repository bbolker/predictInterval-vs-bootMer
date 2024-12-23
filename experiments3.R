library(dplyr)
library(tidyr)
library(lme4)
library(merTools)
library(glmmTMB)
library(GLMMadaptive)
library(broom.mixed)
library(ggplot2); theme_set(theme_bw())
par_cores <- 8L
options("glmmTMB.cores" = par_cores, "glmmTMB.autopar" = FALSE)

p0 <- proc.time()

source("funs.R")

load("reprex_pred_lwdu.RData")
set.seed(101) ## going to subsample data randomly ...
n_sub <- 1e4
n_boot <- 500
data_lwdu <- slice_sample(data_lwdu, n = n_sub)

data_to_pred <- data_lwdu |>
  distinct(month, timegroups) |>
  mutate(no.sp = 15,
         gridg1 = data_lwdu$gridg1[1], 
         gridg3 = data_lwdu$gridg3[1]) |>
    as.data.frame()  ## avoid predictInterval warning


t_lme4 <- system.time(
    model_lme4 <- glmer(OBSERVATION.COUNT ~ month + month:log(no.sp) + timegroups +
                       (1|gridg3/gridg1), 
                   data = data_lwdu, family = binomial(link = 'cloglog'), 
                   nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
)

pfun <- function(x, ret_val = c("vec", "list")) {
    ret_val <- match.arg(ret_val)
    pp <- predict(x, newdata = data_to_pred, re.form = NA,
                  allow.new.levels = TRUE)
    th <- getME(x, "theta")
    bv <- getME(x, "beta")
    res <- list(pred = pp, theta = th, beta = bv)
    if (ret_val == "list") return(res)
    return(unlist(res))
}

bb <- bootMer(model_lme4, nsim = n_boot,
                      FUN = pfun,
                      seed = 1000, use.u = FALSE, type = "parametric",
                      parallel = "multicore", ncpus = par_cores)

saveRDS(bb, "bb_lme4.RDS")
dim(bb$t)

b0 <- pfun(model_lme4, ret_val = "list")

relist(bb$t[1,], skel = b0)

bb_res <- bb$t[bb$t[,"beta6"] > -5 &
               bb$t[,"beta1"] > -15, ]

nrow(bb$t) - nrow(bb_res)  ## excluded 16 cases

b0_v <- pfun(model_lme4)


plot_pairs <- function(inds = c(1:3, 61:84),
                       pt.args = list(pch = ".", col = "gray"),
                       est.args = list(pch = 16, col = "red"),
                       med.args = list(pch = 17, col = "blue"),
                       mean.args = list(pch = 18, col = "purple"),
                       cex.sum = 2) {
    pairs(bb_res[,inds], gap = 0,
          panel = function(x, y, ...) {
              i <- parent.frame(2)$i
              j <- parent.frame(2)$j
              do.call(points,
                      c(list(x = x, y = y), list(...), pt.args))
              do.call(points,
                      c(list(x = b0_v[inds[j]], y = b0_v[inds[i]],
                             cex = cex.sum), est.args))
              do.call(points,
                      c(list(x = mean(x), y = mean(y), cex = cex.sum),
                        mean.args))
              do.call(points,
                      c(list(x = median(x), y = median(y), cex = cex.sum),
                        med.args))
      })
}

plot_pairs(cex.sum=1)
plot_pairs(inds = c(1:3, 61:68))

## can this all be explained by the bias in the bootstrap est of the
##  fixed effect intercept?
