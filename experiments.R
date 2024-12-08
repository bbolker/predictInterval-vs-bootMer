library(dplyr)
library(tidyr)
library(lme4)
library(merTools)
library(glmmTMB)
library(ggplot2); theme_set(theme_bw())
par_cores <- 8L
options("glmmTMB.cores" = par_cores, "glmmTMB.autopar" = FALSE)

source("funs.R")

## redo SOIB example, but with fewer samples, and with a more
##  thorough 
load("reprex_pred_lwdu.RData")
## subsetting the data, still reproduces issue
set.seed(101) ## going to subsample data randomly ...
n_sub <- 1e4
n_boot <- 100
data_lwdu <- slice_sample(data_lwdu, n = n_sub)

# model
t_lme4 <- system.time(
    model_lme4 <- glmer(OBSERVATION.COUNT ~ month + month:log(no.sp) + timegroups +
                       (1|gridg3/gridg1), 
                   data = data_lwdu, family = binomial(link = 'cloglog'), 
                   nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
)

data_to_pred <- data_lwdu |>
  distinct(month, timegroups) |>
  mutate(no.sp = 15,
         gridg1 = data_lwdu$gridg1[1], 
         gridg3 = data_lwdu$gridg3[1]) |>
    as.data.frame()  ## avoid predictInterval warning


ci_lme4 <- compare_intervals(model_lme4, verbose = TRUE, newdata = data_to_pred)
plot_intervals(ci_lme4) + facet_wrap(~method)


ss <- lmer(Reaction ~ Days + (Days | Subject),
            data = sleepstudy)


## convert glmer call to glmmTMB (eliminate unused/unneeded args)
##  (consider profile = TRUE to match nAGQ=0 ?)
cc <- getCall(model)
cc[[1]] <- quote(glmmTMB)
cc$nAGQ <- cc$control <- NULL
t_glmmTMB <- system.time(
    model_glmmTMB <- eval(cc)
)

## equivalent to nAGQ=0
t_glmmTMB.prof <- system.time(
    model_glmmTMB.prof <- update(model_glmmTMB,
                                 control = glmmTMBControl(profile = TRUE))
)


pred_fun <- function(input_model, ret_val = c("vector", "list"))  {
    ret_val <- match.arg(ret_val)
    p0 <- predict(input_model, newdata = data_to_pred, re.form = NA,
                  allow.new.levels = TRUE)
    out <- c(getME(input_model, c("theta", "fixef")),
             list(pred = p0))
    if (ret_val == "vector") out <- unlist(out)
    return(out)
}

skel <- pred_fun(model_lme4, ret_val = "list")
results <- relist(pred_fun(model_lme4), skeleton = skel)

# tictoc::tic("bootMer 10 sims")
t_boot <- system.time(
    pred_bootMer <- bootMer(model_lme4, 
                        nsim = n_boot,
                        FUN = pred_fun, 
                        seed = 1000,
                        use.u = FALSE, type = "parametric", 
                        parallel = "yes", ncpus = par_cores)
)

preds <- t(apply(pred_bootMer$t, 1,
      \(x) {
          r <- relist(x, skeleton =  skel)
          r$pred
      }))

## try this for a "known-good" example
set.seed(101)
sim_data <- expand.grid(rep = 1:20, g = factor(1:20))
sim_data$x <- rnorm(nrow(sim_data))
sim_data$y <- simulate(~ 1 + x + (1 + x|g),
                       newdata = sim_data,
                       family = binomial(link = "cloglog"),
                       newparams = list(theta = c(1,0,1),
                                        beta = c(0, 1)))[[1]]

model_sim <- glmer(y ~ 1 + x + (1 + x|g),
                 data = sim_data,
                 family = binomial(link = "cloglog"))

## predictIntervals doesn't work with newdata = NULL
compare_intervals(model_sim, newdata = sim_data, seed = 101)

## untested binomial GLMM warning
## approximation warning from predict.merMod

## predictInterval         bootMer      predict.se 
##       2.9255899       2.4023305       0.7104698 

model_sim2 <- lmer(Reaction ~ 1 + Days + (Days | Subject), sleepstudy)
compare_intervals(model_sim2, newdata = sleepstudy, seed = 101)
## predictInterval         bootMer      predict.se 
##        75.66799        95.18719        24.90286 


