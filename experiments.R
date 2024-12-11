library(dplyr)
library(tidyr)
library(lme4)
library(merTools)
library(glmmTMB)
library(ggplot2); theme_set(theme_bw())
par_cores <- 8L
options("glmmTMB.cores" = par_cores, "glmmTMB.autopar" = FALSE)

p0 <- proc.time()

source("funs.R")

## redo SOIB example, but with fewer samples, and with a more
##  thorough 
load("reprex_pred_lwdu.RData")

## FULL version
set.seed(101)
data_lwdu_f <- slice_sample(data_lwdu, n = 1e5)
t_lme4_f <- system.time(
    model_lme4_f <- glmer(OBSERVATION.COUNT ~ month + month:log(no.sp) + timegroups +
                       (1|gridg3/gridg1), 
                   data = data_lwdu_f, family = binomial(link = 'cloglog'), 
                   nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
)

## subsetting the data, still reproduces issue
set.seed(101) ## going to subsample data randomly ...
n_sub <- 1e4
n_boot <- 100
data_lwdu <- slice_sample(data_lwdu, n = n_sub)

data_to_pred <- data_lwdu |>
  distinct(month, timegroups) |>
  mutate(no.sp = 15,
         gridg1 = data_lwdu$gridg1[1], 
         gridg3 = data_lwdu$gridg3[1]) |>
    as.data.frame()  ## avoid predictInterval warning

ci_lme4_f <- compare_intervals(model_lme4_f, verbose = TRUE, newdata = data_to_pred)

# model
t_lme4 <- system.time(
    model_lme4 <- glmer(OBSERVATION.COUNT ~ month + month:log(no.sp) + timegroups +
                       (1|gridg3/gridg1), 
                   data = data_lwdu, family = binomial(link = 'cloglog'), 
                   nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
)

ci_lme4 <- compare_intervals(model_lme4, verbose = TRUE, newdata = data_to_pred)
plot_intervals(ci_lme4) + facet_wrap(~method)

## is predictInterval somehow ignoring levels?

model_ss <- lmer(Reaction ~ Days + (Days | Subject),
           data = sleepstudy)
nd <- sleepstudy |> dplyr::select(Days) |> distinct() |> mutate(Subject="308")

ci_ss <- compare_intervals(model_ss, verbose = TRUE,  newdata = nd)
plot_intervals(ci_ss) ## + facet_wrap(~method)


data("Contraception", package = "mlmRev")
model_cc <- glmer(use ~ livch*poly(age, 2) + (urban|district),
                  data = Contraception, family = binomial(link = "logit"))
nd_cc <- with(Contraception, expand.grid(livch = levels(livch),
                                         age = seq(15, 45, 5),
                                         urban = levels(urban)))
nd_cc$district <- Contraception$district[1]

ci_cc <- compare_intervals(model_cc, verbose = TRUE,  newdata = nd_cc)
plot_intervals(ci_cc) ## + facet_wrap(~method)

## uglier but not as far off as original ...
## what about a simple simulated example?
## (perhaps too small to get reliable answers with a Bernoulli outcome but ... ?)

ss <- sleepstudy
ss$reac_binom <- simulate(~ 1 + Days + (1 + Days | Subject),
                          newdata = ss,
                          seed = 101,
                          family = binomial(link = "cloglog"),
                          newparams = list(theta = c(1,0,1),
                                           beta = c(0, 1)))[[1]]

model_cc2 <- glmer(reac_binom ~ Days + (Days | Subject),
                   data = ss, family = binomial(link = "cloglog"))

ci_cc2 <- compare_intervals(model_cc2, verbose = TRUE,  newdata = nd)
plot_intervals(ci_cc2) ## + facet_wrap(~method)

## larger example: theta should be better determined, so differences
##  between bootMer and predictInterval/XvXt should be smaller ...
set.seed(101)
sim_data <- expand.grid(rep = 1:20, g = factor(1:50))
sim_data$x <- rnorm(nrow(sim_data))
sim_data$y <- simulate(~ 1 + x + (1 + x|g),
                       newdata = sim_data,
                       family = binomial(link = "cloglog"),
                       newparams = list(theta = c(1,0,1),
                                        beta = c(0, 1)))[[1]]


model_sim <- glmer(y ~ 1 + x + (1 + x|g),
                 data = sim_data,
                 family = binomial(link = "cloglog"))

nd3 <- data.frame(x = seq(min(sim_data$x), max(sim_data$x), length.out = 21),
                  g = factor(1))

ci_cc3 <- compare_intervals(model_sim, verbose = TRUE,  newdata = nd3)
plot_intervals(ci_cc3)

## smooth; here predictInterval() and XVXt are similar, but diverge
## from bootMer (more what I expected)
vars_ci <- ls(pattern="^ci_")  ## don't call this ci_list ...
save(list = vars_ci, file = "predint_ci.rda")
save(list = c("model_lme4", "data_to_pred"), file = "model_lme4.rda")

## do timing by hand ...
print(proc.time() - p0)
quit()


## leftover junk below here ...

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



