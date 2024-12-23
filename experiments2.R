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
nd <- sleepstudy |> dplyr::select(Days) |> distinct() |> mutate(Subject="308")

p0 <- proc.time()

source("funs.R")

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

model_cc2_gt <- glmmTMB(reac_binom ~ Days + (Days | Subject),
                        data = ss, family = binomial(link = "cloglog"))

## fails to converge
## model_cc2_ga <- mixed_model(reac_binom ~ Days,
##                             random = ~ 1 +Days | Subject,
##                             data = ss,
##                             family = binomial(link = "cloglog"),
##                             control = list(nAGQ=1, iter_EM=50))


ci_cc2 <- compare_intervals(model_cc2, verbose = TRUE,  newdata = nd)
plot_intervals(ci_cc2) ## + facet_wrap(~method)

## now look at profiles and expanded bootMer (save beta, theta as well as preds)

pp <- profile(model_cc2, devmatchtol = Inf, devtol = Inf, verbose = 100)

mod_list <- list(glmer=model_cc2, glmmTMB=model_cc2_gt)

(mod_list
    |> purrr::map_dfr(glance, .id = "model")
    |> transmute(model, nll = -1*logLik, AIC)
    |> mutate(across(c(nll, AIC), ~ . - min(.)))
)

(mod_list
    |> purrr::map_dfr(\(x) tidy(x),
                  .id = "model")
)


gt_to_lmer_pars <- function(fitted) {
    get_theta <- function(x) {
        cc <- t(chol(x))
        cc <- cc[lower.tri(cc, diag = TRUE)]
        return(cc)
    }
    list(theta = unlist(lapply(VarCorr(fitted)$cond, get_theta)),
         fixef = getME(fitted, "beta")
         )
}

pars2 <- gt_to_lmer_pars(model_cc2_gt)
pars3 <- relist(c(1.65118649623164,-0.583925451262354,
                  1.70076652626002,1.75873442173342,0.281955921179663),
                skel = pars2)
pars4 <- relist(c(2.72843960725495,-0.748498839293289,3.20299037036345,2.08538782041507,0.3475999140312),
                skel = pars2)

pars5 <- relist(c(3.05187379946649,-0.799461193362596,3.36250765654948,2.23055283288509,0.15896294223461),
                skel = pars2)

logLik(model_cc3 <- update(model_cc2, start = pars4))
performance::check_singularity(model_cc2_gt)
det(VarCorr(model_cc2_gt)$cond$Subject)


pp <- profile(model_cc3, devmatchtol = 1e-3, devtol = 5e-3, verbose = TRUE)
ggplot(as.data.frame(pp),
       aes(.focal, .zeta)) + geom_point() +
    facet_wrap(~.par, scale = "free")

aa <- allFit(model_cc2)
summary(aa)$llik

logLik(model_cc2_gt)

p2 <- predict(model_cc2_gt)
p1 <- predict(model_cc2)
plot(p1, abs((p1-p2)/p1))
plot(p1, abs(p1-p2))

devfun <- getME(model_cc2, "devfun")
logLik(model_cc2)
devfun(unlist(getME(model_cc2, c("theta", "fixef"))))/2
devfun(unlist(getME(model_cc2, c("theta", "fixef"))))/2
devfun(unlist(pars3))/2
logLik(model_cc3)
