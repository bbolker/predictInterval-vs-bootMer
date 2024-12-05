library(dplyr)
library(lme4)
library(merTools)
n_boot <- 100
par_cores <- 8
clev <- 0.48  ## why???

load("reprex_pred_lwdu.RData")
## subsetting the data, still reproduces issue
set.seed(101)
data_lwdu <- slice_sample(data_lwdu, n = 100000) 


# model
model <- glmer("OBSERVATION.COUNT ~ month + month:log(no.sp) + timegroups + (1|gridg3/gridg1)", 
               data = data_lwdu, family = binomial(link = 'cloglog'), 
               nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))

# dataframe to predict
data_to_pred <- data_lwdu %>% 
  distinct(month, timegroups) %>% 
  mutate(no.sp = 15,
         gridg1 = data_lwdu$gridg1[1], 
         gridg3 = data_lwdu$gridg3[1])


# bootMer

pred_fun <- function(input_model) {
  predict(input_model, newdata = data_to_pred, re.form = NA, allow.new.levels = TRUE)
  # not specifying type = "response" because will later transform prediction along with SE
}

# tictoc::tic("bootMer 10 sims")
pred_bootMer <- bootMer(model, 
                        nsim = n_boot, # for faster compute, estimate doesn't change much with high sims
                        FUN = pred_fun, 
                        seed = 1000, use.u = FALSE, type = "parametric", 
                        parallel = "multicore", ncpus = par_cores)

bootmer_mean <- median(na.omit(pred_bootMer$t[,1]))
bootmer_se <- sd(na.omit(pred_bootMer$t[,1]))
# tictoc::toc()


# predictInterval

# tictoc::tic("predictInterval")
model_predint = predictInterval(model, newdata = data_to_pred, which = "fixed",
                                level = clev, type = "linear.prediction")
predint_mean <- model_predint$fit
predint_se <- model_predint$fit - model_predint$lwr
# tictoc::toc()

model_predfit = predict(model, newdata = data_to_pred,
                        se.fit = TRUE)


levs <- c((1-clev)/2, (1+clev)/2)
qq <- qnorm(levs)
model_predmat = with(model_predfit,
                     data.frame(fit = fit,
                                lwr = fit + qq[1]*se.fit,
                                upr = fit + qq[2]*se.fit))

# print comparisons
paste("Means: bootMer =", bootmer_mean, ", predictInterval =", predint_mean[1])
paste("SEs: bootMer =", bootmer_se, ", predictInterval =", predint_se[1])

save(list = c("pred_bootMer", "model_predint", "model_predmat",
              "predint_mean", "bootmer_mean", "predint_se", "bootmer_se"),
     file = "soib_reprex_orig.rda")
