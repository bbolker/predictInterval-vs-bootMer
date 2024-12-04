library(dplyr)
library(lme4)
library(merTools)


load("reprex_pred_lwdu.RData")
# subsetting the data, still reproduces issue
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
                        nsim = 10, # for faster compute, estimate doesn't change much with high sims
                        FUN = pred_fun, 
                        seed = 1000, use.u = FALSE, type = "parametric", 
                        parallel = "no", ncpus = par_cores)

bootmer_mean <- median(na.omit(pred_bootMer$t[,1]))
bootmer_se <- sd(na.omit(pred_bootMer$t[,1]))
# tictoc::toc()


# predictInterval

# tictoc::tic("predictInterval")
model_predint = predictInterval(model, newdata = data_to_pred, which = "fixed",
                                level = 0.48, type = "linear.prediction")
predint_mean <- model_predint$fit
predint_se <- model_predint$fit - model_predint$lwr
# tictoc::toc()


# print comparisons
paste("Means: bootMer =", bootmer_mean, ", predictInterval =", predint_mean[1])
paste("SEs: bootMer =", bootmer_se, ", predictInterval =", predint_se[1])
