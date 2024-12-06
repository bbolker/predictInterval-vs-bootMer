compare_intervals <- function(model, newdata = NULL, level = 0.8,
                              nboot = 100, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)
    pi <- predictInterval(model, newdata = newdata,
                          type = "linear.prediction", level = level)
    bb <- bootMer(model, nsim = nboot, FUN = predict,
                  seed = 1000, use.u = FALSE, type = "parametric",
                  parallel = "yes", ncpus = par_cores)
    bb2 <- bootMer(model, nsim = nboot, FUN = predict,
                  seed = 1000, use.u = TRUE, type = "parametric",
                  parallel = "yes", ncpus = par_cores)
    p2 <- predict(model, newdata = newdata, se.fit = TRUE,
                  re.form = NA)
    levs <- c((1-level)/2, (1+level)/2)
    qci_wid <- \(x) diff(quantile(x, levs))
    c(
        ## mean CI width
        predictInterval = with(pi, mean(upr-lwr)),
        bootMer = mean(apply(bb$t, 2, qci_wid)),
        bootMer.useu = mean(apply(bb2$t, 2, qci_wid)),
        predict.se =  mean(p2$se.fit*diff(qnorm(levs)))
    )
}
