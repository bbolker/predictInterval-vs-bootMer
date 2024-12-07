compare_intervals <- function(model,
                              methods = c("bootMer",
                                          "predictInterval.fixed",
                                          "XVXt"                                          ## skip "predict.se", "bootMer.useu by default
                                          ),
                              re.form = NA,
                              newdata = NULL,
                              level = 0.8,
                              nboot = 100,
                              seed = NULL,
                              verbose = FALSE) {
    if (!is.null(seed)) set.seed(seed)
    levs <- c((1-level)/2, (1+level)/2)
    qq <- qnorm(levs)
    res <- list()
    p <- predict(model, newdata = newdata, re.form = NA)
    newdata <- newdata[order(p),]
    p <- sort(p)
    if ("predictInterval.fixed" %in% methods) {
        if (verbose) cat("predictInterval.fixed\n")
        res$predictInterval.fixed <-
            predictInterval(model, newdata = newdata,
                            type = "linear.prediction",
                            which = "fixed", level = level)
    }
    if ("XVXt" %in% methods) {
        if (verbose) cat("XVXt\n")
        X <- model.matrix(formula(model, fixed.only = TRUE)[-2], data = newdata)
        ## fixme:: $cond for glmmTMB
        V <- vcov(model)
        se <- sqrt(diag(X %*% V %*% t(X)))
        res$XVXt <- data.frame(lwr = p - qq[1]*se, upr = p + qq[2]*se)
    }
    if ("bootMer" %in% methods) {
        if (verbose) cat("bootMer\n")
        bb <- bootMer(model, nsim = nboot, FUN = predict,
                      seed = 1000, use.u = FALSE, type = "parametric",
                      parallel = "yes", ncpus = par_cores)
        res$bootMer <- apply(bb$t, 2, \(x) quantile(x, levs)) |>
            t() |>
            as.data.frame() |>
            setNames(c("lwr", "upr"))
    }
    if ("bootMer.useu" %in% methods) {
        if (verbose) cat("bootMer.useu\n")
        bb2 <- bootMer(model, nsim = nboot, FUN = predict,
                       seed = 1000, use.u = TRUE, type = "parametric",
                       parallel = "yes", ncpus = par_cores)
        res$bootMer.useu <- apply(bb2$t, 2, \(x) quantile(x, levs))
    }
    if ("predict.se" %in% methods) {
        if (verbose) cat("predict.se\n")
        p2 <- predict(model, newdata = newdata, se.fit = TRUE,
                      re.form = NA)
        res$predict.se <- with(p2, data.frame(lwr = fit - qq[1]*se.fit,
                                              upr = fit + qq[2]*se.fit))
    }
    return(res)
}

plot_intervals <- function(res) {
    require(ggplot2)
    require(purrr)
    require(tidyr)
    require(dplyr)
    bind_rows(res, .id = "method")

}
