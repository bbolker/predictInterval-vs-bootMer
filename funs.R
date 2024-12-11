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
    newdata <- newdata[order(p),,drop = FALSE]
    p <- sort(p)
    if ("predictInterval.fixed" %in% methods) {
        if (verbose) cat("predictInterval.fixed\n")
        pi <- predictInterval(model, newdata = newdata,
                            type = "linear.prediction",
                            which = "fixed", level = level,
                            include.resid.var = FALSE)
        ## reorder
        res$predictInterval.fixed <- pi[c("fit", "lwr", "upr")]
    }
    if ("XVXt" %in% methods) {
        if (verbose) cat("XVXt\n")
        X <- model.matrix(formula(model, fixed.only = TRUE)[-2], data = newdata)
        ## fixme:: $cond for glmmTMB
        V <- vcov(model)
        se <- sqrt(diag(X %*% V %*% t(X)))
        res$XVXt <- data.frame(fit = p, lwr = p + qq[1]*se, upr = p + qq[2]*se)
    }
    if ("bootMer" %in% methods) {
        if (verbose) cat("bootMer\n")
        bb <- bootMer(model, nsim = nboot,
                      FUN = \(x) predict(x, newdata = newdata, re.form = NA,
                                         allow.new.levels = TRUE),
                      seed = 1000, use.u = FALSE, type = "parametric",
                      parallel = "multicore", ncpus = par_cores)
        res$bootMer <- apply(bb$t, 2, \(x) quantile(x, c(0.5, levs))) |>
            t() |>
            as.data.frame() |>
            setNames(c("fit", "lwr", "upr"))
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
    
    rr <- bind_rows(res, .id = "method") |>
        group_by(method) |>
        mutate(rank = seq_len(n()))
    gg0 <- ggplot(rr, aes(rank)) +
        geom_ribbon(aes(ymin = lwr, ymax = upr, fill = method),
                    alpha = 0.5)
    if ("fit" %in% names(rr)) {
        gg0 <- gg0 + geom_line(aes(y = fit, colour = method))
    }
    return(gg0)
}
