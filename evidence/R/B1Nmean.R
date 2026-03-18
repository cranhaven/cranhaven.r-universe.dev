B1Nmean <- function(x, plotit = TRUE, hists = FALSE, pdf = FALSE) {
    cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("      Bayesian analysis of a sample from a Normal distribution\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    if (is.null(x)) 
        stop("Data are essential!")
    dataname <- deparse(substitute(x))
    if (any(is.na(x))) {
        x <- x[!is.na(x)]
        cat("removing NAs from input data \n")
        x <- x[!is.na(x)]
    }
    n <- length(x)
    x.rng <- range(x)
    xdf <- data.frame(dataname = x)
    rng <- range(x)
    obj <- stan_glm(dataname ~ 1, family = gaussian, 
        data = xdf, prior = student_t(df = 3, location = 0), 
        prior_intercept = student_t(df = 3, location = 0), 
        adapt_delta = 0.99)
    Res <- obj$residuals
    SSQdata <- SSQ(Res)
    if (plotit) {
        print(pp_check(obj, nreps = 100))
    }
    par(ask = TRUE)
    if (plotit) 
        print(plot(obj))
    if(hists) {
    objnew <- stan_glm(dataname ~ 1, family = gaussian, 
        data = xdf, prior = student_t(df = 3, location = 0), 
        prior_intercept = student_t(df = 3, location = 0), 
        adapt_delta = 0.99)
    cat("Summary of model fitted for these data:\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n")
    print(summary(obj, digits = 3))
    print(posterior_interval(obj))
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n")
    cat("Summary of model fitted for NEW predicted data:\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    print(summary(objnew, digits = 3))
    print(posterior_interval(objnew))
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n")
    Ppredmat <- posterior_predict(obj, draws = 1000)
    PPnewmat <- posterior_predict(objnew, draws = 1000)
    nr <- nrow(Ppredmat)
    nc <- ncol(Ppredmat)
    RRmat <- matrix(NA, nr, nc)
    RRmatnew <- matrix(NA, nr, nc)
    for (i in 1:nr) {
        RRmat[i, ] <- (Ppredmat[i, ] - x)  # residuals
        RRmatnew[i, ] <- (PPnewmat[i, ] - x)  # new residuals
    }
    Ofit <- rowMeans(RRmat)
    Ofit.new <- rowMeans(RRmatnew)
    xH <- quantile(Ofit, 0.975)
    xL <- quantile(Ofit, 0.025)
    yH <- quantile(Ofit.new, 0.9)
    yL <- quantile(Ofit.new, 0.1)
    if (plotit || pdf) {
        
        ## look at predictions)
        par(mai = c(0.3, 0.3, 0.3, 0.3))
        opar <- par(mfrow = c(5, 4))  ### OR C(1,1,1,1) ????
        jc.repl <- posterior_predict(objnew, draws = 20)
        p.rng <- quantile(jc.repl[, (1:n)], 10, probs = c(0.05, 
            0.95))
        rng <- range(c(x, jc.repl[, (1:n)]))
        histbrks <- pretty(rng, 12)
        for (i in 1:20) {
            hist(jc.repl[i, (1:n)], main = "", xlab = "", 
                ylab = "", breaks = histbrks, xlim = x.rng)
        }
        
        par(opar)
        par(mai = c(0.8, 0.8, 0.8, 0.8))
        ## Overplot original data
        par(new = TRUE)
        hist(x, col = rgb(0, 100, 0, 80, maxColorValue = 255), 
            main = "", xlab = "", ylab = "", breaks = histbrks, 
            axes = FALSE, ann = FALSE)
        axis(1, labels = FALSE, col = rgb(0, 100, 0, 
            80, maxColorValue = 255))
        at1 <- axTicks(1)
        mtext(side = 1, text = at1, at = at1, col = rgb(0, 
            100, 0, 80, maxColorValue = 255), line = 1)
        mtext(side = 1, text = "X", line = 3, col = rgb(0, 
            100, 0, 80, maxColorValue = 255))
        at2 <- axTicks(2)
        mtext(side = 2, text = at2, at = at2, col = rgb(0, 
            100, 0, 80, maxColorValue = 255), line = 1)
        mtext(side = 2, text = "Freq.", line = 3, col = rgb(0, 
            100, 0, 80, maxColorValue = 255))
        par <- opar
        if (pdf) {
            dev.copy2pdf(file = "B1NmeanOUT.pdf", version = 1.4)
            cat("Graphics output saved in file B1NmeanOUT.pdf\n")
        }
    }
    }
    par(new = FALSE)
    par(ask = FALSE)
    if(hists) par <- opar
}
SSQ <- function(x) {
    sum(x %*% x)
}
