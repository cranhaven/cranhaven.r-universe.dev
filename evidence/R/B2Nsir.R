B2Nsir <- function(formula, data, var.equal = TRUE, 
    alpha = 0.05, plotit = TRUE, r = 10000) {
    qq <- function(x, y, xlbl = "x", ylbl = "y") {
        rng <- range(c(x, y))
        n <- length(x)
        m <- length(y)
        qqplot(x, y, xlim = rng, ylim = rng, xlab = xlbl, 
            ylab = ylbl)
        abline(0, 1, col = "grey")
        abline(v = mean(x), col = "light blue")
        abline(h = mean(y), col = "light blue")
    }
    crd <- 100 * (1 - alpha)
    cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("             Bayesian comparison of two Normal means\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    if (!is.data.frame(data)) 
        stop("not a data.frame")
    m <- model.frame(formula, data)
    x <- model.matrix(formula, data)
    y <- model.response(m)
    resp.name <- as.character(formula[[2]])
    Y <- resp.name
    pred.name <- as.character(formula[[3]])
    k <- m[, 2]
    n <- length(Y)
    if (!is.factor(k)) 
        stop("predictor must be a factor")
    lev <- length(levels(k))
    kk <- 1:lev
    p <- length(levels(k))
    df.w <- sum(table(k) - 1)
    df.name <- deparse(substitute(dataf))
    Yrange <- range(y)
    dat <- list(Y = Y, k = kk)
    x1 <- y[x[, 2] == 0]
    x2 <- y[x[, 2] == 1]
    if (var.equal) 
        oneor2 <- "Pooled variances case" else oneor2 <- "Separate variances case"
    n1 <- length(x1)
    n2 <- length(x2)
    mn1 <- mean(x1)
    mn2 <- mean(x2)
    v <- c(x1, x2)
    ind <- factor(c(rep(0, n1), rep(1, n2)))
    nm1 <- levels(m[, 2])[1]
    nm2 <- levels(m[, 2])[2]
    cat("Group 1 (", nm1, ") has", n1, "observations\n")
    cat("Group 2 (", nm2, ") has", n2, "observations\n")
    cat("You have specified the", oneor2, "\n")
    s21 <- sd(x1)
    s22 <- sd(x2)
    SS1 <- sum((x1 - mn1)^2)
    SS2 <- sum((x2 - mn2)^2)
    SSp <- sum(c((x1 - mn1)^2, (x2 - mn2)^2))
    df.p <- n1 + n2 - 2
    if (plotit) {
        opar <- par(mfrow = c(2, 2))
        extend <- range(y)
        boxplot(v ~ ind, horizontal = TRUE, col = "lightgreen", 
            names = c(nm1, nm2), xlab = resp.name)
        stripchart(v ~ ind, method = "stack", offset = 1/3, 
            pch = 1, add = TRUE, col = "brown", xlim = extend)
        title("Boxplot and stripchart")
        if (n1 == n2) {
            qq(x1, x2, xlbl = nm1, ylbl = nm2)
            title("qq-plot")
        }
    }
    
    if (var.equal == TRUE) {
        SSp <- sum(c((x1 - mn1)^2, (x2 - mn2)^2))
        df.p <- n1 + n2 - 2
        s.p <- sqrt(SSp/df.p)
    }
    sigma21 <- SS1/rchisq(r, n1 - 1)
    sigma22 <- SS2/rchisq(r, n2 - 1)
    sigma2.p <- SSp/rchisq(r, df.p)
    sigmapv <- sqrt(sigma2.p)
    sigma1v <- sqrt(sigma21)
    sigma2v <- sqrt(sigma22)
    sigmap <- sqrt(mean(sigma2.p))
    sigma1 <- sqrt(mean(sigma21))
    sigma2 <- sqrt(mean(sigma22))
    mu1 <- rnorm(r, mn1, sqrt(sigma21/n1))
    mu2 <- rnorm(r, mn2, sqrt(sigma22/n2))
    mu.rng <- range(c(mu1, mu2))
    mu1.p <- rnorm(r, mn1, sqrt(sigma2.p/df.p))
    mu2.p <- rnorm(r, mn2, sqrt(sigma2.p/df.p))
    cat("sample mean 1:", round(mn1, 3), "; sample sd 1:", 
        round(sqrt(s21), 3), "\n")
    cat("sample mean 2:", round(mn2, 3), "; sample sd 2:", 
        round(sqrt(s22), 3), "\n")
    cat("diff. sample means:", round(mn1 - mn2, 3), 
        "\n")
    md.mu1 <- median(mu1)
    md.mu2 <- median(mu2)
    md.mu1.p <- median(mu1.p)
    md.mu2.p <- median(mu2.p)
    md.sigma1 <- median(sigma1)
    md.sigma2 <- median(sigma2)
    diff.p <- mu1.p - mu2.p
    diff <- mu1 - mu2
    mu.diff.p <- median(diff.p)
    mu.diff <- median(diff)
    ci.diff <- HPDcrd(diff)
    ci.sigmap <- HPDcrd(sigmapv)
    ci.sigma1 <- HPDcrd(sigma1v)
    ci.sigma2 <- HPDcrd(sigma2v)
    ci.diff.p <- HPDcrd(diff.p)
    ci.diff <- HPDcrd(diff)
    if (var.equal == TRUE) {
        if (plotit) {
            plot(density(mu1.p), type = "l", main = "", 
                sub = oneor2, xlim = mu.rng, xlab = resp.name, 
                yaxt = "n")
            title("Posterior densities of means")
            lines(density(mu2.p), col = "red", lty = 3)
            legend("topright", legend = c(nm1, nm2), 
                lty = c(1, 3), col = c("black", "red"))
            plot(density(diff.p), main = "", sub = oneor2, 
                xlab = resp.name)
            title("Post. density of diff. between means")
            arrows(ci.diff.p[1], 0, ci.diff.p[2], 0, 
                angle = 90, code = 3, col = "blue", 
                lwd = 0.5)
        }
        cat("mu1 =", round(md.mu1.p, 3), "; mu2 =", 
            round(md.mu2.p, 3), "\n")
        cat("diff. mu1 - mu2:", round(mu.diff.p, 3), 
            "\n", crd, "cred. int.:", round(ci.diff.p, 
                3), "\n")
        cat("pooled sigma:", round(mean(sigmap), 3), 
            "\nwith ", crd, "% cred. int.:", round(ci.sigmap, 
                3), "\n")
    } else {
        if (plotit) {
            plot(density(mu1), type = "l", main = "", 
                sub = oneor2, xlim = mu.rng, xlab = resp.name, 
                yaxt = "n")
            title("Posterior densities of means")
            lines(density(mu2), col = "red", lty = 3)
            legend("topright", legend = c(nm1, nm2), 
                lty = c(1, 3), col = c("black", "red"))
            plot(density(diff), main = "", sub = oneor2, 
                xlab = resp.name)
            title("Post. density of diff. between means")
            arrows(ci.diff[1], 0, ci.diff[2], 0, angle = 90, 
                code = 3, col = "blue", lwd = 0.5)
        }
        diff.mns <- mu2 - mu1
        cat("mu1 =", round(md.mu1, 3), "; mu2 =", round(md.mu2, 
            3), "\n")
        cat("diff. mu1 - mu2:", round(mu.diff, 3), 
            "\n", crd, "% cred. int.:", round(ci.diff, 
                3), "\n")
        cat("sigma1:", round(sigma1, 3), "\nwith ", 
            crd, "% cred. int.:", round(ci.sigma1, 
                3), "\n")
        cat("sigma2:", round(sigma2, 3), "\nwith ", 
            crd, "% cred. int.:", round(ci.sigma2, 
                3), "\n")
    }
    par(opar)
}
