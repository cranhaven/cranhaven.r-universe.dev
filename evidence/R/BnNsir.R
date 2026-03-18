BnNsir <- function(formula, data, var.equal = TRUE, 
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
    crbt <- 100 * (1 - alpha)
    cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("             Bayesian comparison of n Normal means\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    if (!is.data.frame(data)) 
        stop("not a data.frame")
    m <- model.frame(formula, data)
    X <- model.matrix(formula, data)
    Y <- model.response(m)
    rngY <- range(Y)
    resp.name <- as.character(formula[[2]])
    pred.name <- as.character(formula[[3]])
    k <- m[, 2]
    kn <- as.numeric(k)
    n.all <- length(Y)
    if (!is.factor(k)) 
        stop("predictor must be a factor")
    lev <- length(levels(k))
    n <- numeric(lev)
    kk <- 1:lev
    p <- lev
    nm <- character(p)
    for (j in 1:p) nm[j] <- as.character(levels(k)[j])
    cat("You have", n.all, "observations;\n")
    cat("the predictor has", p, "levels:", "\n", nm, 
        "\n")
    cS <- colSums(X)
    for (j in 2:p) {
        n[j] <- cS[j]
    }
    n[1] <- n.all - sum(cS[-1])
    nmx <- max(n)
    df.w <- sum(table(k) - 1)
    df.name <- deparse(substitute(dataf))
    Yrange <- range(Y)
    if (var.equal) 
        oneorall <- "pooled variances case" else oneorall <- "separate variances case"
    mn <- numeric(p)
    s2 <- numeric(p)
    s <- numeric(p)
    SS <- numeric(p)
    mx <- numeric(p)
    md.sigma <- numeric(p)
    mu <- matrix(0, r, p)
    mu.p <- matrix(0, r, p)
    md.mu <- numeric(p)
    sigma <- matrix(0, r, p)
    sigma2 <- matrix(0, r, p)
    sigma2.p <- matrix(0, r, p)
    sigma2v <- matrix(0, r, p)
    sigmav <- matrix(0, r, p)
    sigmav.p <- numeric(r)
    diff.mns <- numeric(p)
    diff.p <- numeric(p)
    md.diff <- numeric(p)
    diff <- matrix(0, r, p)
    ci.diff <- matrix(0, 2, p)
    ci.diff.p <- matrix(0, 2, p)
    ci.sigma.p <- numeric(2)
    ci.sigma <- matrix(0, 2, p)
    Z <- list()
    q <- Y[kn == 1]
    Z[[1]] <- q[q != 0]
    for (j in 2:p) {
        q <- Y[kn == j]
        Z[[j]] <- q[q != 0]
    }
    for (j in 1:p) {
        mn[j] <- mean(Z[[j]])
    }
    gmn <- mean(Y)
    gs2 <- var(Y)
    gs <- sqrt(gs2)
    cat("Group 1", "(", nm[1], ") has", n[1], "observations\n")
    s[1] <- sd(Z[[1]])
    SS[1] <- sum((Z[[1]] - mn[1])^2)
    for (j in 2:p) {
        cat("Group", j, "(", nm[j], ") has", n[j], 
            "observations\n")
        s[j] <- sd(Z[[j]])
        SS[j] <- sum((Z[[j]] - mn[j])^2)
    }
    cat("You have specified the", oneorall, "\n")
    if (plotit) {
        opar <- par(mfrow = c(2, 2))
        for (j in 2:p) {
            if (p >= 2) 
                par(ask = TRUE)
            v <- c(Z[[1]], Z[[j]])
            extend <- range(v)
            ind <- c(rep(nm[1], n[1]), rep(nm[j], n[j]))
            boxplot(v ~ ind, horizontal = TRUE, col = "lightgreen", 
                names = c(nm[1], nm[j]), xlab = resp.name, 
                sub = paste(nm[1], "vs.", nm[j]))
            stripchart(v ~ ind, method = "stack", offset = 1/3, 
                pch = 1, add = TRUE, col = "brown", 
                xlim = extend)
            title("Boxplot and stripchart")
            x1 <- Z[[1]]
            x2 <- Z[[j]]
            qq(x1, x2, xlbl = nm[1], ylbl = nm[j])
            title("qq-plot")
        }
    }
    ############################################################## 
    if (var.equal) {
        SSp <- (n.all - 1) * var(Y)
        df.p <- sum(n) - p
        sigma2p <- SSp/rchisq(r, df.p)
        sigmav.p <- sqrt(sigma2p)
        ci.sigma.p <- quantile(sigmav.p, c(alpha/2, 
            1 - alpha/2))
        mu[, 1] <- rnorm(r, mn[1], sigmav.p/sqrt(df.p))
        md.mu[1] <- median(mu[, 1])
        for (j in 2:p) {
            mu[, j] <- rnorm(r, mn[j], sigmav.p/sqrt(df.p))
            md.mu[j] <- median(mu[, j])
            diff[, j] <- mu[, j] - mu[, 1]
            md.diff[j] <- median(diff[, j])
            ci.diff[, j] <- quantile(diff[, j], c(alpha/2, 
                1 - alpha/2))
            ci.sigma.p <- quantile(sigmav.p, c(alpha/2, 
                1 - alpha/2))
        }
        rng.diff <- range(diff)
    } else {
        for (j in 1:p) {
            sigma2v[, j] <- SS[j]/rchisq(r, n[j] - 
                1)
            sigmav[, j] <- sqrt(sigma2v[, j])
            sigma[j] <- median(sigmav[, j])
            ci.sigma[, j] <- quantile(sigmav[, j], 
                c(0.025, 0.975))
            mu[, j] <- rnorm(r, mn[j], sigmav[, j]/sqrt(n[j]))
            md.mu[j] <- median(mu[, j])
            diff[, j] <- mu[, j] - mu[, 1]
            md.diff[j] <- median(diff[, j])
            ci.diff[, j] <- quantile(diff[, j], c(0.025, 
                0.975))
        }
        rng.diff <- range(diff)
    }
    ############################################################## 
    if (var.equal) {
        cat("Parametric mean estimates:\n", round(md.mu, 
            3), "\n")
        cat("alpha[i]:\n", round(md.diff, 3), "\n")
    } else {
        cat("Parametric mean estimates:\n", md.mu, 
            "\n")
        cat("alpha[i]:\n", md.diff, "\n")
    }
    mu.rng <- range(mu)
    for (j in 1:p) {
        cat("sample mean", j, ":", round(mn[j], 3), 
            "; sample sd", j, ":", round(sqrt(s[j]), 
                3), "\n")
        cat("estimated par. mean:", round(md.mu[j], 
            3), "\n")
        cat("diff. between sample means:", round(mn[1] - 
            mn[j], 3), "\n")
    }
    if (var.equal == TRUE) {
        if (plotit) {
            plot(density(mu[, 1]), type = "l", main = "", 
                sub = oneorall, xlim = mu.rng, xlab = resp.name)
            title("Posterior densities of means")
            for (j in 2:p) {
                lines(density(mu[, j]), col = j, lty = j)
            }
            legend("topright", legend = nm[1:p], lty = 1:p, 
                col = 1:p)
            if (p >= 2) {
                for (jj in 2:p) {
                  par(ask = TRUE)
                  plot(density(diff[, jj]), main = "", 
                    col = jj, lty = jj, sub = oneorall, 
                    xlim = rng.diff, xlab = resp.name)
                  arrows(ci.diff[1, jj], 0, ci.diff[2, 
                    jj], 0, angle = 90, code = 3, col = jj, 
                    lty = jj, lwd = 0.5)
                  mu1 <- expression(mu[1])
                  jje <- deparse(substitute(jj))
                  mujj <- expression(mu[jje])
                  title(paste("Post. dens. of diff. mean 1 and", 
                    jj))
                }
            }
        }
        par(ask = FALSE)
        cat("- - - - - - - - - - - - - - - - - - - - - - - - - - - \n")
        for (j in 1:p) {
            cat("diff. mu[", j, "] - mu[1]:", round(md.diff[j], 
                3), "\n", crbt, "% cred. int.:", round(ci.diff[, 
                j], 3), "\n")
            cat("pooled sigma:", round(median(sigmav.p), 
                3), "\nwith", crbt, "% cred. int.:", 
                round(ci.sigma.p, 3), "\n")
            cat("- - - - - - - - - - - - - - - - - - - - - - - - - - - \n")
        }
    } else {
        if (plotit) {
            plot(density(mu[, 2]), type = "l", main = "", 
                sub = oneorall, xlim = mu.rng, xlab = resp.name)
            title("Posterior densities of means")
            for (j in 3:p) {
                lines(density(mu[, j]), col = j, lty = j)
            }
            legend("topright", legend = nm[1:p], lty = 1:p, 
                col = 1:p)
            if (p >= 2) {
                for (jj in 2:p) {
                  par(ask = TRUE)
                  plot(density(diff[, jj]), main = "", 
                    col = jj, lty = jj, sub = oneorall, 
                    xlim = rng.diff, xlab = resp.name)
                  arrows(ci.diff[1, jj], 0, ci.diff[2, 
                    jj], 0, angle = 90, code = 3, col = jj, 
                    lty = jj, lwd = 0.5)
                  title("Post. density of diff. between means")
                  plot(density(diff[, j]), main = "", 
                    sub = oneorall, xlim = rng.diff, 
                    xlab = resp.name)
                  title("Post. density of diff. between means")
                  lines(density(diff[, j]), col = j, 
                    lty = j)
                  arrows(ci.diff[1, j], 0, ci.diff[2, 
                    j], 0, angle = 90, code = 3, col = j, 
                    lty = j, lwd = 0.5)
                }
            }
        }
        par(ask = FALSE)
        cat("- - - - - - - - - - - - - - - - - - - - - - - - - - - \n")
        for (j in 1:p) {
            cat("diff. mu[", j, "] - mu[1]:", round(md.diff[j], 
                3), "\n", crbt, "% cred. int.:", round(ci.diff[, 
                j], 3), "\n")
            cat("sigma:", round(median(sigmav[, j]), 
                3), "\nwith", crbt, "% cred. int.:", 
                round(ci.sigma[, j], 3), "\n")
            cat("- - - - - - - - - - - - - - - - - - - - - - - - - - - \n")
        }
    }
    par(opar)
}
