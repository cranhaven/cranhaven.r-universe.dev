B1prop <- function(s, n, p = 0.5, alpha = 0.05, prior = c("uniform", 
    "near_0.5", "not_near_0.5", "near_0", "near_1", 
    "custom"), params = NULL) {
    cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("             Bayesian analysis of one binomial proportion\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    if (alpha < 0 || alpha >= 1) 
        stop("alpha must be between 0 and 1\n")
    if (s >= n) 
        stop("s must be smaller than n")
    crd <- 100 * (1 - alpha)
    a <- NA
    b <- NA
    if (is.null(prior)) {
        prior <- "uniform"
    }
    if (prior == "uniform") {
        a <- 1
        b <- 1
    }
    if (prior == "near_0.5") {
        a <- 2
        b <- 2
    }
    if (prior == "not_near_0.5") {
        a <- 0.5
        b <- 0.5
    }
    if (prior == "near_0") {
        a <- 0.5
        b <- 2
    }
    if (prior == "near_1") {
        a <- 2
        b <- 0.5
    }
    if (prior == "custom") {
        a <- params[1]
        b <- params[2]
    }
    cat("Prior:", prior, " ( a =", a, ", b =", b, ")\n")
    cat("Bayesian one sample proportion\n\n")
    cat("Hypothesized p:", p, "\n")
    prop <- s/n
    A <- a + s
    B <- b + n - s
    prior.mode <- (a - 1)/(a + b - 2)
    post.mode <- (A - 1)/(A + B - 2)
    prior.max <- dbeta(prior.mode, a, b)
    post.max <- dbeta(post.mode, A, B)
    F <- function(x) dbeta(x, shape1 = a + s, shape2 = b + 
        n - s)
    post.scaled <- pbeta(seq(0, 1, 0.01), shape1 = a + 
        s, shape2 = b + n - s)
    cat("Prior: mode =", round(prior.mode, 4), "\n")
    cat("Posterior: mode =", round(post.mode, 4), "\n")
    fmax <- max(prior.mode, post.mode)
    s.f1 <- 1/prior.max
    s.f2 <- 1/post.max
    post.pH0 <- pbeta(p, shape1 = A, shape2 = B)
    cat("Posterior prob. for p >", p, ":", round(post.pH0, 
        4), "\n")
    cat(crd, "% Cred. Int.:\n")
    CI <- HPDcrd(qbeta(seq(0, 1, 0.01), shape1 = a + 
        s, shape2 = b + n - s))
    CI1 <- CI[1]
    CI2 <- CI[2]
    cat(round(CI, 4), "\n")
    plot(seq(0, 1, 0.01), dbeta(seq(0, 1, 0.01), shape1 = a, 
        shape2 = b) * s.f1, type = "l", lty = 3, axes = FALSE, 
        , xlab = "p", ylab = "", ylim = c(0, 1))
    axis(1, seq(0, 1, 0.1), tick = TRUE, xlab = "p", 
        ylim = c(0, 1), xlab = "P")
    lines(seq(0, 1, 0.01), s.f2 * dbeta(seq(0, 1, 0.01), 
        shape1 = a + s, shape2 = b + n - s))
    polygon(x = c(seq(0, CI1, 0.01), rev(seq(0, CI1, 
        0.01))), y = s.f2 * c(dbeta(x = seq(0, CI1, 
        0.01), shape1 = a + s, shape2 = b + n - s), 
        rep(0, times = length(seq(0, CI1, 0.01)))), 
        col = "skyblue", border = FALSE)
    polygon(x = c(seq(CI2, 1, 0.01), rev(seq(CI2, 1, 
        0.01))), y = s.f2 * c(dbeta(x = c(seq(CI2, 
        1, 0.01)), shape1 = a + s, shape2 = b + n - 
        s), rep(0, times = length(seq(CI2, 1, 0.01)))), 
        col = "skyblue", border = FALSE)
    title("Binomial single sample")
    segments(p, 0, p, 1, col = "blue", lty = 2)
    invisible(dbeta(seq(0, 1, 0.01), shape1 = a + s, 
        shape2 = b + n - s))
}
