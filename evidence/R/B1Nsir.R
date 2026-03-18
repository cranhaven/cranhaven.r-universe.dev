B1Nsir <- function(x, r = 10000, alpha = 0.05) {
    crd <- 100 * (1 - alpha)
    cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("      Bayesian analysis of one Normal sample with SIR-priors\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    n <- length(x)
    mn <- mean(x)
    s2 <- var(x)
    SS <- sum((x - mn)^2)
    sigma2 <- SS/rchisq(r, n - 1)
    sigma <- sqrt(sigma2)
    mu <- rnorm(r, mn, sqrt(sigma2/n))
    cat("sample mean:", round(mn, 3), "; sample sd:", 
        round(sqrt(s2), 3), "\n")
    plot(density(mu), type = "l", xlab = deparse(substitute(x)), 
        ylab = "", yaxt = "n", main = "Posterior density")
    md.mu <- median(mu)
    md.sigma <- median(sigma)
    ci.mu <- HPDcrd(mu, alpha)
    ci.sigma <- HPDcrd(sigma, alpha)
    cat("Post. mean:", round(md.mu, 3), "\n", crd, 
        "% cred. int.:", round(ci.mu, 3), "\n")
    cat("Post. std. dev.:", round(md.sigma, 3), "\n")
    arrows(ci.mu[1], 0, ci.mu[2], 0, angle = 90, code = 3, 
        col = "blue", lwd = 0.5)
}
