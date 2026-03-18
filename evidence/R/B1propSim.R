B1propSim <- function(p, N = 100, prior = c("uniform", "near_0.5",
                                            "not_near_0.5", "near_0",
                                            "near_1"))
{
    if(p <= 0 || p >= 1) {
        p <- runif(1, 0, 1)
        }
    a <- NA
    b <- NA
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
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("| Bayesian one sample proportion simulation|\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("Demo of Bayesian analyses of sequentially larger samples of the same population.\n")
    cat("Prior:", prior, " ( a =", a, ", b =", b, ")\n")
    cat("Hypothesized p:", p, "\n")
    y.lim <- N^(0.5)
    x <- seq(0, 1, length.out = 1000)
    success <- 0  # inititialize
    curve(dbeta(x, shape1 = a, shape2 = b), from = 0, 
        to = 1, ylim = c(0, y.lim), xlab = "p", ylab = "Posterior Density", 
        lty = 2, yaxt = "n")  # plot prior
    legend("topright", legend = c("Prior", "Updated Posteriors", 
        "Final Posterior"), lty = c(2, 1, 1), col = c("black", 
        "black", "red"))
    for (i in 1:N) {
        if (runif(1, 0, 1) <= p) 
            success <- success + 1
        curve(dbeta(x = x, shape1 = success + a, shape2 = i - success + b),
              from = 0, to = 1, add = TRUE)
        cat(paste(success, "successes and ", i - 
            success, " failures"), "\n")
    }
    curve(dbeta(x = x, shape1 = success + a, shape2 = (N - 
                success) + b), add = TRUE, col = "red", lwd = 1.5)
    CI <- HPDcrd(rbeta(1000, shape1 = success + a, shape2 = N - success + b))
    post <- median(rbeta(1000, shape1 = success + a, shape2 = N - success + b))
    segments(CI[1], 0, CI[2], 0, lwd = 5, col = "skyblue")
    cat("Posterior probability =", round(post, 3), "\n")
    cat("with a 95% cred. int. of", round(CI, 3), "\n")
}
