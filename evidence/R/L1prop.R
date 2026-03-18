L1prop <- function(x, n, p.hypoth, pLset = 0.05) {
    L <- function(p, x, n) {
        p^x * (1 - p)^(n - x)
    }
    lL <- function(p, x, n) {
        x * log(p) + (n - x) * log(1 - p)
    }
    nL <- function(p, x, n) {
        (p^x * (1 - p)^(n - x))/((x/n)^x * (1 - (x/n))^(n - 
            x))
    }
    
    # Likelihood comparison of observed count x out of
    # n, to a hypothesized proportion p.hypoth
    
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("            Likelihood analysis of one sample Binomial data\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    p <- x/n
    pies <- seq(0, 1, 0.001)
    nLpies <- nL(pies, x, n)
    inint <- pies[nLpies > pLset]
    low <- min(inint)
    high <- max(inint)
    Lratio <- L(p.hypoth, x, n)/L(x/n, x, n)
    pval <- 1 - pchisq(-2 * log(Lratio), 1)
    cat("Likelihood ratio for hypothesized prob.:", 
        round(Lratio, 3), "\n", "with approximate p-value for these data:", 
        round(pval, 3), "\n")
    plot(pies, nLpies, type = "l", ylab = "Normed likelihood", 
        xlab = expression(hat(pi)))
    segments(low, 0, high, 0, lwd = 5)
    title(paste("Normed likelihood and", deparse(substitute(pLset)), 
        "% likelihood interval"))
    abline(v = p.hypoth, col = "red")
    cat("MLE of p for these data:", round(x/n, 3), 
        "\n")
    cat(pLset, "% likelihood interval: [", low, ",", 
        high, "]\n")
}
