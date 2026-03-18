L2prop <- function(x, n) {
    
    L.psi <- function(psi, x, n) {
        psi^n * (1 - psi)^(n - x)
    }
    
    R <- function(pi, x, n) {
        (pi/(x/n))^x * ((1 - pi)/((n - x)/n))^(n - 
            x)
    }
    
    x1 <- x[1]
    x2 <- x[2]
    n1 <- n[1]
    n2 <- n[2]
    p1 <- x1/n1
    p2 <- x2/n2
    x <- x1 + x2
    n <- n1 + n2
    p <- x/n
    pies <- seq(0, 1, 0.001)
    plot(pies, R(pies, x1, n1), type = "l", ylab = "Normed likelihood", 
        xlab = expression(pi), col = "skyblue")
    lines(pies, R(pies, x2, n2), col = "red")
    abline(v = p1, col = "skyblue", lty = 3)
    abline(v = p2, col = "red", lty = 3)
    
    cat("MLE of p1:", round(x1/n1, 3), "\n")
    cat("MLE of p2:", round(x2/n2, 3), "\n")
    lL <- x1 * log(p1) + (n1 - x1) * log(1 - p1) + 
        x2 * log(p2) + (n2 - x2) * log(1 - p2)
    aic <- -2 * lL + 2 * 2
    lL.H0 <- x * log(p) + (n - x) * log(1 - p)
    aicH0 <- -2 * lL + 2
    cat("AIC:\n Two parameter model:", round(aic, 3), 
        "\n", "One parameter model:", round(aicH0, 
            3), "\n")
    LR <- -2 * (x * log(p) + (n - x) * log(1 - p) - 
        (x1 * log(p1) + (n1 - x1) * log(1 - p1) + x2 * 
            log(p2) + (n2 - x2) * log(1 - p2)))
    pvalue <- pchisq(LR, 1, lower.tail = FALSE)
    
    cat("Likelihood ratio:", round(LR, 3), "\n")
    cat("(approximate) p-value =", round(pvalue, 3), 
        "\n")
    
    LR <- R(p, x, n)/R(x/n, x, n)
    D <- -2 * (log(R(p, x, n)) - log(R(x/n, x, n)))
    
    cat("Likelihood ratio p / MLE:", round(LR, 3), 
        "\n")
    cat("Deviance p vs. MLE:", round(D, 3), "\n")
    if (D > 10) 
        cat("so very strong evidence against p =", 
            p, "\n")
    if (D > 6 & D < 10) 
        cat("so strong evidence against p =", p, "\n")
    if (D > 2 & D < 10) 
        cat("so positive evidence against p =", p, 
            "\n")
    if (D < 2) 
        cat("so little evidence against p =", p, "\n")
}
