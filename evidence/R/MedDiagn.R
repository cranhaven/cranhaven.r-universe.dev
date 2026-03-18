MedDiagn <- function(x0, n0, x1, n1, x2, n2, N = 10000, 
    alpha = 0.05, pdf = FALSE) {
    # prevalence p0 = x0/n0 sensitivity p1 = x1/n1 :
    # P[+test | +D] specificity p2 = x2/n2 : P[-test |
    # -D] output: p[D | +test] and 1-alpha cred. int.
    # RvH, 070104, see Mossman,D & Berger, JO. 2001.
    # Medical Decision Making Dec. 2001, p.  498-507.
    # = = = = = = = = = = = = = = = = = = = = = = =
    p0 <- x0/n0
    p1 <- x1/n1
    p2 <- x2/n2
    phi.hat <- (p0 * p1)/(p0 * p1 + (1 - p0) * (1 - 
        p2))
    cat("P(Disease | test +) = ", round(phi.hat, 3), 
        "\n")
    phi <- numeric(N)
    for (i in 1:N) {
        p0.r <- rbeta(1, x0 + 0.5, n0 - x0 + 0.5)
        p1.r <- rbeta(1, x1 + 0.5, n1 - x1 + 0.5)
        p2.r <- rbeta(1, x2 + 0.5, n2 - x2 + 0.5)
        phi[i] <- (p0.r * p1.r)/(p0.r * p1.r + (1 - 
            p0.r) * (1 - p2.r))
    }
    phi <- sort(phi)
    phi.max <- median(phi)
    q1 <- quantile(phi, alpha/2)
    q2 <- quantile(phi, 1 - alpha/2)
    q0 <- phi.max
    Conf <- 100 * (1 - alpha)
    if (!pdf) {
        plot(density(phi), main = paste("P(Disease | + test), with ", 
            deparse(substitute(Conf)), "% Credible Interval"), 
            yaxt = "n", xlab = "Probability", sub = "")
        abline(v = q0, col = "skyblue")
        segments(q1, 0, q2, 0, col = "gray", lwd = 10)
    }
    if (pdf) {
        postscript(file = "MedDiagn.ps", horizontal = FALSE, 
            paper = "special")
        plot(density(phi), main = paste("P(Disease|+ test), with ", 
            deparse(substitute(Conf)), "% Credible Interval"), 
            yaxt = "n", xlab = "Probability", sub = "")
        abline(v = q0, col = "skyblue")
        segments(q1, 0, q2, 0, col = "gray", lwd = 10)
        dev.off()
    }
    cat(paste(deparse(substitute(Conf)), "% cred. interval"), 
        round(q1, 4), round(q2, 4), "\n")
}
