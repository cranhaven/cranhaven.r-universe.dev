B2props <- function(s, n, alpha = 0.05) {
    Ls <- length(s)
    Ln <- length(n)
    if (Ls != 2 | Ln != 2) 
        stop("arguments S and N must be vectors with two elements")
    ldirich <- function(x) {
        sum(lgamma(x)) - lgamma(sum(x))
    }
    logL <- function(pAp, pBp) {
        X[1, 1] * log(pAp) + X[1, 2] * log(1 - pAp) + 
            X[1, 2] * log(pBp) + X[2, 2] * log(1 - 
            pBp)
    }
    postA <- function(pAp) {
        dbeta(pAp, shape1 = X[1, 1] + 1, shape2 = colTotals[1] + 
            1 - X[1, 1])
    }
    postB <- function(pBp) {
        dbeta(pBp, shape1 = X[1, 2] + 1, shape2 = colTotals[2] + 
            1 - X[1, 2])
    }
    post <- function(pAp, pBp) {
        postA(pAp) * postB(pBp)
    }
    ss <- s[1] + s[2]
    sn <- n[1] + n[2]
    p1 <- s[1]/n[1]
    p2 <- s[2]/n[2]
    s1 <- s[1]
    s2 <- s[2]
    n1 <- n[1]
    n2 <- n[2]
    p.pooled <- ss/sn
    cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("       B2prop: Bayesian analysis of two proportions \n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("\nProportion 1:", round(p1, 3))
    cat("\nProportion 2:", round(p2, 3))
    cat("\nProportion 1 and 2 pooled:", round(p.pooled, 
        3), "\n")
    cat("\nThe same data written in a contingency table:\n")
    X <- sn2ft2x2(s, n)
    print(X)
    div = 100
    plotit = TRUE
    nTotal <- sum(X)
    trtmNames <- colnames(X)
    resNames <- rownames(X)
    F1 <- names(dimnames(X))[1]
    F2 <- names(dimnames(X))[2]
    F11 <- paste(F1, ":", dimnames(X)[[1]][1], "|", 
        F2, ":", dimnames(X)[[2]][1])
    F12 <- paste(F1, ":", dimnames(X)[[1]][1], "|", 
        F2, ":", dimnames(X)[[2]][2])
    F21 <- paste(F1, ":", dimnames(X)[[1]][2], "|", 
        F2, ":", dimnames(X)[[2]][1])
    F22 <- paste(F1, ":", dimnames(X)[[1]][2], "|", 
        F2, ":", dimnames(X)[[2]][2])
    rowTotals <- rowSums(X)
    colTotals <- colSums(X)
    F <- prop.table(X, 2)
    dimnames(F) <- dimnames(X)
    cat("\nRelative frequencies:\n")
    print(round(F, 3))
    E <- matrix(NA, 2, 2)
    for (i in 1:2) {
        for (j in 1:2) {
            E[i, j] <- rowTotals[i] * colTotals[j]/sum(X)
        }
    }
    dimnames(E) <- dimnames(X)
    cat("\nExpected frequencies if no interaction:\n")
    print(E)
    pgrid <- seq(0.01, 1, length = div)
    cat("\nTotal number of cases was", nTotal, "\n")
    OR <- (X[1, 1] * X[2, 2])/(X[1, 2] * X[2, 1])
    cat("\nSample odds ratio =", round(OR, 3), "\n")
    m <- ((X[1, 1] + X[1, 2]) * (X[1, 1] + X[2, 1]))/sum(X)
    lL <- 0
    for (i in 1:2) {
        for (j in 1:2) {
            lL <- lL + X[i, j] * log(X[i, j]/E[i, j])
        }
    }
    G <- 2 * lL
    support <- G - 0.5 - 0.5 * log(G)
    cat("G =", round(G, 3), "\nsupport difference = G/2 =", 
        round(G/2, 3), "\nlogLik. (support) =", round(support, 
            3), "\n")
    if (!is.nan(support)) {
        if (support > 10) 
            cat("so very strong evidence for an effect\n")
        if (support > 6 && support < 10) 
            cat("so strong evidence for an effect\n")
        if (support > 2 && support < 6) 
            cat("so positive evidence for an effect\n")
        if (support < 2) 
            cat("so little evidence for an effect\n")
    }
    A <- matrix(rep(1, 4), 2, 2)
    rSA <- rowSums(A)
    cSA <- colSums(A)
    ones <- rep(1, 2)
    lBF <- ldirich(as.numeric(X) + as.numeric(A)) + 
        ldirich(rSA - ones) + ldirich(cSA - ones) - 
        ldirich(as.numeric(A)) - ldirich(rowTotals + 
        colSums(A) - ones) - ldirich(colTotals + colSums(A) - 
        ones)
    BF <- exp(lBF)
    cat("\nBayes factor against independence =", round(BF, 
        2), "\n")
    if (BF < 3) 
        cat("so neglible evidence\n")
    if (BF >= 3 & BF < 20) 
        cat("so positive evidence\n")
    if (BF >= 20 & BF < 150) 
        cat("so strong evidence\n")
    if (BF >= 150) 
        cat("so very strong evidence\n")
    Post <- expand.grid(pAp = pgrid, pBp = pgrid)
    PstP <- post(pAp = Post$pAp, pBp = Post$pBp)
    Post$P <- PstP/max(PstP)
    Pmat <- matrix(Post$P, div, div)
    if (plotit) {
        pstA <- postA(pgrid)
        pstA <- pstA/max(pstA)
        pstB <- postB(pgrid)
        pstB <- pstB/max(pstB)
        plot(pgrid, pstA, xlab = "Proportion", ylab = "Posterior density", 
            type = "l", lwd = 3)
        lines(pgrid, pstB, lty = 3, lwd = 3)
        legend("topright", c(expression(pi[1]), expression(pi[2])), 
            lty = c(1, 3))
        par(ask = TRUE)
        print(wireframe(P ~ pAp * pBp, Post, xlab = paste("prop.", 
            "1"), ylab = paste("prop.", "2")))
        ### xlab # 'p'[1] ylab # 'p'[2]
        par(ask = TRUE)
        contour(Pmat, xlab = expression(pi[1]), ylab = expression(pi[2]))
        abline(0, 1, lty = 3)
    }
    u <- sum(Post$P[Post$pAp >= Post$pBp])
    l <- sum(Post$P[Post$pAp < Post$pBp])
    a <- u + l
    upper <- u/a
    lower <- l/a
    cat("P(prop1 < prop2) =", round(lower, 3), "\n")
    cat("P(prop1 > prop2) =", round(upper, 3), "\n")
    sample.diff <- p1 - p2
    y1_sim <- rbeta(10000, s1, n1)
    y2_sim <- rbeta(10000, s2, n2)
    diff <- y1_sim - y2_sim
    CI_diff <- quantile(diff, c(alpha, 1 - alpha))
    diff <- y1_sim - y2_sim
    CI_diff <- quantile(diff, c(alpha, 1 - alpha))
    cred <- 100 * (1 - alpha)
    credchar <- as.character(cred)
    if (plotit) {
        plot(density(diff, bw = "SJ"), xlab = expression(pi[1] - 
            pi[2]), main = "Posterior density difference", 
            sub = paste("with", credchar, " perc. credible interval"))
        arrows(CI_diff[1], 0, CI_diff[2], 0, angle = 90, 
            code = 3, col = "blue", lwd = 0.5)
    }
    par(ask = FALSE)
}
