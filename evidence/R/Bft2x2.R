Bft2x2 <- function(X, div = 100, plotit = TRUE) {
    ## utility functions
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
    
    cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("       Bft2x2: Bayesian analysis of a 2 x 2 contingency table \n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    
    if (dim(X)[1] != 2 & dim(X)[2] != 2) 
        stop("X must be a 2 x 2 matrix")
    if (is.null(dimnames(X))) 
        dimnames(X) <- list(outcome = c("I", "II"), 
            sample = c("1", "2"))
    print(X)
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
    
    ## odds ratio
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
    ## model comparison using BF
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
    
    ## calculate posterior densities
    Post <- expand.grid(pAp = pgrid, pBp = pgrid)
    PstP <- post(pAp = Post$pAp, pBp = Post$pBp)
    Post$P <- PstP/max(PstP)
    Pmat <- matrix(Post$P, div, div)
    
    if (plotit) {
        pstA <- postA(pgrid)
        pstA <- pstA/max(pstA)
        pstB <- postB(pgrid)
        pstB <- pstB/max(pstB)
        ## plot posteriors for the two treatments
        plot(pgrid, pstA, xlab = "Proportion", ylab = paste("Post.Prob.", 
             F1, resNames[1]), type = "l", lwd = 3)
        title("Conditional posterior probabilities")
        lines(pgrid, pstB, lty = 3, lwd = 3)
        legend("topright", paste(F2, trtmNames), lty = c(1, 
            3))
        par(ask = TRUE)
        ## plot posteriors in 3D
        print(wireframe(P ~ pAp * pBp, Post, xlab = paste("Prob.", 
                        F11), ylab = paste("Prob.", F12)))
        title("Joint posterior probabilities")
        par(ask = TRUE)
        ## plot posteriors in contour contour(Pmat, xlab =
        ## expression(p[A]), ylab = expression(p[B]))
        contour(Pmat, xlab = paste("Prob.", F11), ylab = paste("Prob.", 
                                                                F12))
        abline(0, 1, lty = 3)
        title("Joint posterior probabilities")
    }
    u <- sum(Post$P[Post$pAp >= Post$pBp])
    l <- sum(Post$P[Post$pAp < Post$pBp])
    a <- u + l
    upper <- u/a
    lower <- l/a
    cat(paste("P(", F11, "<", F12), ") =", round(lower, 
        3), "\n")
    cat(paste("P(", F11, ">", F12), ") =", round(upper, 
        3), "\n")
    cat(paste("LR", F11, "/", F12, "="), round((1 - 
        lower)/lower, 3), "\n")
    par(ask = FALSE)
    invisible(Post)
}
