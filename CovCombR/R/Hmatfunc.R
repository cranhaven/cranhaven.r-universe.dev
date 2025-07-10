Hmatfunc <- function(Amat, Gmat, w = 0.5, nu = 1000) {
    A11 <- Amat[rownames(Amat) %in% rownames(Gmat), colnames(Amat) %in% 
        colnames(Gmat)]
    G <- Gmat[match(rownames(A11), rownames(Gmat)), match(rownames(A11), 
        rownames(Gmat))]
    invA11 <- solve(A11)
    Gw <- (w) * G + (1 - w) * nu * A11
    A22 <- Amat[!(rownames(Amat) %in% rownames(A11)), !(colnames(Amat) %in% 
        colnames(A11))]
    A12 <- Amat[match(rownames(G), rownames(Amat)), !(colnames(Amat) %in% 
        colnames(A11))]
    Hmat11 <- Gw
    Hmat12 <- t(A12) %*% invA11 %*% Gw
    Hmat22 <- (nu) * A22 + t(A12) %*% invA11 %*% (Gw - (nu) * A11) %*% 
        invA11 %*% A12
    Hmat <- rbind(cbind(Hmat11, t(Hmat12)), cbind(Hmat12, Hmat22))
    Hmat <- Hmat[match(rownames(Amat), rownames(Hmat)), match(colnames(Amat), 
        colnames(Hmat))]
    return(Hmat)
}