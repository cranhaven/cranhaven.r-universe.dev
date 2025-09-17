condnumBpI <- function(delta, B, p){
    (kappa(B+delta * diag(p))-p)^2
}

omega_banded <- function(p, rho=0.6){
    temp <- (1:p) %*% matrix(1,1,p)
    return(rho^(abs(temp-t(temp))))
}

omega_sparse <- function(p){
    B <- matrix(0,p,p)
    upt <- which(upper.tri(B,FALSE)) # only deal with upper triangular part
    bij0.5 <- upt[runif(length(upt))<=0.1] # sample the non zero entries in B
    B[bij0.5] <- 0.5
    B <- B + t(B)
    # find the delta so that condition number match
    delta_opt <- optim(par = 1, fn=condnumBpI, B = B, p = p, 
                        method = "Brent", lower = 0, upper = 1000*p)
    return(B + delta_opt$par * diag(p))
}

omega_dense <- function(p){
    B <- matrix(0.5,p,p)
    diag(B) <- 1
    return(B)
}

