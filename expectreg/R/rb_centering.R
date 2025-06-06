rb_centering <-
function(B, P, min.eigenvalue = 1e-10) {
    
    #S <- t(P) %*% P
    S <- P
    C <- matrix(colMeans(B),1,ncol(B))
    
    maBB <- mean(abs(t(B)%*%B)) # `size' of B'B
    maS <- mean(abs(S)) / maBB
    S <- S / maS
    
    k <- ncol(B)
    
    j <- nrow(C)
    
    qrc  <- qr(t(C)) 
    ZSZ  <- qr.qty(qrc,S)[(j+1):k,]
    ZtSZ <- t(qr.qty(qrc,t(ZSZ))[(j+1):k,]) ## Z'SZ
    XZ   <- t(qr.qty(qrc,t(B))[(j+1):k,]) ## form XZ
    
    es <- eigen(ZtSZ,symmetric=TRUE)
    U <- es$vectors
    D <- abs(es$values)
    
    nbunp <- sum(D < min.eigenvalue) # number of unpenalized elements
    nbp <- sum(D >= min.eigenvalue) # number of penalized elements
    ind1 <- D >= min.eigenvalue ## index penalized elements
    D[ind1] <- 1/sqrt(D[ind1])
    D[!ind1] <- 1
    D_new <- t(D*t(U)) ## D <- U%*%diag(D)
    
    B <- XZ%*%D_new 
    B <- B[,ncol(B):1] # put the unpenalized part in the front
    P <- diag(c(rep(0,length(ind1) - sum(ind1)), rep(1,ncol(B)-(length(ind1) - sum(ind1)))))
    param_center <- list("qrc"=qrc, "D_new"=D_new, "ind1"=ind1, "j"=j, "k"=k, 
                         "B_mean" = C, "nbunp" = nbunp, "nbp" = nbp)
    list("B"=B, "P"=P, "param_center"=param_center)
}
