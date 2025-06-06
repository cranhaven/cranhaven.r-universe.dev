cdf.qp <-
function(expectreg, x = NA, qout = NA, extrap = FALSE, e0 = NA, eR = NA, lambda = 0, var.dat = NA)
{
    epsilon = 1e-05
    max.iter = 20
    step.halfing = 0.5
    p = expectreg$asymmetries
    if (is.na(x)) 
        e <- expectreg$fitted[1, ]
    else if (length(which(expectreg$covariates[[1]] == x)) > 
             0) 
        e <- expectreg$fitted[which(expectreg$covariates[[1]] == 
                                        x)[1], ]
    else e <- expectreg$fitted[which.min(abs(expectreg$covariates[[1]] - 
                                                 x))[1], ]
    e = sort(e)
    K <-  length(e)
    if(is.na(var.dat) || var.dat < 0)
    {
        var.dat <- var(e)
    }
    if (is.null(p)==TRUE) # if no p is given we assume that quantiles are for equidistant p
    {
        p <- seq(0 + 1/(K+1), 1 - 1/(K+1), length = K)
    }
    mat <- apply(matrix(p), 1, all.equal, 0.5)
    k0 <- which(mat == 'TRUE')
    k0.i <- length(k0)  
    if (is.na(e0) == TRUE) 
    {
        e0 <- min(e) + (min(e) - min(e[-which.min(e)])) 
    }  
    if (is.na(eR) == TRUE) 
    {
        eR <- max(e) + (max(e) - max(e[-which.max(e)]))
    }   
    if (k0.i) mu05 <- e[k0] else mu05 <- approx(p,y=e,xout=0.5)$y # Approximation of mean value 
    eg <-  c(e0, e)            
    
    step <- eg[-1]-eg[-length(eg)]  
    if(any(as.vector(step == 0)))
    {
        ind <- which(as.vector(step) == 0)
        step[ind] <- 1e-16
    }                
    eg <- eg[-1]+eg[-length(eg)]
    
    eg <- eg/2
    egR <- (e[K] + eR)/2 
    P <- diag(1/step[-1], K-1, K-1)
    P <- cbind(0, P)
    diag(P) <- -1/step[-K] 
    Kmat <- t(P)%*%P
    
    delta <- rep(1/(K+1),K)
    loop <-  0
    iter.diff <-  1
    while ((loop < max.iter) & (iter.diff>epsilon))
    {
        loop <-  loop + 1
        F <- cumsum(delta)
        Fs <- ( kronecker( matrix(1:K), matrix(1,1,K)) >= kronecker(matrix(1,K,1), t(matrix(1:K))))*1
        
        G <- cumsum(eg*delta)
        
        Gs <- kronecker(matrix(1,K,1), t(matrix(eg))) * Fs
        h <- e - ( (1-p) * G + p * (mu05-G))/ ( (1-p) * F + p * (1-F))
        if(k0.i)
        {
            h.tilde <- h[-k0]
            hk0 <- mu05 - (G[K] + egR * (1 - F[K]))
            h <- c(h.tilde, hk0)
        }
        hs <- - kronecker( matrix((1-2*p)/((1-p)*F + p*(1-F))), matrix(1,1,K)) * Gs +
            kronecker( matrix( (( (1-p)*G+p*(mu05-G))*(1-2*p))/ (((1-p)*F + p*(1-F))**2)),matrix(1,1,K))*Fs 
        if(k0.i)
        {
            hs.tilde <- hs[-k0,]  
            hsk0 <- -eg + egR  
            hs <- rbind(hs.tilde, hsk0)
        }         
        Ls <- t(hs) %*% h
        Lss1 <-  t(hs) %*%  hs
        Lss <- Lss1         
        
        dvec <-  -Ls 
        
        Dmat <- Lss + (lambda * var.dat^2) * Kmat  
        
        ### Zwischenschritt: Überprüfung auf Nichtsingularität
        if(qr(Dmat)$rank != K) 
        {

            penalty.term <- 0.001 * var.dat
            Dmat <-  Dmat + penalty.term*diag(K)
        }
        
        Amat <- cbind(diag(K), -matrix(1,K,1))
        bvec <- matrix(c(-delta, sum(delta)-1))
        
        xsi <- solve.QP(Dmat, dvec, Amat, bvec, meq=0)$solution 
        
        delta <- delta + step.halfing * xsi
        iter.diff <- max(abs(xsi))
        
    }
    Fdistcum <- cumsum(delta)
    if (any(is.na(qout))) 
        qout = p
    if (extrap) 
        quant <- as.vector(my.approx(Fdistcum, e, xout = qout, rule = 3)$y)
    else quant <- as.vector(my.approx(Fdistcum, e, xout = qout, rule = 2)$y)
    result = list(x = e, density = delta, cdf = Fdistcum, quantiles = quant, qout = qout)
    class(result) = "expectilecdf"
    return(result)    
}
