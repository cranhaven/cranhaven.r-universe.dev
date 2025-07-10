CovComb <- function(Klist = NULL, Kinvlist = NULL, lambda = 1, w = 1, 
    nu = 1000, maxiter = 500, miniter = 100, Kinit = NULL, 
    tolparconv = 1e-04, loglik=FALSE, plotll=FALSE) {
    if (((is.null(Kinvlist)) && (!is.null(Klist)))) {
        Kinvlist <- lapply(Klist, solve)
    }
    if (((!is.null(Kinvlist)) && (is.null(Klist)))) {
        Klist <-lapply(Kinvlist, solve)
    }
    linenames <- c()
    for (i in seq_len(length(Kinvlist))){
        linenames <- union(linenames, rownames(Kinvlist[[i]]))
    }
    if (!is.null(Kinit)) {
        Kinit <- Kinit[match(linenames, colnames(Kinit)), match(linenames, 
            rownames(Kinit))]/nu
    }
    if (is.null(Kinit)) {
        Hmat <- Hinvmat <- diag(length(linenames))/nu
    } else {
        Hmat <- Hinvmat <- Kinit
    }
    if (length(w)<2){
      wvec<-rep(w,length(Klist))
    } else{
      wvec<-w
      }
    rownames(Hmat) <- colnames(Hmat) <- rownames(Hinvmat) <- colnames(Hinvmat) <- linenames
    logLikvec <- c()
    for (i in seq_len(maxiter)) {
        Hmatadd <- matrix(0, nrow = nrow(Hmat), ncol = ncol(Hmat))
        dimnames(Hmatadd) <- dimnames(Hinvmat)
        for (j in seq_len(length(Klist))) {
            Hmatadd <- Hmatadd + (Hmatfunc(Hmat, Klist[[j]], w = wvec[j], 
                nu = nu))
            colnames(Hmatadd) <- rownames(Hmatadd) <- rownames(Hinvmat)
        }
        Hmatadd <- Hmatadd/(length(Klist) * nu)
        lambdat <- lambda
        sum(is.na(Hmatadd))
        loglikval <- NA
       # print(nlme::logDet(nlme::pdSymm(Hmatadd)))
        while ((is.na(loglikval) & !(0.5 > lambdat))) {
            Hmat <- as.matrix(Matrix::nearPD(lambdat * Hmatadd + 
                (1 - lambdat) * Hmat)$mat)
            rownames(Hmat) <- colnames(Hmat) <- linenames
            loglikval <- tryCatch(loglikfunc(Hmat, Klist, nu), error = function(e) {
                return(NA)
            })
            lambdat <- max(0, lambdat - 5 * 0.01)
        }
        
        logLikvec <- c(logLikvec, loglikval)
       # if (sum(!is.na(logLikvec)) > 0) {
       #     plot(logLikvec, type = "b", pch = 10, cex = 0.5)
       #     maxloglik <- max(na.omit(logLikvec))
        #    abline(h = maxloglik, col = "red")
       # }
        if (sum(!is.na(logLikvec)) > 0) {
        maxloglik <- max(na.omit(logLikvec))
        }else {maxloglik=-Inf}
        if (!is.na(loglikval)) {
            logLikvec01 <- (logLikvec - min(na.omit(logLikvec)))/(max(na.omit(logLikvec)) - 
                min(na.omit(logLikvec)))
            if (length(logLikvec01) > miniter) {
                tocomp <- min(logLikvec01[(length(logLikvec01) - 
                  2):length(logLikvec01)])
                if (is.na(tocomp)) {
                  tocomp <- 1 - tolparconv
                }
                if (max(na.omit(logLikvec01)) - tolparconv < tocomp) {
                #  print("convergence")
                  (break)()
                }
            }
        }
    }
    outlist <- list(Hmat * nu, logLikvec)
   if (loglik){
     if (plotll){
       if (sum(!is.na(logLikvec)) > 0) {
            plot(logLikvec, type = "b", pch = 10, cex = 0.5)
            maxloglik <- max(na.omit(logLikvec))
           abline(h = maxloglik, col = "red")
        }
     }
   }
    if (loglik){
      return(outlist)
      } else{
    return(outlist[[1]])
    }
}
