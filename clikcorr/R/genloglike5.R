genloglike5 <-
function(F) {
    # Return a function that evaluates the 5 parameter log likelihood
    # for a bivariate normal model under censoring.
    #
    # Arguments:
    #  F: A list formatted as returned by 'prepare_data'.
    #
    # Returns:
    #  loglike : a function with argument F that returns the log-likelihood
    #            evaluated at F, where the mean is [F[1],F[2]], and the
    #            covariance matrix is [[F[3],F[4]],[F[4],F[5]]].

    ## All pairs among Obs, Miss, Left, Right, Interval.
    ## Insert generated code here
    iOO <- intersect(F$Obs[[1]], F$Obs[[2]])
    YOO <- cbind(F$Y1[iOO,1], F$Y2[iOO,1])
    iOM <- intersect(F$Obs[[1]], F$Miss[[2]])
    YOM <- F$Y1[iOM,1]
    iOL <- intersect(F$Obs[[1]], F$Left[[2]])
    YOL <- cbind(F$Y1[iOL,1], F$Y2[iOL,2])
    iOR <- intersect(F$Obs[[1]], F$Right[[2]])
    YOR <- cbind(F$Y1[iOR,1], F$Y2[iOR,1])
    iOI <- intersect(F$Obs[[1]], F$Interval[[2]])
    YOI <- cbind(F$Y1[iOI,1], F$Y2[iOI,1], F$Y2[iOI,2])
    iMO <- intersect(F$Miss[[1]], F$Obs[[2]])
    YMO <- F$Y2[iMO,1]
    iML <- intersect(F$Miss[[1]], F$Left[[2]])
    YML <- F$Y2[iML,2]
    iMR <- intersect(F$Miss[[1]], F$Right[[2]])
    YMR <- F$Y2[iMR,1]
    iMI <- intersect(F$Miss[[1]], F$Interval[[2]])
    YMI <- cbind(F$Y2[iMI,1], F$Y2[iMI,2])
    iLO <- intersect(F$Left[[1]], F$Obs[[2]])
    YLO <- cbind(F$Y1[iLO,2], F$Y2[iLO,1])
    iLM <- intersect(F$Left[[1]], F$Miss[[2]])
    YLM <- F$Y1[iLM,2]
    iLL <- intersect(F$Left[[1]], F$Left[[2]])
    YLL <- cbind(F$Y1[iLL,2], F$Y2[iLL,2])
    iLR <- intersect(F$Left[[1]], F$Right[[2]])
    YLR <- cbind(F$Y1[iLR,2], F$Y2[iLR,1])
    iLI <- intersect(F$Left[[1]], F$Interval[[2]])
    YLI <- cbind(F$Y1[iLI,2], F$Y2[iLI,1], F$Y2[iLI,2])
    iRO <- intersect(F$Right[[1]], F$Obs[[2]])
    YRO <- cbind(F$Y1[iRO,1], F$Y2[iRO,1])
    iRM <- intersect(F$Right[[1]], F$Miss[[2]])
    YRM <- F$Y1[iRM,1]
    iRL <- intersect(F$Right[[1]], F$Left[[2]])
    YRL <- cbind(F$Y1[iRL,1], F$Y2[iRL,2])
    iRR <- intersect(F$Right[[1]], F$Right[[2]])
    YRR <- cbind(F$Y1[iRR,1], F$Y2[iRR,1])
    iRI <- intersect(F$Right[[1]], F$Interval[[2]])
    YRI <- cbind(F$Y1[iRI,1], F$Y2[iRI,1], F$Y2[iRI,2])
    iIO <- intersect(F$Interval[[1]], F$Obs[[2]])
    YIO <- cbind(F$Y1[iIO,1], F$Y1[iIO,2], F$Y2[iIO,1])
    iIM <- intersect(F$Interval[[1]], F$Miss[[2]])
    YIM <- cbind(F$Y1[iIM,1], F$Y1[iIM,2])
    iIL <- intersect(F$Interval[[1]], F$Left[[2]])
    YIL <- cbind(F$Y1[iIL,1], F$Y1[iIL,2], F$Y2[iIL,2])
    iIR <- intersect(F$Interval[[1]], F$Right[[2]])
    YIR <- cbind(F$Y1[iIR,1], F$Y1[iIR,2], F$Y2[iIR,1])
    iII <- intersect(F$Interval[[1]], F$Interval[[2]])
    YII <- cbind(F$Y1[iII,1], F$Y1[iII,2], F$Y2[iII,1], F$Y2[iII,2])

    loglike <- function(FF) {

        L <- 0
        
        Mu <- FF[1:2]
        C <- array(c(exp(FF[3]),FF[4],FF[4],exp(FF[5])), c(2,2))

        ## Check that the covariance matrix is SPD.
        if ((min(diag(C))<=0) | (C[1,1]*C[2,2]-C[1,2]^2<=0)) {
            return(-Inf)
        }

        ## Insert generated code here
        if (length(iOO) > 0) {
            d <- YOO[,1] - Mu[1]
            p <- -0.5*log(C[1,1]) - 0.5*d^2/C[1,1]
            L <- L + sum(p)    
            cm <- Mu[2] + C[1,2]*d/C[1,1]
            cv <- C[2,2] - C[1,2]^2/C[1,1]
            d <- YOO[,2] - cm
            p <- -0.5*log(cv) - 0.5*d^2/cv
            L <- L + sum(p)        
        }

        if (length(iOM) > 0) {
            d <- YOM - Mu[1]
            p <- -0.5*log(C[1,1]) - 0.5*d^2/C[1,1]
            L <- L + sum(p)        
        }

        if (length(iOL) > 0) {
            d <- YOL[,1] - Mu[1]
            p <- -0.5*log(C[1,1]) - 0.5*d^2/C[1,1]
            L <- L + sum(p)    
            cm <- Mu[2] + C[1,2]*d/C[1,1]
            cv <- C[2,2] - C[1,2]^2/C[1,1]
            d <- YOL[,2] #- cm
            p <- pnorm((d-cm)/sqrt(cv))
            L <- L + sum(log(p))        
        }

        if (length(iOR) > 0) {
            d <- YOR[,1] - Mu[1]
            p <- -0.5*log(C[1,1]) - 0.5*d^2/C[1,1]
            L <- L + sum(p)    
            cm <- Mu[2] + C[1,2]*d/C[1,1]
            cv <- C[2,2] - C[1,2]^2/C[1,1]
            d <- YOR[,2] #- cm
            p <- pnorm((d-cm)/sqrt(cv))
            L <- L + sum(log(1-p))        
        }

        if (length(iOI) > 0) {
            d <- YOI[,1] - Mu[1]
            p <- -0.5*log(C[1,1]) - 0.5*d^2/C[1,1]
            L <- L + sum(p)    
            cm <- Mu[2] + C[1,2]*d/C[1,1]
            cv <- C[2,2] - C[1,2]^2/C[1,1]
            d1 <- YOI[,2] - cm
            d2 <- YOI[,3] - cm
            p1 <- pnorm(d1/sqrt(cv))
            p2 <- pnorm(d2/sqrt(cv))
            L <- L + sum(log(p2-p1))        
        }

        if (length(iMO) > 0) {
            d <- YMO - Mu[2]
            p <- -0.5*log(C[2,2]) - 0.5*d^2/C[2,2]
            L <- L + sum(p)        
        }

        if (length(iML) > 0) {
            p <- pnorm((YML-Mu[2]) / sqrt(C[2,2]))
            L <- L + sum(log(p))        
        }

        if (length(iMR) > 0) {
            p <- pnorm((YMR-Mu[2]) / sqrt(C[2,2]))
            L <- L + sum(log(1-p))        
        }

        if (length(iMI) > 0) {
            p1 <- pnorm((YMI[,2]-Mu[2]) / sqrt(C[2,2]))
            p2 <- pnorm((YMI[,1]-Mu[2]) / sqrt(C[2,2]))
            L <- L + sum(log(p1-p2))        
        }

        if (length(iLO) > 0) {
            d <- YLO[,2] - Mu[2]
            p <- -0.5*log(C[2,2]) - 0.5*d^2/C[2,2]
            L <- L + sum(p)    
            cm <- Mu[1] + C[1,2]*d/C[2,2]
            cv <- C[1,1] - C[1,2]^2/C[2,2]
            d <- YLO[,1] #- cm
            p <- pnorm((d-cm)/sqrt(cv))
            L <- L + sum(log(p))        
        }

        if (length(iLM) > 0) {
            p <- pnorm((YLM-Mu[1]) / sqrt(C[1,1]))
            L <- L + sum(log(p))        
        }

        if (length(iLL) > 0) {
            for (i in 1:length(iLL)) {
                p <- pmvnorm(mean=Mu, sigma=C, lower=c(-Inf,-Inf),
                       upper=c(YLL[i,1],YLL[i,2]))
                L <- L + log(p)
            }    
        }

        if (length(iLR) > 0) {
            for (i in 1:length(iLR)) {
                p <- pmvnorm(mean=Mu, sigma=C, lower=c(-Inf,YLR[i,2]),
                       upper=c(YLR[i,1],Inf))
                L <- L + log(p)
            }    
        }

        if (length(iLI) > 0) {
            for (i in 1:length(iLI)) {
                p <- pmvnorm(mean=Mu, sigma=C, lower=c(-Inf,YLI[i,2]),
                       upper=c(YLI[i,1],YLI[i,3]))
                L <- L + log(p)
            }    
        }

        if (length(iRO) > 0) {
            d <- YRO[,2] - Mu[2]
            p <- -0.5*log(C[2,2]) - 0.5*d^2/C[2,2]
            L <- L + sum(p)    
            cm <- Mu[1] + C[1,2]*d/C[2,2]
            cv <- C[1,1] - C[1,2]^2/C[2,2]
            d <- YRO[,1] #- cm
            p <- pnorm((d-cm)/sqrt(cv))
            L <- L + sum(log(1-p))        
        }

        if (length(iRM) > 0) {
            p <- pnorm((YRM-Mu[1]) / sqrt(C[1,1]))
            L <- L + sum(log(1-p))        
        }

        if (length(iRL) > 0) {
            for (i in 1:length(iRL)) {
                p <- pmvnorm(mean=Mu, sigma=C, lower=c(YRL[i,1],-Inf),
                       upper=c(Inf,YRL[i,2]))
                L <- L + log(p)
            }    
        }

        if (length(iRR) > 0) {
            for (i in 1:length(iRR)) {
                p <- pmvnorm(mean=Mu, sigma=C, lower=c(YRR[i,1],YRR[i,2]),
                       upper=c(Inf,Inf))
                L <- L + log(p)
            }    
        }

        if (length(iRI) > 0) {
            for (i in 1:length(iRI)) {
                p <- pmvnorm(mean=Mu, sigma=C, lower=c(YRI[i,1],YRI[i,2]),
                       upper=c(Inf,YRI[i,3]))
                L <- L + log(p)
            }    
        }

        if (length(iIO) > 0) {
            d <- YIO[,3] - Mu[2]
            p <- -0.5*log(C[2,2]) - 0.5*d^2/C[2,2]
            L <- L + sum(p)    
            cm <- Mu[1] + C[1,2]*d/C[2,2]
            cv <- C[1,1] - C[1,2]^2/C[2,2]
            d1 <- YIO[,1] - cm
            d2 <- YIO[,2] - cm
            p1 <- pnorm(d1/sqrt(cv))
            p2 <- pnorm(d2/sqrt(cv))
            L <- L + sum(log(p2-p1))        
        }

        if (length(iIM) > 0) {
            p1 <- pnorm((YIM[,2]-Mu[1]) / sqrt(C[1,1]))
            p2 <- pnorm((YIM[,1]-Mu[1]) / sqrt(C[1,1]))
            L <- L + sum(log(p1-p2))        
        }

        if (length(iIL) > 0) {
            for (i in 1:length(iIL)) {
                p <- pmvnorm(mean=Mu, sigma=C, lower=c(YIL[i,1],-Inf),
                       upper=c(YIL[i,2],YIL[i,3]))
                L <- L + log(p)
            }    
        }

        if (length(iIR) > 0) {
            for (i in 1:length(iIR)) {
                p <- pmvnorm(mean=Mu, sigma=C, lower=c(YIR[i,1],YIR[i,3]),
                       upper=c(YIR[i,2],Inf))
                L <- L + log(p)
            }    
        }

        if (length(iII) > 0) {
            for (i in 1:length(iII)) {
                p <- pmvnorm(mean=Mu, sigma=C, lower=c(YII[i,1],YII[i,3]),
                       upper=c(YII[i,2],YII[i,4]))
                L <- L + log(p)
            }    
        }

        return(L)
    }
      

    return(loglike)  
}
