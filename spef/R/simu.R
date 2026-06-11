# Simulate data from various data settings.
# List of internal functions :
#   simu.Zhang
#   simu.Huang3
#   simu.Huang4
#   simu.Huang4IC
#   simu.HuangScen2

##############################################################################
## Zhang's Study 1, Poisson given covariate; Study mixed Poisson given covariate
##############################################################################
simu.Zhang <- function(dataSetting) {
    N <- dataSetting@N
    X <- cbind(runif(N), rnorm(N), rbinom(N, 1, 0.5))

    intensity <- 2 * c(exp(X %*% dataSetting@beta))
    if (dataSetting@frailty) {
        z <- sample(c(0.8, 1, 1.2), size=N,
                    replace=TRUE, prob=c(0.25, 0.5, 0.25))
        intensity <- intensity * z
    }
    nObs <- sample(1:6, N, replace=TRUE)
    df <- NULL
    for (i in 1:N) {
        sq <- unique(round(sort(runif(nObs[i], 1, 10)), 2))
        nObs[i] <- length(sq)
        dsq <- diff(c(0, sq))
        df <- rbind(df,
                    data.frame(ID=i,
                               time=sq,
                               count=rpois(n=nObs[i], lambda=dsq * intensity[i])))
    }
    X <- X[rep(1:N, nObs), ]
    colnames(X) <- paste("X", 1:3, sep="")
    df <- cbind(df, X)
    subset(df, time > 0)
}

##############################################################################
## Huang's Study 3, informative observation process (only depend on covariate)
##############################################################################
simu.Huang3 <- function(dataSetting) {
    N <- dataSetting@N
    X <- rbinom(N, 1, 0.5)
    intensity <- 2 * exp(X * dataSetting@beta)
    if (dataSetting@frailty) {
        z <- rgamma(N, shape=2, scale=0.5)
        intensity <- intensity * z
    }
    nObs <- rpois(N, lambda=log(21) * exp(X / 2))
    df <- NULL
    for (i in which(nObs > 0)) {
        sq <- unique(round(sort((21^runif(nObs[i]) - 1) / 2), 2))
        nObs[i] <- length(sq)
        dsq <- diff(c(0, sq))
        df <- rbind(df,
                    data.frame(ID=i,
                               time=sq,
                               count=rpois(n=nObs[i], lambda=dsq * intensity[i]),
                               X=X[i]))
    }
    subset(df, time > 0 & time <= 10)
}

##############################################################################
## Huang's Study 4, informative observation times (depend on covariate and frailty)
##############################################################################
simu.Huang4 <- function(dataSetting) {
    N <- dataSetting@N
    X <- rbinom(N, 1, 0.5)
    intensity <- 2 * exp(X * dataSetting@beta)
    if (dataSetting@frailty) {
        z <- rgamma(N, shape=2, scale=0.5)
        intensity <- intensity * z
        g1 <- which(X == 1 & z > 1)
    } else {
        g1 <- which(X == 1)
    }
    g2 <- (1:N)[-g1]
    nObs <- rep(0, N)
    nObs[g1] <- sample(1:8, length(g1), replace=TRUE)
    nObs[g2] <- sample(1:6, length(g2), replace=TRUE)
    df <- NULL
    for (i in 1:N) {
        if (i %in% g1)
            sq <- unique(round(sort(rexp(nObs[i], rate=1/2)), 2))
        else
            sq <- unique(round(sort(runif(nObs[i], 0, 10)), 2))
        nObs[i] <- length(sq)
        dsq <- diff(c(0, sq))
        df <- rbind(df,
                    data.frame(ID=i,
                               time=sq,
                               count=rpois(n=nObs[i], lambda=dsq * intensity[i]),
                               X=X[i]))
    }
    subset(df, time > 0 & time <= 10)
}

##############################################################################
## Independent censoring times of Huang's Study 4
##############################################################################
simu.Huang4IC <- function(dataSetting) {
    N <- dataSetting@N
    X <- rbinom(N, 1, 0.5)
    intensity <- 2 * exp(X * dataSetting@beta)
    if (dataSetting@frailty) {
        z <- rgamma(N, shape=2, scale=0.5)
        intensity <- intensity * z
        g1 <- which(X == 1 & z > 1)
    } else {
        g1 <- which(X == 1)
    }
    g2 <- (1:N)[-g1]
    nObs <- rep(0, N)
    nObs[g1] <- sample(1:8, length(g1), replace=TRUE)
    nObs[g2] <- sample(1:6, length(g2), replace=TRUE)
    tau <- round(runif(N, 6, 10), 2)
    df <- NULL
    for (i in 1:N) {
        if (nObs[i] == 1) {
            sq <- tau[i]
        } else {
            sq <- unique(round(sort(c(runif(nObs[i] - 1, 0, tau[i]), tau[i])), 2))
            nObs[i] <- length(sq)
        }
        dsq <- diff(c(0, sq))
        df <- rbind(df,
                    data.frame(ID=i,
                               time=sq,
                               count=rpois(n=nObs[i], lambda=dsq * intensity[i]),
                               X=X[i]))
    }
    subset(df, time > 0)
}

##############################################################################
## Conditional independent censoring times of Huang's Study 4
##############################################################################
simu.Huang4CIC <- function(dataSetting) {
    N <- dataSetting@N
    X <- rbinom(N, 1, 0.5)
    intensity <- 2 * exp(X * dataSetting@beta)
    z <- rgamma(N, shape=2, scale=0.5)
    intensity <- intensity * z
    ## Number of observation time
    nObs <- rep(0, N)
    g1 <- which(X == 1 & z > 1)
    g2 <- (1:N)[-g1]
    nObs[g1] <- sample(1:8, length(g1), replace=TRUE)
    nObs[g2] <- sample(1:6, length(g2), replace=TRUE)
    ## Censoring time
    cTime <- rep(10, N)
    cTime[which(X == 1)] <- round(runif(length(which(X == 1)), 4, 10), 2)
    df <- NULL
    for (i in 1:N) {
        if (i %in% g1)
            sq <- unique(round(sort(rexp(nObs[i], rate=1/2)), 2))
        else
            sq <- unique(round(sort(runif(nObs[i], 0, 10)), 2))
        sq <- c(subset(sq, sq < cTime[i]), cTime[i])
        nObs[i] <- length(sq)
        dsq <- diff(c(0, sq))
        df <- rbind(df,
                    data.frame(ID=i,
                               time=sq,
                               count=rpois(n=nObs[i], lambda=dsq * intensity[i]),
                               X=X[i]))
    }
    subset(df, time > 0)
}

##############################################################################
## Copy from sim-scen2.s, Huang's Study 4
##############################################################################
simu.HuangScen2 <- function(dataSetting) {
    n <- dataSetting@N
    b0 <- dataSetting@beta
    data0 <- data1 <- NULL
    y <- NULL
    mi <- rep(-99,n)
    for (i in 1:n) {
        ## x is a covariate vector
        x <- rbinom(1,1,0.5)
        z <- k <- t <- m <- ab <- NULL
        z <- rgamma(1,16)/16   # latent variable
        t <- 0
        if( x == 0 ) {
            if( z <= 1 ) {
                k <- as.numeric( cut( runif(1,0,1), 0:4/4 ) ) # No of obs times
                for(l in 1:k) {
                    t0 <- round(runif(1,0,10),2)
                    while( any(t-t0==0) ) t0 <- round(runif(1,0,10),2)
                    t <- c(t,t0)
                }
            }
            else if( z > 1) {
                k <- as.numeric( cut( runif(1,0,1), 0:8/8 ) ) # No of obs times
                for(l in 1:k) {
                    t0 <- round(rexp(1,0.5),2)
                    while( any(t-t0==0)|t0>10 ) t0 <- round(rexp(1,0.5),2)
                    t <- c(t,t0)
                }
            }
        }
        else { k <- as.numeric( cut( runif(1,0,1), 0:6/6 ) ) # No of obs times
               for(l in 1:k) {
                   t0 <- round(runif(1,0,10),2)
                   while( any(t-t0==0) ) t0 <- round(runif(1,0,10),2)
                   t <- c(t,t0)
               }
           }
        t <- sort(t)  # observation times
        y <- round(cbind(y,t[k+1]),2)
        ab <- unlist(sapply( 1:k,      # generate interval censored data
                            function(l){
                                m <- rpois(1, 2*t[l+1]*exp(b0*x)-2*t[l]*exp(b0*x))
                                rbind(i,t[l],t[l+1],t[k+1],m )
                            } ))
        ab <- matrix(ab, ncol=5, byrow=T)
        mi[i] <- sum(ab[,5])
        ## data0 is in the format Li Ri Ti #
        data0 <-rbind(data0,cbind(ab,rep(mi[i],k),k))
        data1 <- rbind(data1,c(i,1,x))
    }
    ## data0 = (subject, start, end, censor, count, total count, nObs)
    ## data1 = (subject, 1, X)
    df <- data.frame(ID=data0[, 1], time=data0[, 3], count=data0[, 5],
                     X=rep(data1[, 3], as.numeric(table(data0[, 1]))))
    df
}

##############################################################################
## Class Definition
##############################################################################
setClass("DataSetting",
         representation(N="numeric", beta="numeric"),
         prototype(N=100))

setClass("Zhang",
         representation(frailty="logical"),
         prototype(beta=c(-1, 0.5, 1.5), frailty=FALSE),
         contains="DataSetting")

setClass("Huang3",
         representation(frailty="logical"),
         prototype(beta=-1, frailty=FALSE),
         contains="DataSetting")

setClass("Huang4",
         representation(frailty="logical"),
         prototype(beta=-1, frailty=TRUE),
         contains="DataSetting")

setClass("Huang4IC",
         representation(frailty="logical"),
         prototype(beta=-1, frailty=TRUE),
         contains="DataSetting")

setClass("Huang4CIC",
         representation(),
         prototype(beta=-1),
         contains="DataSetting")

setClass("HuangScen2",
         representation(),
         prototype(beta=-1),
         contains="DataSetting")

##############################################################################
## Method Dispatch
##############################################################################
setGeneric("simu",
           function(dataSetting) {
               standardGeneric("simu")
           })

setMethod("simu",
          signature("Zhang"),
          simu.Zhang)

setMethod("simu",
          signature("Huang3"),
          simu.Huang3)

setMethod("simu",
          signature("Huang4"),
          simu.Huang4)

setMethod("simu",
          signature("Huang4IC"),
          simu.Huang4IC)

setMethod("simu",
          signature("Huang4CIC"),
          simu.Huang4CIC)

setMethod("simu",
          signature("HuangScen2"),
          simu.HuangScen2)
