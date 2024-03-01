ncMcTest <- function(x,y,data=NULL,ncomp=2,ncincr=1,intercept=TRUE,nsim=99,
                     seed=NULL,ts1=NULL,ts2=NULL,semiPar=FALSE,
                     conditional=semiPar,verb=FALSE,progRep=TRUE,...) {
#
# Function ncMcTest --- number of components Monte Carlo Test. Test
# for K components versus K+INCR components in a mixture of
# linear regressions (with normal errors) via a Monte Carlo test.
# The Monte Carlo procedure is necessary since the likelihood ratio
# statistic.  (NOTE:  This statistic DOES NOT have a chi-squared-nu
# distribution!!!) (Where --- irrelevantly --- nu is the number of
# parameters for any one component, equal to p+2 where p is the
# number of linear parameters in each component.)  The simulated
# statistics for the Monte Carlo test are created either by
# generating Gaussian data from the fitted model or by means of a
# semi-parametric bootstrap procedure.
#
# Screw-ups:
#            1 <--> singularity under H0
#            2 <--> didn't converge, ncomp components;
#            3 <--> singularity under Ha
#            4 <--> converged for n components but not for ncomp+ncincr components;
#            5 <--> lrs(ncomp) > lrs(ncomp+ncincr).

obj1 <- mixreg(x=x,y=y,data=data,ncomp=ncomp,intercept=intercept,
               thetaStart=ts1,verb=verb,...)
obj2 <- mixreg(x=x,y=y,data=data,ncomp=ncomp+ncincr,intercept=intercept,
               thetaStart=ts2,verb=verb,...)
theta1 <- obj1$theta
theta2 <- obj2$theta
fmla   <- obj1$formula
nmy    <- as.character(fmla[[2]])
data   <- obj1$data

rslt <- vector("list",nsim)
screwUps <- vector("list",nsim)
aic1 <- vector("list",nsim)
aic2 <- vector("list",nsim)
if(is.null(seed)) seed <- sample(1:1e5,1)
set.seed(seed)
seeds <- sample(1:1e5,nsim)
k <- 0
for(i in 1:nsim) {
    repeat {
        simdat <- rmixreg(obj1,semiPar=semiPar,conditional=conditional,
                         seed=seeds[i],...)
        tmp <- try(mixreg(fmla,data=simdat,ncomp=ncomp,thetaStart=theta1,
                          warn=FALSE,verb=verb,...),silent=TRUE)
        if(inherits(tmp,"try-error")) {
            k <- k+1
            screwUps[[k]] <- list(seed=seeds[i],i=i,type=1)
            seeds[i] <- sample(1:1e5,1)
            if(progRep) cat("\nSingularity under H_0; i = ",i,".\n",sep="")
            next
        }
        if(!tmp$converged) {
            k <- k+1
            screwUps[[k]] <- list(seed=seeds[i],i=i,type=2)
            seeds[i] <- sample(1:1e5,1)
            if(progRep) cat("\nDid not converge under H_0; i = ",i,".\n",sep="")
            next
        }
        l1 <- tmp$log.like
        a1 <- tmp$aic
        tmp <- try(mixreg(fmla,data=simdat,ncomp=ncomp+ncincr,thetaStart=theta2,
                          warn=FALSE,verb=verb,...),silent=TRUE)
        if(inherits(tmp,"try-error")) {
            k <- k+1
            screwUps[[k]] <- list(seed=seeds[i],i=i,type=3)
            seeds[i] <- sample(1:1e5,1)
            if(progRep) cat("\nSingularity under H_a; i = ",i,".\n",sep="")
            next
        }
        if(!tmp$converged) {
            k <- k+1
            screwUps[[k]] <- list(seed=seeds[i],i=i,type=4)
            seeds[i] <- sample(1:1e5,1)
            if(progRep) cat("\nDid not converge under H_a; i = ",i,".\n",sep="")
            next
        }
        l2  <- tmp$log.like
        a2  <- tmp$aic
        if(l1 > l2) {
            k <- k+1
            screwUps[[k]] <- list(seed=seeds[i],i=i,type=5)
            seeds[i] <- sample(1:1e5,1)
            if(progRep) cat("\nLog likelihoods in wrong order i = ",i,".\n",sep="")
            next
        }
        break
    }
    rslt[[i]] <- 2*(l2-l1)
    aic1[[i]] <- a1
    aic2[[i]] <- a2
    if(progRep) cat(i,"")
    if(progRep & i%%10 == 0) cat("\n")
}
if(progRep & nsim%%10 !=0) cat("\n")

rslt <- unlist(rslt)
lrs  <- 2*(obj2$log.like-obj1$log.like)
pval <- (1+sum(lrs<=rslt))/(1+nsim)
if(k > 0) {
    scrps <- data.frame(seed=unlist(lapply(screwUps,function(x){x[[1]]})),
                        i=unlist(lapply(screwUps,function(x){x[[2]]})),
                        type=unlist(lapply(screwUps,function(x){x[[3]]})))
} else scrps <- NULL
df <- length(unlist(theta2)) - length(unlist(theta1))
xxx <- list(lrs=lrs,pval=pval,simStats=rslt,unlist(aic1),
            unlist(aic2),df=df)
names(xxx)[4] <- paste('aic',ncomp,sep='.')
names(xxx)[5] <- paste('aic',ncomp+ncincr,sep='.')
if(!is.null(scrps)) xxx[["screwUps"]] <- scrps
attr(xxx,"seed") <- seed
xxx
}
