mixregEngine <- function(fmla,data,ncomp,eqVar,thetaStart,itmax,
                         eps,verb,digits,maxTry,seed,covMat,MC,cawl,
                         warn,...) {

# Count the predictors.
ncoef <- ncol(model.matrix(fmla,data=data))

# Determine whether the model has an intercept.
intercept <- as.logical(attr(terms(fmla),"intercept"))

# Get the name of the response.
nmy <- as.character(fmla[[2]])

# Get starting values; if these are not supplied they are
# basically generated at random.  For single-predictor models
# starting values could be found by applying the function visualFit()
# and extracting the "theta" component of the returned value.
if(is.null(thetaStart)) {
# Replace the name "ncomp" by "K" for brevity.
    K <- if(is.null(ncomp)) 2 else ncomp
    theta.old <- initRand(fmla,data,K,seed)
    seed      <- attr(theta.old,"seed")
    randStart <- TRUE
} else {
    if(inherits(thetaStart,"mixreg")) {
        whinge <- paste0("Argument \"thetaStart\" should not be of",
                         " class \"mixreg\".\n  It should be either the ",
                         "\"theta\" or the \"parmat\" component\n",
                         "  of an object of this class.\n")
        stop(whinge)
    }
    if(inherits(thetaStart,"matrix")) {
        if(ncol(thetaStart) != ncoef + 2) {
            whinge <- paste0("Argument \"thetaStart\" is a matrix ",
                             "with the wrong number of columns.\n")
            stop(whinge)
        }
        K <- nrow(thetaStart)
        xxx <- vector("list",K)
        for(k in 1:K) {
            xxx[[k]] <- list(beta=thetaStart[k,1:ncoef],
                             sigsq=thetaStart[k,ncoef+1],
                             lambda=thetaStart[k,ncoef+2])
        }
        thetaStart <- xxx
    } else {
        K <- length(thetaStart)
    }

# Check that the thetaStart values are sensible.
    lamsum <- sum(sapply(thetaStart,function(x){x$lambda}))
    if(!isTRUE(all.equal(1,lamsum)))
        stop("The \"lambda\" values must sum to 1.\n")
    if(!all(sapply(thetaStart,function(x){x$sigsq > 0})))
        stop("The \"sigsq\" values must be strictly positive.\n")
    theta.old <- thetaStart
    randStart <- FALSE
    seed      <- NA
}

# Check on the dimensions in theta.old.
dimsok <- all(unlist(sapply(theta.old,function(x){length(x$beta)}))==ncoef)
if(!dimsok) {
    whinge <- paste0("Starting values for beta are of wrong length for\n",
                     "  the number of predictors.\n")
    stop(whinge)
}

# Sort the initial parameter list according to the first regression
# coefficient, with the largest coefficient coming first.
tmp <- matrix(unlist(theta.old),byrow=TRUE,nrow=K)
j   <- if(intercept) 2 else 1
ind <- rev(order(tmp[,j]))
theta.old <- theta.old[ind]

# Iterate:
em.step <- 0
ntry    <- 1
theta   <- vector("list",K)
sigzero <- .Machine$double.eps
xm      <- model.matrix(fmla,data=data)
repeat {
    restart <- FALSE
    if(em.step==0) xxx <- gfun(xm,data[[nmy]],theta.old)
    em.step <- em.step + 1
    gma <- xxx$gamma
    lma <- apply(gma,2,mean)
    if(eqVar) sigsq <- 0
    sing <- FALSE
    for(k in 1:K) {
        nzw  <- sum(gma[,k] > sigzero)
        if(nzw > ncoef) {
            data[["waits"]] <- gma[,k]
            waits      <- NULL
            tmp        <- lm(fmla,weights=waits,data=data)
            ccc        <- coef(tmp)
            names(ccc) <- NULL
            yhat       <- fitted(tmp)
            vvv        <- sum(gma[,k]*(data[[nmy]]-yhat)**2)
            if(eqVar) {
                sigsq <- sigsq + vvv
                theta[[k]] <- list(beta=ccc,sigsq=NA,lambda=lma[k])
                next
            } else vvv  <- vvv/sum(gma[,k])
            if(vvv < sigzero) sing <- TRUE
        } else sing <- TRUE
        if(sing) {
            if(randStart) {
                if(ntry <= maxTry) {
                    restart <- TRUE
                    cat("Hit singularity in likelihood surface.\n")
                    cat("Trying a new random starting configuration.\n")
                    ntry <- ntry+1
                    em.step <- 0
                    theta.old <- initRand(fmla,data,K)
                    seed <- attr(theta.old,"seed")
                    break
                } else {
                    whinge <- paste0("Number of tries at finding",
                                     " feasible random starting",
                                     " values exceeded ",maxTry,".\n")
                    stop(whinge)
                }
            } else {
                whinge <- paste0("Hit singularity in likelihood surface.\n",
                                 "  Perhaps try different (random?) starting ",
                                 "values.\n")
                stop(whinge)
            }
        }
        if(restart) break
        theta[[k]] <- list(beta=ccc,sigsq=vvv,lambda=lma[k])
    }
    if(restart) next # Next in the repeat loop; note that em.step
                     # has been reset to 0 so we are starting over.
    if(eqVar) {
        sigsq <- sigsq/nrow(data)
        for(k in 1:K) theta[[k]]$sigsq <- sigsq
    }
    chnge  <- max(abs(unlist(theta)-unlist(theta.old)))
    old.ll <- xxx$log.like
    xxx    <- gfun(xm,data[[nmy]],theta)
    new.ll <- xxx$log.like
    if(verb) {
        cat(paste("     EM step ",em.step,":\n",sep=""))
        cat("     max abs. change in coef.: ",
        format(round(chnge,digits)),"\n",sep="")
        cat("     Old log like.: ",
        format(round(old.ll,digits)),"\n",sep="")
        cat("     New log like.: ",
        format(round(new.ll,digits)),"\n",sep="")
        cat("     Change in log like.: ",
        format(round(new.ll-old.ll,digits)),"\n",sep="")
    }
    if(chnge < eps) {
        converged <- TRUE
        break
    }
    if(em.step == itmax) {
        if(warn) warning("Failed to converge in ",itmax," EM steps.\n",sep="")
        converged  <- FALSE
        break
    }
    theta.old <- theta
}

# Sort theta.
xbar   <- apply(xm,2,mean)
Beta   <- matrix(unlist(theta),ncol=K)[1:ncol(xm),]
midval <- xbar%*%Beta
o      <- order(midval,decreasing=TRUE)
theta  <- theta[o]

# Wrap it up and quit:
ll     <- gfun(xm,data[[nmy]],theta)$log.like
M      <- K*(ncol(xm)-2) + (if(eqVar) 1 else K) + K-1
#         beta,            sigsq,                 lambda
aic    <- -2*ll + 2*M
parmat <- matrix(unlist(theta),byrow=TRUE,nrow=K)
bnms   <- paste0("beta",if(intercept) 0:(ncoef - 1) else 1:ncoef)
dimnames(parmat) <- list(1:K,c(bnms,"sigsq","lambda"))
data[["waits"]] <- NULL
rslt <- list(parmat=parmat,theta=theta,log.like=ll,aic=aic,
             intercept=intercept,eqVar=eqVar,nsteps=em.step,
             converged=converged,data=data,formula=fmla)
rslt$call <- cawl
class(rslt) <- "mixreg"
attr(rslt,"seed") <- seed
if(covMat) {
    if(MC) {
        covMat <- covMixMC(rslt,...)
    } else {
        covMat <- covMix(rslt,...)
    }
    rslt$covMat <- covMat
}
rslt
}
