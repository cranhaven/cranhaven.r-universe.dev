cband <- function(object,alpha=0.05,MC=FALSE,xlen=100,plot=FALSE,...) {
#
# Function cband.  To do the calculations to provide 100(1-alpha)%
# confidence bands and prediction bands for a mixture of single
# variable regressions.
#

if(!inherits(object,"mixreg")) {
    stop("Argument \"object\" must be of class \"mixreg\".\n") 
}

vars <- all.vars(object$formula)
if(length(vars) == 1) { # No predictors; scalar mixture.
   return(cint(object,alpha,MC,plot,...))
}

if(length(vars) > 2)
    stop("Can do conf. bands for single variable regression only.")

# The following can *probably* be done more simply (i.e. it can be
# done more simply if it is true that the *first* entry of all.vars(formula)
# is the name of the response, but I am choosing to err on the side of
# caution.
nmy <- as.character(object$formula[[2]]) # This gives the name of the response.
iy  <- match(nmy,vars) # Position of the response name.
nmx <- vars[-iy] # What's left over is the name of the (single) predictor.
x   <- object$data[[nmx]]
if(ncol(as.matrix(x)) > 1)
    stop("Can do conf. bands for single variable regression only.")
y   <- object$data[[nmy]]

theta  <- object$theta
K      <- length(theta)
intcpt <- object$intercept
eqVar  <- object$eqVar
covMat <- object$covMat
if(is.null(covMat)) {
    covMat <- if(MC) covMixMC(object,...) else covMix(object,...)
} else {
    if(!MC & attr(covMat,"MC")) {
        covMat <- covMix(object,useMC="no")
    } else if(MC & !attr(covMat,"MC")) {
         covMat <- covMixMC(object,...)
    }
}

alpha1 <- alpha
alpha2 <- alpha/2

xf <- if(intcpt) cbind(1,seq(min(x),max(x),length=xlen))
    else
           as.matrix(seq(min(x),max(x),length=xlen))
tv1 <- qnorm(1-alpha1)
tv2 <- qnorm(1-alpha2)

bnds <- vector("list",K)
for(k in 1:K) {
    beta <- theta[[k]]$beta
    if(eqVar) {
        ind  <- if(intcpt) 3*(k-1) + 1:2 else 2*(k-1) + 1
    } else {
        ind  <- if(intcpt) 4*(k-1) + 1:2 else 3*(k-1) + 1
    }
    yf    <- xf%*%beta
    vf    <- apply(xf*(xf%*%covMat[ind,ind]),1,sum)
    ucb1  <- yf + tv1*sqrt(vf)
    lcb1  <- yf - tv1*sqrt(vf)
    upb1  <- yf + tv1*sqrt(theta[[k]]$sigsq + vf)
    lpb1  <- yf - tv1*sqrt(theta[[k]]$sigsq + vf)
    ucb2  <- yf + tv2*sqrt(vf)
    lcb2  <- yf - tv2*sqrt(vf)
    upb2  <- yf + tv2*sqrt(theta[[k]]$sigsq + vf)
    lpb2  <- yf - tv2*sqrt(theta[[k]]$sigsq + vf)
    bnds[[k]] <- cbind(lcb1,ucb1,lcb2,ucb2,lpb1,upb1,lpb2,upb2)
}

if(intcpt) xf <- xf[,2] else xf <- as.vector(xf)
rslt <- list(theta=theta,intercept=intcpt,x=x,y=y,xf=xf,bnds=bnds,
             alpha=alpha,varnms=c(nmx=nmx,nmy=nmy))
class(rslt) <- "cband"
if(plot) {
    plot(rslt,...)
    return(invisible(rslt))
} else return(rslt)
}
