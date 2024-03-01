visualFit <- function(fmla,data=NULL,ncomp,eqVar=FALSE,chsnPts=NULL,
                      keepPlotVisible=FALSE) {
if(is.null(chsnPts)) {
    if(!interactive())
        stop("When \"chsnPts\" is not supplied, the session must be interactive.\n")
} else {
    if(!missing(ncomp)) {
        if(ncomp != length(chsnPts)) {
            whinge <- paste("The specified \"ncomp\" is incompatible with the",
                            " specified \"chsnPts\".\n")
            stop(whinge)
        }
    } else ncomp <- length(chsnPts)
}

vnms <- all.vars(fmla)
if(length(vnms) > 2)
    stop("Visual fitting is feasible only for one-variable regression.\n")
nmx  <- vnms[2]
nmy  <- vnms[1]
if(is.null(data[[nmx]])) data[[nmx]] <- get(nmx)
if(is.null(data[[nmy]])) data[[nmy]] <- get(nmy)
if(ncol(as.matrix(data[[nmx]])) != 1)
    stop("Visual fitting is feasible only for one-variable regression.\n")
data      <- as.data.frame(data[c(nmx,nmy)])
intercept <- as.logical(attr(terms(fmla),"intercept"))
if(is.null(chsnPts)) {
    plot(fmla,data=data)
    mess <- paste0("\"Visually\" determine ",ncomp," components:\n")
    cat(mess)
    pears <- vector("list",ncomp)
    if(intercept) {
        for(i in 1:ncomp) {
            cat("Left click on TWO points that you judge to lie on\n")
            mess <- paste0("Component ",i,":\n")
            cat(mess)
            pears[[i]] <- locator(2,type="o",col="red",pch=20)
        }
    } else {
        for(i in 1:ncomp) {
            cat("Left click on ONE point that you judge to lie on\n")
            mess <- paste0("Component ",i,".  (The \"other\" point needed ",
                           "is taken to be the origin.)\n")
            cat(mess)
            pears[[i]] <- locator(1,type="o",col="red",pch=20)
            pears[[i]][["x"]] <- c(0,pears[[i]][["x"]])
            pears[[i]][["y"]] <- c(0,pears[[i]][["y"]])
            lines(pears[[i]],col="red")
        }
    }
    if(!keepPlotVisible) dev.off()
} else {
    nc.inferred <- length(chsnPts)
    pears <- chsnPts
}

x  <- data[[nmx]]
y  <- data[[nmy]]
n  <- nrow(data)
dM <- matrix(nrow=n,ncol=ncomp)
for(j in 1:ncomp) {
    ptb <- with(pears[[j]],c(x[1],y[1]))
    ptc <- with(pears[[j]],c(x[2],y[2]))
    for(i in 1:n) {
        dM[i,j] <- dist2d(a=c(x[i],y[i]),b=ptb,c=ptc)
    }
}
gps   <- factor(apply(dM,1,which.min))
theta <- vector("list",ncomp)
lll   <- 1/ncomp
if(eqVar) {
    fit   <- if(intercept) lm(y~(gps+0)/x) else lm(y ~ (gps+0):x)
    sigsq <- summary(fit)$sigma^2
    ccc   <- coef(fit)
    for(j in 1:ncomp) {
	cft <- if(intercept) c(ccc[j],ccc[ncomp+j]) else ccc[j]
	theta[[j]] <- list(beta=cft,sigsq=sigsq,lambda=lll)
    }
} else {
    ddd <- data.frame(x=x,y=y)
    sdd <- split(ddd,f=gps)
    for(j in 1:ncomp) {
        X  <- sdd[[j]]
        fX <- if(intercept) lm(y ~ x,data=X) else lm(y ~ x - 1,data=X)
        theta[[j]] <- list(beta=coef(fX),sigsq=summary(fX)$sigma^2,lambda=lll)
   }
}
xm     <- model.matrix(fmla,data=data)
ll     <- gfun(xm,y,theta)$log.like
M      <- ncomp*(ncol(xm)-2) + (if(eqVar) 1 else ncomp) + ncomp-1
          # beta, sigsq, lambda
aic    <- -2*ll + 2*M
parmat <- matrix(unlist(theta),byrow=TRUE,nrow=ncomp)
bnms   <- paste0("beta",if(intercept) 0:1 else 1)
dimnames(parmat) <- list(1:ncomp,c(bnms,"sigsq","lambda"))
data[["waits"]] <- NULL
rslt   <- cbind(data,groups=gps)
rslt <- list(parmat=parmat,theta=theta,log.like=ll,aic=aic,
             intercept=intercept,eqVar=TRUE,nsteps=NA,
             converged=NA,data=data,formula=fmla)
class(rslt) <- c("mixreg","visualEstimate")
attr(rslt,"chsnPts") <- pears

rslt
}
