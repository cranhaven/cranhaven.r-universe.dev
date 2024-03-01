residuals.mixreg <- function(object,std=FALSE,...) {
    fmla   <- object$formula
    noPred <- length(all.vars(fmla)) == 1
    intcpt <- as.logical(attr(terms(fmla),"intercept"))
    xm     <- model.matrix(fmla,data=object$data)
    if(noPred) {
        pnms <- "index"
        x    <- 1:nrow(xm)
    } else {
        pnms   <- if(intcpt) colnames(xm)[-1] else colnames(xm)
        x      <- if(intcpt) drop(xm[,-1]) else xm
    }
    nmy    <- all.vars(fmla)[1]
    y      <- object$data[[nmy]]

# Note that "vnms" consists of "reasonable" names for the response
# and all of the individual (vector) predictors, even if some the
# terms on the right hand side of "fmla" are matrices.
    vnms  <- c(nmy,pnms)
    theta <- object$theta
    K     <- length(theta)
    resid <- vector("list",K)
    fvals <- vector("list",K)
    gamma <- gfun(xm,y,theta)$gamma
    for(k in 1:K) {
        div <- if(std) {
            sqrt(hatfun(xm,gamma[,k])*theta[[k]]$sigsq)
        } else {
            1
        }
        fitz       <- drop(xm%*%theta[[k]]$beta)
        fvals[[k]] <- fitz
        resid[[k]] <- (y-fitz)/div
    }
    rslt <- list(resid=matrix(unlist(resid),ncol=K),
                 fvals=matrix(unlist(fvals),ncol=K),
                 gamma=gamma,x=x,y=y,vnms=vnms,noPred=noPred)
    class(rslt) <- "mixresid"
    rslt
}
