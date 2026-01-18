#' @importFrom KernSmooth dpih
#' @importFrom KernSmooth bkde2D
#' @export
fkde <- function(n=5, pm0=0.5, pI0=0.2, lambda=0.05,
                 gridsize=100,B=10000,T=10000){
    
    Z <- .SampleControlChart(n=n,pm0=pm0,pI0=pI0,
                             lambda=lambda, B=B, T=T);
    
    gridsize <- round(gridsize)
    stopifnot(gridsize>5)
    hx <- KernSmooth::dpih(Z[,1])
    hy <- KernSmooth::dpih(Z[,2])
    out <- KernSmooth::bkde2D(x=Z, bandwidth=c(1.5*hx,1.5*hy),
                              gridsize=c(gridsize,gridsize))
    
    structure(
        list(fhat = out$fhat,
             x1=out$x1,
             x2=out$x2,
             n=n, pm=pm0,pI=pI0,lambda=lambda,
             B=B, T=T,
             call = match.call()),
        class="ControlChart")  
}

.SampleControlChart <- function(n=10,pm0=0.5,pI0=0.316,
                                lambda=0.05,T,B){
    n <- round(n)
    T <- round(T)
    B <- round(B)
    stopifnot(n>3)
    stopifnot(pm0>0 && pm0<1)
    stopifnot(pI0>0 && pI0<1)
    stopifnot(lambda>0 && lambda<1)
    stopifnot(T>3)
    stopifnot(B>100)
    Md <- rep(0,B)
    D <- rep(0,B)
    res <- .Fortran(.F_simucc, as.integer(B), as.integer(T),
                    as.integer(n), as.double(pm0), as.double(pI0),
                    as.double(lambda), Md=as.double(Md),
                    D=as.double(D))
    cbind(res$Md, res$D)
}

print.ControlChart <- function(x,...){
    print(x$call)
    cat("\n  n=",x$n)
    cat("  B=",x$B)
    cat(",  T=",x$T,"\n")
    cat("  pm=",x$pm)
    cat(",  pI=",x$pI)
    cat(",  lambda=",x$lambda,"\n")
}

plot.ControlChart <- function(x,K,
                              xlab=expression(EWMA[Md]),
                              ylab=expression(EWMA[D]),...){
    if(missing(K)){
        contour(x=x$x1, y=x$x2, z=x$fhat,
                xlab=xlab,ylab=ylab,
                ...)
    }else{
        stopifnot(K>0)
        contour(x=x$x1, y=x$x2, z=x$fhat,
                levels=K,
                xlab=xlab,ylab=ylab,...)
    }
}

contour.ControlChart <- function(x,
                                 xlab=expression(EWMA[Md]),
                                 ylab=expression(EWMA[D]),
                                 ...){
    contour(x=x$x1, y=x$x2, z=x$fhat,
            xlab=xlab,ylab=ylab,
            ...)
}

persp.ControlChart <- function(x,
                               xlab=expression(EWMA[Md]),
                               ylab=expression(EWMA[D]),
                               ...){
    persp(x=x$x1, y=x$x2, z=x$fhat,
          xlab=xlab,ylab=ylab,
          ...)
}

ARL0 <- function(x,ARL0=370,gridsize=20){
    stopifnot(class(x)=="ControlChart")
    fmax <- max(x$fhat)
    gridsize <- round(gridsize)
    stopifnot(gridsize > 0)
    stopifnot(ARL0 > 0)

    x.range <- range(x$x1)
    y.range <- range(x$x2)
    nx <- length(x$x1)
    ny <- length(x$x2)

    res <- .Fortran(.F_arl0, K=as.double(ARL0),
                    as.double(x.range), as.integer(nx),
                    as.double(y.range), as.integer(ny),
                    as.double(x$fhat), as.integer(x$n),
                    as.double(x$pm), as.double(x$pI),
                    ARL=as.double(x$lambda))
    list(K=res$K, ARL=res$ARL)
}

ARL1 <- function(x,K,pm1,pI1){
    stopifnot(class(x)=="ControlChart")
    stopifnot(K > 0)

    if(missing(pm1)){
        pm1 <- x$pm
    }
    if(missing(pI1)){
        pI1 <- x$pI
    }
    stopifnot(pm1>0 && pm1<1)
    stopifnot(pI1>0 && pI1<1)

    x.range <- range(x$x1)
    y.range <- range(x$x2)
    nx <- length(x$x1)
    ny <- length(x$x2)
    
    
    res <- .Fortran(.F_arl1,
                    ARL = as.double(K), 
                    as.double(x.range), as.integer(nx),
                    as.double(y.range), as.integer(ny),
                    as.double(x$fhat), as.integer(x$n),
                    as.double(pm1), as.double(pI1),
                    as.double(x$lambda))
    res$ARL
}

