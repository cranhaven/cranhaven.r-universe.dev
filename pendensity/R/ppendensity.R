#plotting the estimated density/densities, using the estimated density. Here, it's called 'obj'.
ppendensity <- function(x,val) {
  obj <- x
    weight <- obj$results$ck
    q <- obj$splines$q
    x.factor <- obj$values$covariate$x.factor
    help.env <- new.env()
    if(!is.null(x.factor)) {
      len.x.fac <- length(x.factor[1,])
      all.x <- 0
      all.x2 <- len.x.fac
    }
    max.bsp <-0
    max.kern <-0
    base <- obj$splines$base
    base.den <- obj$splines$base.den
    MeanW <- obj$splines$MeanW
    knots.spline <- obj$splines$knots.val$all
    knots.val <- obj$splines$knots.val
    Stand.abw <- obj$splines$Stand.abw
    help.degree <- obj$splines$help.degree
    K <- obj$splines$K
    N <- obj$splines$N
    Z <- obj$values$Z
    x <- obj$values$x
    m <- obj$splines$m
    Dm <- obj$splines$Dm
    h <- obj$splines$h
    eps <- 1e-4
    levels <- obj$values$covariate$levels
    how.combi <- obj$values$covariate$how.combi
    how.levels <- obj$values$covariate$how.levels
    
    lev1 <- c()
    for(i in 1:how.levels) lev1 <- c(lev1,as.numeric(as.vector(levels[[i]])))
    lev <- lev1[-1]
    if(base=="bspline") {
      h.help <- abs(knots.spline[1]-knots.spline[2])
      m <- length(knots.spline)
    }
    if(base=="gaussian") {
      h.help <- abs(MeanW[1]-MeanW[2])
      m <- length(MeanW)
    }

    y.list <- list()
    sum.list <- list()
    x.factor <- obj$values$covariate$x.factor
  if(is.null(obj$values$x)) {
    N <- obj$splines$N
    p <- obj$splines$p
    y <- obj$values$y
    base.den2 <- obj$splines$base.den2
    y.order <- order(obj$values$y)
    K <- length(base.den2[,1])-1
    weight <- c(obj$results$ck)
    row.help <- rep(0,length(base.den2[1,]))
    r.y<-range(obj$splines$knots.val$val)
    if(any(val<r.y[1])|any(val>r.y[2])) {
        #print("Any value of val outside of the range of the observed values, extended with 0 at the left and 1 at the right of the interval of observed values")
    ind<-which(val>=r.y[1]&val<=r.y[2])
    }
    else ind<-c(1:length(val))
    sum.add <- rep(0,length(val))
    if(length(val[ind])>0) {
      if(length(val[ind])>1) {
        base.den2<- my.bspline(h=obj$splines$h,q=obj$splines$q,knots.val=obj$splines$knots.val,y=val[ind],K=obj$splines$K-1,plot.bsp=FALSE)$base.den2
        for(k in 1:(obj$splines$K-1)) {
          sum.add[ind] <- obj$results$ck[k]*colSums(base.den2[(k:(obj$splines$K+1)),]) +sum.add[ind]
        }
      }
      if(length(val[ind])==1) {
        base.den2<- my.bspline(h=obj$splines$h,q=obj$splines$q,knots.val=obj$splines$knots.val,y=val[ind],K=obj$splines$K-1,plot.bsp=FALSE)$base.den2
        for(k in 1:(obj$splines$K-1)) {
          sum.add[ind] <- obj$results$ck[k]*sum(base.den2[(k:(obj$splines$K+1))]) +sum.add[ind]
        }
      }
    }
    if(any(val>r.y[2])) {
       ind2<-which(val>r.y[2])
       sum.add[ind2]<-1
    }
    return(sum.add)
}
}
