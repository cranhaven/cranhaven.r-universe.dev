#plotting the estimated density/densities, using the estimated density. Here, it's called 'obj'.
dpendensity <- function(x,val) {
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
    h <- obj$splines$h
    sort <- obj$values$sort
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

    max.bw.all <- c()
    
    if (is.null(obj$values$x)) {
      y.val <- val
      ind0<-NULL
      den.val<-rep(0,length(val))
      cond <- (y.val>=min(knots.val$val) & y.val <=max(knots.val$val))
      if(any(cond==FALSE)) {
        ind0<-which(y.val<min(knots.val$val)|y.val>max(knots.val$val))
        y.val<-y.val[-ind0]
      }
      if(base=="bspline"&(length(y.val)>0)) {
        if(q>2) K.help <- K-q+2 else K.help <- 0
        help.base.y.val <- my.bspline(h,q,knots.val,y.val,K.help,plot.bsp=FALSE)$base.den
        assign("base1.y.val",help.base.y.val,help.env)
        assign("y.help1.val",y.val,help.env)     
      }
      if(base=="gaussian") {
        y.r <- range(knots.val$val)
        for(i in 1:m) {
          if(MeanW[i] >=y.r[1] & MeanW[i]<=y.r[2]) list <- c(list,i)
        }
        nn <- matrix(1:length(y.val))
        help.base.y.val <- apply(nn,1,function(i,MeanW,Stand.abw,y.val) dnorm(y.val[i],MeanW,Stand.abw),MeanW,Stand.abw,y.val)[list,]
        assign("y.help1.val",y.val,help.env)
        assign("base1.y.val",help.base.y.val,help.env)
        }
      }   
      assign("weight1",weight,envir=help.env)
      if(length(ind0)==length(y.val)) return(den.val) 
      if((length(ind0)!=length(val))&length(ind0)>=1) den.val[-ind0] <- get("weight1",envir=help.env)%*%get("base1.y.val",envir=help.env)
      if(is.null(ind0)) den.val <- get("weight1",envir=help.env)%*%get("base1.y.val",envir=help.env)
      return(den.val)
    }
