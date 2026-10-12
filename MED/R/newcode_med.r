#Objective function which we need to minimize
#for the optimization problem
obj<- function(lam,u,ubar,Ti,rho,...){
  lam.t.u<- apply(u,1,crossprod,lam)
  #plot(lam.t.u)
  lam.t.ubar<- crossprod(lam,ubar)

  -mean(Ti*rho(lam.t.u, ...),na.rm = TRUE) + lam.t.ubar
}

#The first and Second derivative of the above objective function
derv.obj<- function(lam,Ti,rho1,u,ubar,...){
  lam.t.u <- apply(u,1,crossprod,lam)
  lam.t.ubar<- crossprod(lam,ubar)

  #get rho1(lam^Tu)*R/pi
  v<- numeric(length(Ti))
  v[Ti==1]<- rho1(lam.t.u, ...)[Ti==1]
  #get final answer
  -apply(apply(u,2,"*",v),2,mean,na.rm = TRUE)+ubar
}

derv2.obj<- function(lam,Ti,rho2,u,...){
  lam.t.u<- apply(u,1,crossprod,lam)

  #get rho(lam^Tu)
  v<- numeric(length(Ti))
  v[Ti==1]<- rho2(lam.t.u, ...)[Ti==1]

  #Get matrices for hessian
  mats<- sapply(1:nrow(u), function(i) tcrossprod(u[i,]),simplify = "array")
  mats2<- apply(mats,c(1,2),"*",v)
  #get final answer
  -apply(mats2,c(2,3),mean,na.rm = TRUE)
}

#The special case of CR family of functions

cr.rho<-function (v, theta)
{
  v<-as.vector(v)
  if (theta == 0) {
    ans <- -exp(-v)
  }
  else if (theta == -1){
    ans <- suppressWarnings(log(1+v))
    ind <- is.na(ans)
    ans[ind] <- -Inf
  }
  else  {
    a <- -(1 - theta * v)^(1 + 1/theta)
    ans <- a/(theta + 1)
    ind <- is.na(ans)
    ans[ind] <- -Inf
  }
  return(ans)
}


#The first and second derivatives of the CR family functions

d.cr.rho<-  function (v, theta)
{
  v<-as.vector(v)
  if (theta == 0) {
    ans <- exp(-v)
  }
  else if (theta == -1){
    ans <- 1/(1+v)
    ind <- is.na(ans)
    ans[ind] <- -Inf
  }
  else {
    ans <- (1 - theta * v)^(1/theta)
    ind <- is.na(ans)
    ans[ind] <- -Inf
  }
  return(ans)
}

dd.cr.rho<-   function (v, theta)
{
  v<-as.vector(v)
  if (theta == 0) {
    a <- -exp(-v)
  }
  else if (theta == -1){
    a <- -1/(1+v)^2
    ind <- is.na(a)
    a[ind] <- -Inf
  }
  else {
    a <- -(1 - theta * v)^(1/theta - 1)
    ind <- is.na(a)
    a[ind] <- -Inf
  }
  a
}

###################################################################################
#The backtracking function for the main newton Raphson
backtrack<- function(alpha,beta,x.val,del.x,nabla.f,obj,u, ubar,Ti,rho, ...){
  u<- u
  step<- 1

  l.t.u<- apply(u,1,crossprod,x.val)
  f.x<- obj(x.val,u,ubar, Ti, rho,...)

  df.t.dx<- crossprod(nabla.f, del.x)
  #print(f.x)
  #print(df.t.dx)

  while(obj(x.val+step*del.x, u,ubar,Ti,rho,...) > f.x + alpha*step*df.t.dx ){
    step<- beta*step
  }
  return(step)

}

###################################################################################



newton.r<-function (ini, X, Y, Ti, rho, rho1, rho2, FUNu, weight, max.iter = 100,
                    tol = 0.001, backtrack = TRUE,
                    bt.a = 0.3, bt.b = 0.5, verbose = TRUE, ...) {

  res <- matrix(ini, nrow = 1)
  N <- length(Y)
  umat <- t(apply(X, 1, FUNu))
  ubar <- apply( sweep(umat,1,weight,FUN="*") , 2,sum)
  dervs<- c(0)

  for (i in 2:max.iter) {
    x.val <- res[i - 1, ]
    objectiveValue<- obj(res[i-1,],umat,ubar,Ti,rho,...)

    if(objectiveValue <= -1e30){
      warning("The objective function is unbounded, a different rho() function
              might be needed.")
      lam.hat <- as.vector(tail(res, 1))
      l.t.u <- apply(umat, 1, crossprod, lam.hat)
      nom <- rep(0, N)
      nom[Ti == 1] <- rho1(l.t.u,...)[Ti == 1]/N
      weights <- nom
      return(list("res" = res, "weights" = weights, "conv" = FALSE))
    }
    del.f <- derv.obj(x.val, Ti, rho1, u = umat, ubar = ubar,...)
    H <- derv2.obj(x.val, Ti, rho2, u = umat,...)
    del.x <- -solve(H, del.f)
    nabla.f <- del.f
    dervs<- c(dervs,sum(nabla.f^2))

    step <- 1
    if (backtrack) {
      step <- backtrack(bt.a, bt.b, x.val, del.x, nabla.f,
                        obj, umat, ubar, Ti, rho,...)
    }
    if (verbose) {
      cat(paste("Iteration Number: ", i - 1, "\n"))
      cat(paste("Norm of Derivative: ", sum(nabla.f^2),
                "\n"))
      cat(paste("Objective value", objectiveValue , "\n"))
    }

    res <- rbind(res, x.val + step * del.x)

    if (sum(nabla.f^2) < tol) {
      lam.hat <- as.vector(tail(res, 1))
      l.t.u <- apply(umat, 1, crossprod, lam.hat)
      nom <- rep(0, N)
      nom[Ti == 1] <- rho1(l.t.u,...)[Ti == 1]/N
      weights <- nom
      return(list("res" = res, "weights" = weights, "conv" = TRUE))
    }
  }

  warning("The algorithm did not converge")
  lam.hat <- as.vector(tail(res, 1))
  l.t.u <- apply(umat, 1, crossprod, lam.hat)
  nom <- rep(0, N)
  nom[Ti == 1] <- rho1(l.t.u,...)[Ti == 1]/N
  weights <- nom
  return(list("res" = res, "weights" = weights, "conv" = FALSE))
}

####################

get.est.med<- function(ini1,ini2, ini3, M, X, Y, Ti, rho, rho1, rho2,
                          FUNu, PIE=FALSE, max.iter = 100,
                          tol = 1e-3, backtrack= TRUE, bt.a = 0.3, bt.b = 0.5,
                          verbose = TRUE, ...){

  N<- length(Y)
  MX<-cbind(M,X)
  if(length(ini1) != length(FUNu(X[1,]))){
    stop("Incorrect length of initial vector")
  }
  if(length(ini2) != length(FUNu(X[1,]))){
    stop("Incorrect length of initial vector")
  }
  if(length(ini3) != length(FUNu(MX[1,]))){
    stop("Incorrect length of initial vector")
  }

  if(verbose){
    cat("Fitting Newton Raphson for estimating Weights p: \n")
  }

  u.weight<-rep(1/N,N)

  p.hat<- newton.r(ini1, X, Y, Ti, rho, rho1, rho2, FUNu,
                   weight=u.weight, max.iter= max.iter, tol = tol,
                   backtrack = backtrack, bt.a = bt.a,
                   bt.b = bt.b,verbose = verbose, ...)

  if(verbose){
    cat("\nFitting Newton Raphson for estimating Weights q: \n")
  }

  q.hat<- newton.r(ini2, X, Y, 1-Ti, rho, rho1, rho2, FUNu,
                   weight=u.weight, max.iter = max.iter, tol = tol,
                   backtrack = backtrack, bt.a = bt.a,
                   bt.b = bt.b, verbose = verbose, ...)
  if(verbose){
    cat("\nFitting Newton Raphson for estimating Weights r: \n")
  }

  if(PIE){
    r.hat<- newton.r(ini3, MX, Y, 1-Ti, rho, rho1, rho2, FUNu,
                     weight=p.hat$weight, max.iter = max.iter, tol = tol,
                     backtrack = backtrack, bt.a = bt.a,
                     bt.b = bt.b, verbose = verbose, ...)
  }else
  {r.hat<- newton.r(ini3, MX, Y, Ti, rho, rho1, rho2, FUNu,
                   weight=q.hat$weight, max.iter = max.iter, tol = tol,
                   backtrack = backtrack, bt.a = bt.a,
                   bt.b = bt.b, verbose = verbose, ...)}

  Y_one <-  sum((p.hat$weights*Y)[Ti==1])
  Y_zero<- sum((q.hat$weights*Y)[Ti==0])
  if(PIE){Y_med <- sum((r.hat$weights*Y)[Ti==0])}else{Y_med <- sum((r.hat$weights*Y)[Ti==1])}
  tau.hat<-  Y_one - Y_zero
  nie.hat <- Y_one - Y_med
  nde.hat <- Y_med - Y_zero

  w.p<- p.hat$weights
  w.q<- q.hat$weights
  w.r<- r.hat$weights

  lam.p<- tail(p.hat$res,1)
  lam.q<- tail(q.hat$res,1)
  lam.r<- tail(r.hat$res,1)

  conv = TRUE
  if(!p.hat$conv | !q.hat$conv){
    warning("Newton Raphson did not converge for atleast one objective function.")
    conv<- FALSE
    #tau.hat<- NaN
  }

  res.l<- list("lam.p" = lam.p, "lam.q" = lam.q, "lam.r" = lam.r, "weights.p" = w.p,
               "weights.q" = w.q, "weight.r"=w.r, "Y1" = Y_one, "Y0"= Y_zero, "Ymed" =Y_med, "tau" = tau.hat,
               "nie"=nie.hat, "nde"=nde.hat,"conv" = conv)
  return(res.l)
}

#####################


get.cov.MED<-function(obj,...){

  N<-length(obj$Y)
  X<-obj$X
  MX<-cbind(obj$M,obj$X)
  FUNu<-obj$FUNu
  umat<-t(apply(X,1,FUNu))
  vmat<-t(apply(MX,1,FUNu))
  lam.p<-obj$lam.p
  lam.q<-obj$lam.q
  lam.r<-obj$lam.r
  Y<-obj$Y
  Y1<-obj$Y1
  Y0<-obj$Y0
  Ymed<-obj$Ymed
  Ti<-obj$Ti
  K<-obj$K
  L<-obj$L
  rho1<-obj$rho1
  rho2<-obj$rho2
  PIE<-obj$PIE

  w1<-Ti*sapply(tcrossprod(umat,lam.p),rho2,...)
  w2<-(1-Ti)*sapply(tcrossprod(umat,lam.q),rho2,...)
  if (PIE){w3<-(1-Ti)*sapply(tcrossprod(vmat,lam.r),rho2,...)}else{w3<-Ti*sapply(tcrossprod(vmat,lam.r),rho2,...)}

  L11<-crossprod(crossprod(umat,w1*Y),solve(crossprod(umat,sweep(umat,1,w1,FUN="*"))))
  L22<-crossprod(crossprod(umat,w2*Y),solve(crossprod(umat,sweep(umat,1,w2,FUN="*"))))
  L33<-crossprod(crossprod(vmat,w3*Y),solve(crossprod(vmat,sweep(vmat,1,w3,FUN="*"))))
  if(PIE){L31<-L33%*%crossprod(crossprod(umat,sweep(vmat,1,w1,FUN="*")),solve(crossprod(umat,sweep(umat,1,w1,FUN="*"))))}
  else  {L32<-L33%*%crossprod(crossprod(umat,sweep(vmat,1,w2,FUN="*")),solve(crossprod(umat,sweep(umat,1,w2,FUN="*"))))}

  p1=Ti*sapply(tcrossprod(umat,lam.p),rho1,...)
  p2=(1-Ti)*sapply(tcrossprod(umat,lam.q),rho1,...)
  if(PIE){p3=(1-Ti)*sapply(tcrossprod(vmat,lam.r),rho1,...)}else{p3=Ti*sapply(tcrossprod(vmat,lam.r),rho1,...)}

  g1=sweep(umat,1,(p1-1),FUN="*")
  g2=sweep(umat,1,(p2-1),FUN="*")
  if (PIE){g3=sweep(vmat,1,(p3-p1),FUN="*")}else{g3=sweep(vmat,1,(p3-p2),FUN="*")}
  g4=p1*Y-Y1
  g5=p2*Y-Y0
  g6=p3*Y-Ymed

  g=cbind(g1,g2,g3,g4,g5,g6)
  Pk=crossprod(g)/N

  Lk=matrix(0,ncol=(2*K+L+3),nrow=3)

  Lk[1,1:K]=L11
  Lk[2,(K+1):(2*K)]=L22
  Lk[3,(2*K+1):(2*K+L)]=L33
  if(PIE){Lk[3,1:K]=L31}
  else{Lk[3,(K+1):(2*K)]=L32}
  Lk[1,ncol(Lk)-2]=-1
  Lk[2,ncol(Lk)-1]=-1
  Lk[3,ncol(Lk)]=-1

  A.var<-Lk%*%Pk%*%t(Lk)/N

  return(A.var)
}

######################

MED<- function(Y, Ti, M, X, theta=0,  verbose = FALSE,
               PIE=FALSE,max.iter = 100, tol = 1e-10,
               backtrack = TRUE, backtrack.alpha = 0.3, backtrack.beta = 0.5, ...){
  bt.a<- backtrack.alpha
  bt.b<- backtrack.beta

  if(is.vector(X)){
    X<- matrix(X, ncol = 1, nrow = length(X))
    warning("Data matrix 'X' is a vector, will be treated as n x 1 matrix")
  }
  if(is.vector(M)){
    M<- matrix(M, ncol = 1, nrow = length(M))
    warning("Data matrix 'M' is a vector, will be treated as n x 1 matrix")
  }
  if(nrow(X)!=length(Y)){
    stop("Dimensions of covariates and response do not match")
  }
  if(nrow(M)!=length(Y)){
    stop("Dimensions of mediators and response do not match")
  }

  J<- length(unique(Ti))

  if(!all(0:1 == sort(unique(Ti)))){
    stop("The treatment levels must be labelled 0, 1")
  }
  #  if(!is.function(rho) | !is.function(rho1) | !is.function(rho2) ){
  #    stop("rho, rho1, rho2 must be user defined functions.
  #Alternatively, users can use the CR family of functions included in the package.")
  #  }
  #  if(!is.function(FUNu)){
  #    stop("'FUNu' must be a user defined function." )
  #  }
  if(!is.numeric(theta)){
    stop("theta must be a real number")
  }

  K<- ncol(X)+1
  L<- K+ ncol(M)

  ini1<-numeric(K)
  ini2<-numeric(K)
  ini3<-numeric(L)
#  if(is.null(initial.values)){
#    if(ATT){
#      initial.values<- numeric(K)
#    }else{
#      initial.values<- matrix(0, ncol = K, nrow = J )
##    }
#  }

#  if(ATT & !is.vector(initial.values)){
#    stop("For ATT we only need one vector of initial values for newton raphson")
#  }
#  if(!ATT){
#    if( !is.matrix(initial.values) ){
#      stop("Initial values must be a matrix")
#    }
#    if(any(dim(initial.values) !=  c(J,K)) ){
#      stop("Matrix of initial values must have dimensions J x K.")
#    }
#  }

  rho<-function(x){cr.rho(x,theta=theta)}
  rho1<-function(x){d.cr.rho(x,theta=theta)}
  rho2<-function(x){dd.cr.rho(x,theta=theta)}

  FUNu<-  function(x) c(1,x)

  #Now we determine which category of the problem we are in
 # gp<- "simple"
#  if(ATT) gp<- "ATT"
#  if(J>2) gp<- "MT"

    est<- get.est.med(ini1, ini2, ini3, M, X, Y, Ti, rho, rho1, rho2,
                         FUNu, PIE, max.iter,
                         tol, backtrack, bt.a, bt.b,
                         verbose = verbose, ...)
  res<- est
  res$M<- M
  res$X<- X
  res$Y<- Y
  res$Ti<- Ti
  res$rho<- rho
  res$rho1<- rho1
  res$rho2<- rho2
  res$theta<- theta
  res$FUNu<- FUNu
  res$K<- K
  res$L<- L
  res$PIE<-PIE
  res$vcov<- get.cov.MED(res,...)
  res$call<- match.call()
    est<- c(res$Y1, res$Ymed, res$Y0, res$tau,res$nie,res$nde)
    if(PIE){names(est)<- c("E[Y(1,M(1))]","E[Y(0,M(1))]","E[Y(0,M(0))]","ATE", "PDE", "PIE")}
    else{names(est)<- c("E[Y(1,M(1))]","E[Y(1,M(0))]","E[Y(0,M(0))]","ATE", "NIE", "NDE")}
    res$est<- est
    res$Y1<- NULL
    res$Y0<- NULL
    res$tau<- NULL
    res$Ymed<-NULL
    res$nie<-NULL
    res$nde<NULL
    res$se<-c(sqrt(res$vcov[1,1]),sqrt(res$vcov[3,3]),sqrt(res$vcov[2,2]),sqrt(c(1,-1,0)%*%res$vcov%*%c(1,-1,0)),sqrt(c(1,0,-1)%*%res$vcov%*%c(1,0,-1)),sqrt(c(0,-1,1)%*%res$vcov%*%c(0,-1,1)))

  class(res)<- "MED"
  return(res)
}

print.MED<- function(x, ...){
  object<- x
    cat("Call:\n")
    print(object$call)
    cat("\nMediation analysis with a binary treatment.\n")
    cat("\nPoint Estimates:\n")
    print(object$est)
}

summary.MED<- function(object, ...){

  Ci.l<- object$est+object$se*qnorm(0.025)
  Ci.u<- object$est+object$se*qnorm(0.975)

  z.stat<- object$est/object$se
  p.values<- 2*pnorm(-abs(z.stat))
  coef<- cbind(Estimate = object$est,
               StdErr = object$se,
               "95%.Lower" = Ci.l,
               "95%.Upper" = Ci.u,
               Z.value = z.stat,
               p.value = p.values)


    res<- list(call = object$call, Estimate = coef, vcov = object$vcov,
               Conv = object$conv)


  class(res)<- "summary.ATE"
  return(res)

}

print.summary.MED <- function(x, ...){
  cat("Call:\n")
  print(x$call)
  cat("\n")
  printCoefmat(x$Estimate, P.values = TRUE, has.Pvalue=TRUE)
}

