#' Parametric bootstrap-based spatial nonstationarity test for Fay-Herroit model
#'
#' @description This function performs a parametric bootstrap-based test procudure for testing spatial nonstationarity in the data.
#'
#' @param formula an object of class list of formula, describe the model to be fitted
#' @param vardir a vector of sampling variances of direct estimators for each small area
#' @param lat a vector of latitude for each small area
#' @param long a vector of longitude for each small area
#' @param iter number of iterations allowed in the algorithm. Default is 100 iterations
#' @param data a data frame comprising the variables named in formula and vardir
#'
#' @return The function returns a list with class "htest" containing the following components:
#' \describe{
#'   \item{method}{a character string indicating what type of test was performed.}
#'   \item{p.value}{the p-value for the test.}
#'   \item{data.name}{a character string giving the name of the data.}
#'   }
#' @export NS.test
#'
#' @examples
#' # Load data set
#' data(paddysample)
#' # Testing spatial nonstationarity of the data
#' result <- NS.test(y ~ x1+x2, var, latitude, longitude, iter=50, data = paddysample[1:10,])
#' result
NS.test <- function (formula, vardir, lat,long, iter = 100, data) {
  namevar <- deparse(substitute(vardir))
  namelat <- deparse(substitute(lat))
  namelong <- deparse(substitute(long))
  if (!missing(data)) {
    formuladata <- model.frame(formula, na.action = na.omit,data)
    X <- model.matrix(formula, data)
    vardir <- data[, namevar]
    lat <- data[, namelat]
    long <- data[, namelong]
  }
  else {
    formuladata <- model.frame(formula, na.action = na.omit)
    X <- model.matrix(formula)
  }
  if (attr(attributes(formuladata)$terms, "response") == 1)
    textformula <- paste(formula[2], formula[1], formula[3])
  else textformula <- paste(formula[1], formula[2])
  if (length(na.action(formuladata)) > 0)
    stop("Argument formula=", textformula, " contains NA values.")
  if (any(is.na(vardir)))
    stop("Argument vardir=", namevar, " contains NA values.")
  y <- formuladata[, 1]
  m <- length(y)
  p <- dim(X)[2]
  direct <- y
  I<-diag(1,m)
  distance<-matrix(0,m,m)
  distance<-as.matrix(dist(cbind(as.vector(lat),as.vector(long))))
  W <- 1/(1+distance)
  W.sam <- W[1:m,1:m]
  Method <- "Parametric bootstrap-based spatial nonstationarity test"
  logl=function(delta){
        area=m
        psi=matrix(c(vardir),area,1)
        Y=matrix(c(direct),area,1)
        Z.area=diag(1,area)
        lambda<-delta[1]
        sigma.u<-delta[2]
        C<-lambda*diag(1,p)
        Cov<-(X%*%C%*%t(X))*W.sam+sigma.u*Z.area%*%t(Z.area)
        V<-Cov+I*psi[,1]
        Vi<-solve(V)
        Xt=t(X)
        XVi<-Xt%*%Vi
        Q<-solve(XVi%*%X)
        P<-Vi-(Vi%*%X%*%Q%*%XVi)
        b.s<-Q%*%XVi%*%Y
        ee=eigen(V)
        -(area/2)*log(2*pi)-0.5*sum(log(ee$value))-(0.5)*log(det(t(X)%*%Vi%*%X))-(0.5)*t(Y)%*%P%*%Y
      }
  grr=function(delta) {
      lambda<-delta[1]
      sigma.u<-delta[2]
      area=m
      psi=matrix(c(vardir),area,1)
      Y=matrix(c(direct),area,1)
      Z.area=diag(1,area)
      I<-diag(1,area)
      C<-lambda*diag(1,p)
      Cov<-(X%*%C%*%t(X))*W.sam+sigma.u*Z.area%*%t(Z.area)
      V<-Cov+I*psi[,1]
      Vi<-solve(V)
      Xt=t(X)
      XVi<-Xt%*%Vi
      Q<-solve(XVi%*%X)
      P<-Vi-(Vi%*%X%*%Q%*%XVi)
      derLambda<-(X%*%t(X))*W
      derSigmau<-Z.area%*%t(Z.area)
      s<-matrix(0,2,1)
      PG<-P%*%derLambda
      PU<-P%*%derSigmau
      Pdir<-P%*%Y
      s[1,1]<-((-0.5)*sum(diag(PG)))+((0.5)*(t(Y)%*%PG%*%Pdir))
      s[2,1]<-((-0.5)*sum(diag(PU)))+((0.5)*(t(Y)%*%PU%*%Pdir))
      c(s[1,1],s[2,1])
     }
  logl.boot=function(delta){
            area=m
            psi=matrix(c(vardir),area,1)
            Y=matrix(c(direct.boot),area,1)
            Z.area=diag(1,area)
            lambda<-delta[1]
            sigma.u<-delta[2]
            I<-diag(1,area)
            C<-lambda*diag(1,p)
            Cov<-(X%*%C%*%t(X))*W+sigma.u*Z.area%*%t(Z.area)
            V<-Cov+I*psi[,1]
            Vi<-solve(V)
            Xt=t(X)
            XVi<-Xt%*%Vi
            Q<-solve(XVi%*%X)
            P<-Vi-(Vi%*%X%*%Q%*%XVi)
            b.s<-Q%*%XVi%*%Y
            ee=eigen(V)
            -(area/2)*log(2*pi)-0.5*sum(log(ee$value))-(0.5)*log(det(t(X)%*%Vi%*%X))-(0.5)*t(Y)%*%P%*%Y
       }
  grr.boot=function(delta){
            lambda<-delta[1]
            sigma.u<-delta[2]
            area=m
            psi=matrix(vardir,area,1)
            Y=matrix(c(direct.boot),area,1)
            Z.area=diag(1,area)
            I<-diag(1,area)
            C<-lambda*diag(1,p)
            Cov<-(X%*%C%*%t(X))*W+sigma.u*Z.area%*%t(Z.area)
            V<-Cov+I*psi[,1]
            Vi<-solve(V)
            Xt=t(X)
            XVi<-Xt%*%Vi
            Q<-solve(XVi%*%X)
            P<-Vi-(Vi%*%X%*%Q%*%XVi)
            derLambda<-(X%*%t(X))*W
            derSigmau<-Z.area%*%t(Z.area)
            s<-matrix(0,2,1)
            PG<-P%*%derLambda
            PU<-P%*%derSigmau
            Pdir<-P%*%Y
            s[1,1]<-((-0.5)*sum(diag(PG)))+((0.5)*(t(Y)%*%PG%*%Pdir))
            s[2,1]<-((-0.5)*sum(diag(PU)))+((0.5)*(t(Y)%*%PU%*%Pdir))
            c(s[1,1],s[2,1])
     }
  loglfh=function(delta){
          area=m
          psi=matrix(c(vardir),area,1)
          Y=matrix(c(direct),area,1)
          Z.area=diag(1,area)
          sigma.u<-delta[1]
          I<-diag(1,area)
          V<-sigma.u*Z.area%*%t(Z.area)+I*psi[,1]
          Vi<-solve(V)
          Xt=t(X)
          XVi<-Xt%*%Vi
          Q<-solve(XVi%*%X)
          P<-Vi-(Vi%*%X%*%Q%*%XVi)
          b.s<-Q%*%XVi%*%Y
          ee=eigen(V)
          -(area/2)*log(2*pi)-0.5*sum(log(ee$value))-(0.5)*log(det(t(X)%*%Vi%*%X))-(0.5)*t(Y)%*%P%*%Y
       }
  loglfh.boot=function(delta){
              area=m
              psi=matrix(c(vardir),area,1)
              Y=matrix(c(direct.boot),area,1)
              Z.area=diag(1,area)
              sigma.u<-delta[1]
              I<-diag(1,area)
              V<-sigma.u*Z.area%*%t(Z.area)+I*psi[,1]
              Vi<-solve(V)
              Xt=t(X)
              XVi<-Xt%*%Vi
              Q<-solve(XVi%*%X)
              P<-Vi-(Vi%*%X%*%Q%*%XVi)
              b.s<-Q%*%XVi%*%Y
              ee=eigen(V)
              -(area/2)*log(2*pi)-0.5*sum(log(ee$value))-(0.5)*log(det(t(X)%*%Vi%*%X))-(0.5)*t(Y)%*%P%*%Y
      }
  fh=optimize(loglfh,c(0.001,100),maximum = TRUE)
  sigma2u<-fh$maximum
  V<-sigma2u*I%*%t(I)+I*vardir
  Vi<-solve(V)
  Q<-solve(t(X)%*%Vi%*%X)
  Beta.hat<-Q%*%t(X)%*%Vi%*%direct
  nsfh=constrOptim(c(0.1,0.2),logl,grr,method="Nelder-Mead",ui=rbind(c(1,0),c(0,1)),
                   ci=c(0.001,0),control=list(fnscale=-1))
  lambda.stim= nsfh$par[1]
  sigma2.u.stim= nsfh$par[2]
  test.value<-2*(nsfh$value-fh$objective)
  L0<-NULL
  La<-NULL
  test.value.boot<-NULL
  for (ii in 1:iter)
  {
    u.boot<-rnorm(m,0,sqrt(sigma2u))
    e.boot=rnorm(m,0,sqrt(vardir))
    theta.boot=X%*%Beta.hat+u.boot
    direct.boot=matrix(c(theta.boot+e.boot),m,1)
    nsfh.boot=constrOptim(c(lambda.stim,sigma2.u.stim),logl.boot,grr.boot,method="Nelder-Mead",
                          ui=rbind(c(1,0),c(0,1)),ci=c(0.001,0),control=list(fnscale=-1))
    La[ii]<-nsfh.boot$value
    fh.boot=optimize(loglfh.boot,c(0.001,100),maximum = TRUE)
    L0[ii]<-fh.boot$objective
    test.value.boot[ii]<-2*(La[ii]-L0[ii])
  }
  pval <-(sum(test.value.boot>rep(test.value,iter)))/iter
  structure(list(method = Method, p.value = pval, data.name = names(formuladata)[1]),class = "htest")
}
