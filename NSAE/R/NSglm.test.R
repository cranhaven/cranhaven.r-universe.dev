#' Parametric bootstrap-based spatial nonstationarity test for generalized linear mixed model
#'
#' @description This function performs a parametric bootstrap-based test procudure for testing spatial nonstationarity in the data.
#'
#' @param formula an object of class list of formula, describe the model to be fitted
#' @param vardir a vector of sampling variances of direct estimators for each small area
#' @param Ni a vector of population size for each small area
#' @param ni a vector of sample size for each small area
#' @param lat a vector of latitude for each small area
#' @param lon a vector of longitude for each small area
#' @param method type of fitting method, default is "REML" method
#' @param maxit number of iterations allowed in the algorithm. Default is 100 iterations
#' @param precision convergence tolerance limit for the Fisher-scoring algorithm. Default value is 1e-04
#' @param data a data frame comprising the variables named in formula and vardir
#'
#' @return The function returns a list with class "htest" containing the following components:
#' \describe{
#'   \item{method}{a character string indicating what type of test was performed.}
#'   \item{p.value}{the p-value for the test.}
#'   \item{data.name}{a character string giving the name of the data.}
#'   }
#' @export NSglm.test
#'
#' @examples
#' # Load data set
#' data(headcount)
#' # Testing spatial nonstationarity of the data
#' result <- NSglm.test(y~x1, var, N,n,lat,long, "REML", 10, 1e-04, headcount[1:10,])
#' result
NSglm.test <- function(formula, vardir, Ni, ni, lat, lon, method = "REML", maxit = 100, precision = 1e-04, data){
  nameNi <- deparse(substitute(Ni))
  nameni <- deparse(substitute(ni))
  namevar <- deparse(substitute(vardir))
  namelat <- deparse(substitute(lat))
  namelon <- deparse(substitute(lon))
  if (!missing(data)) {
    formuladata <- model.frame(formula, na.action = na.omit,data)
    X <- model.matrix(formula, data)
    Ni <- data[, nameNi]
    ni <- data[, nameni]
    vardir <- data[, namevar]
    lat <- data[, namelat]
    lon <- data[, namelon]
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
  Method <- "Parametric bootstrap-based spatial nonstationarity test for proportion"
  y <- formuladata[, 1]
  m <- length(y)
  p <- dim(X)[2]
  dir <- y
  mod0 <- ebp(y~X-1, vardir, Ni, ni, method = "REML", maxit, precision = 1e-04)
  mod1 <- ebpNS(y~X-1, vardir, Ni, ni, lat, lon, method = "REML", maxit, precision = 1e-04)
  sigma2u<-try(mod0$fit$refvar,silent = TRUE)
  Beta.hat<-try(mod0$fit$estcoef$beta,silent = TRUE)
  lambda.stim= try(mod1$fit$lambda,silent = TRUE)
  sigma2.u.stim= try(mod1$fit$refvar,silent = TRUE)
  test.value<-2*(mod1$fit$loglike1-mod0$fit$loglike1)
  L0<-NULL
  La<-NULL
  test.value.boot<-NULL
  n.iter <- maxit
  for (ii in 1:n.iter) {
    u.boot<-rnorm(m,0,sqrt(sigma2u))
    eta.boot<-X%*%Beta.hat+u.boot
    y.boot=exp(eta.boot)/(1+exp(eta.boot))
    direct.boot=rbinom(m,ni,y.boot)
    pop = cbind(data,y.boot)
    mod1.boot=ebpNS(y.boot~X-1, vardir, Ni, ni, lat, lon, method = "REML",  maxit , precision = 1e-04)
    La[ii]<-mod1.boot$fit$loglike1
    mod0.boot=ebp(y.boot~X-1, vardir, Ni, ni, method = "REML", maxit , precision = 1e-04)
    L0[ii]<-mod0.boot$fit$loglike1
    test.value.boot[ii]<-2*(La[ii]-L0[ii])
  }
  pval<-(sum(test.value.boot>rep(test.value,n.iter)))/n.iter
  structure(list(method = Method, p.value = pval, data.name = names(formuladata)[1]),class = "htest")
}
