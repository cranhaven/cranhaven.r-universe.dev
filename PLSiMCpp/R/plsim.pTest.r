
#' @name plsim.pTest
#' 
#' @title Testing Parametric Components
#' 
#' @description  Test whether some elements of \eqn{\alpha} and \eqn{\beta} are zero, that is,
#' \deqn{H_0: \alpha_{i_1}=\cdots=\alpha_{i_k}=0 \ \mbox{ and } \beta_{j_1}=\cdots=\beta_{j_l}=0}
#' versus
#' \deqn{H_1: \mbox{not \ all }\ \alpha_{i_1},\cdots,\alpha_{i_k}  \ \mbox{ and } \beta_{j_1}, \cdots,\beta_{j_l} \ \mbox{ are equal to }\ 0.}
#' 
#' 
#' 
#' @param fit the result of function \link{plsim.est} or \link{plsim.vs.soft}.
#' @param parameterSelected select some coefficients for testing, default: NULL.
#' @param TargetMethod default: "plsim.est".
#' 
#' @return
#' A list with class "htest" containing the following components
#' \item{statistic}{the value of the test statistic.}
#' \item{parameter}{the degree of freedom for the test}
#' \item{p.value}{the p-value for the test}
#' \item{method}{a character string indicating what type of test was performed}
#' \item{data.name}{a character string giving the name of input}
#' 
#' @export
#' 
#' @examples
#' 
#' n = 50
#' sigma = 0.1
#'
#' alpha = matrix(1,2,1)
#' alpha = alpha/norm(alpha,"2")
#'
#' beta = matrix(4,1,1)
#'
#' x = matrix(1,n,1)
#' z = matrix(runif(n*2),n,2)
#' y = 4*((z%*%alpha-1/sqrt(2))^2) + x%*%beta + sigma*matrix(rnorm(n),n,1)
#' 
#' # Obtain parameters in PLSiM using Profile Least Squares Estimator
#' fit_plsimest = plsim.est(x, z, y)
#' 
#' # Test whether the parameters of parametric part estimated by plsimest
#' # are zero
#' res_pTest_plsimest = plsim.pTest(fit_plsimest)
#' 
#' # Test whether the second parameter of parametric part estimated by plsimest
#' # is zero
#' res_pTest_plsimest_ = plsim.pTest(fit_plsimest,parameterSelected = c(2))
#' 
#' # Obtain parameters in PLSiM using Penalized Profile Least Squares Estimator
#' # with lambda set as 0.01
#' fit_plsim = plsim.vs.soft(x,z,y,lambda = 0.01)
#' 
#' # Test whether the parameters of parametric part estimated by plsim are zero
#' res_pTest_plsim = plsim.pTest(fit_plsim,TargetMethod = "plsim")
#' 
#' # Test whether the second parameter of parametric part estimated by plsim is zero
#' res_pTest_plsim_ = plsim.pTest(fit_plsim,parameterSelected = c(2),TargetMethod = "plsim")
#' 
#' @references
#'
#' H. Liang, X. Liu, R. Li, C. L. Tsai. \emph{Estimation and testing for partially linear single-index models}.
#' Annals of statistics, 2010, 38(6): 3811.
#' 
#' 
#' 

plsim.pTest<-function(fit, parameterSelected=NULL, TargetMethod="plsimest")
{
  DNAME <- deparse(substitute(fit))
  METHOD <- "Test for parametric component"
  
  data <- fit$data

  x <- data$x
  y <- data$y
  z <- data$z

  h <- data$h
  lambda <- data$lambda
  zetaini <- data$zetaini
  MaxStep <- data$MaxStep
  flag <- data$SiMflag
  useSCAD <- data$penalty

  zeta <- fit$zeta
  mse <- fit$mse



  n <- nrow(z)
  dx <- ncol(x)
  dz <- ncol(z)

  Q_H1 <- mse*n

  if(flag) ids <- 1:(dx+dz) else ids <- 1:dz

  if(is.null(parameterSelected))
  {
    parameterSelected <- ids
  }
  else
  {
    if( !(max(parameterSelected)%in%ids && min(parameterSelected)%in%ids) )
    {
      stop('The variable index parameterSelected is out of boundary')
    }    
  }

  parameterRetained <- setdiff(ids,parameterSelected)

  m <- length(parameterSelected)
  names(m) <- "df"

  if(length(parameterRetained) == 0)
  {
    Q_H0 <- sum(y^2)
    T1 <- n*(Q_H0 - Q_H1)/Q_H1

    pvalue <- pchisq(T1,df=m,lower.tail=F)
    
    names(T1) <- "statistics"
    res <- list(statistic = T1, method = METHOD, p.value = pvalue,
                parameter = m, data.name = DNAME) 
    class(res) <- "htest"
    return(res)
  }


  xs <- as.matrix(x[,parameterRetained[parameterRetained>dz]-dz])
  zs <- as.matrix(z[,parameterRetained[parameterRetained<=dz]])

  if( ncol(xs) == 0)
  {
    xs <- NULL
  }

  if( ncol(zs) == 0)
  {
    eyep1 <- diag(c(matrix(1,ncol(xs)+1,1)))/n^2
    u <- cbind(matrix(1,n,1),xs)
    beta <- solve(t(u)%*%u+eyep1)%*%t(u)%*%y

    Q_H0 <- sum((y-u%*%beta)^2)
    T1 <- n*(Q_H0-Q_H1)/Q_H1

    pvalue <- pchisq(T1,df=m,lower.tail=F)
    
    names(T1) <- "statistics"
    res <- list(statistic = T1, method = METHOD, p.value = pvalue,
               parameter = m, data.name = DNAME) 
    class(res) <- "htest"
    return(res)
  }


  zetaini0 = as.matrix(zetaini[parameterRetained])
  
  if(TargetMethod == "plsim")
  {
    fit0 = plsim.vs.soft(xs, y, zs, h, zetaini0, lambda, NULL, MaxStep,useSCAD)
  }
  else if(TargetMethod == "plsimest")
  {
    fit0 = plsim.est(xs, zs, y, h, zetaini0, MaxStep)
  }
  else
  {
    stop("Please indicate an implemented method (plsim.est or plsim.vs.soft)")
  }
  
  Q_H0 = fit0$mse*n
  T1 = n*(Q_H0-Q_H1)/Q_H1
  names(T1) <- "statistics"
  
  pvalue = pchisq(T1,df=m,lower.tail=F)
  
  res = list(statistic = T1, method = METHOD, p.value = pvalue,
             parameter = m, data.name = DNAME) 
  class(res) <- "htest"
  return(res)
}

