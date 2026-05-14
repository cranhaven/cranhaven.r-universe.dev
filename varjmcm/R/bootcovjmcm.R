#' Estimate the covariance of estimated parameters using a bootstrap based method
#'
#' \code{bootcovjmcm} gives the estimation of the covariance of estimated parameters returned by \code{jmcm} by using a bootstrap based method.
#'
#' @param object a fitted joint mean-covariance model of class "jmcmMod", returned by the function \code{jmcm}.
#' @param mydata the data frame used in fitting the model.
#' @param numboot the number of the bootstrap replications
#' @return an estimated covariance matrix of the estimated parameters.
#' @references [1] Liu, R.Y. (1988) "Bootstrap Procedure under Some Non-i.i.d. Models." Annals of Statistics, 16, 1696-1708.
#' @examples
#' cattleA <- cattle[cattle$group=='A', ]
#' fit.mcd <- jmcm(weight|id|I(ceiling(day/14+1))~1|1,
#'                data = cattleA,
#'                cov.method = "mcd",
#'                triple = c(1,1,1))
#' bootcovjmcm(fit.mcd, cattleA, 5)
#' ## Larger number of replications is needed to achieve accuracy,
#' ## however it may take hours.
#' \donttest{bootcovjmcm(fit.mcd, cattleA, 500)}
#' @import jmcm
#' @importFrom Matrix bdiag
#' @importFrom stats cov as.formula
#' @export
bootcovjmcm <- function(object,mydata,numboot){

  ## object is the fitted jmcm model
  ## numboot is the number to be bootstrap
  ## mydata is the original data

  if (missing(object)) stop("missing object.")
  if (missing(mydata)) stop("mydata can't be missing.")
  if (missing(numboot)) stop("Need to specify the number of replications.")
  if (class(object)[1]!="jmcmMod") stop("Object must be of class 'jmcmMod'.")

  SS <- as.list(x = c(1:length(object@args$m)))
  mean_X <- getJMCM(object,name = "X")
  mean_beta <- getJMCM(object,name = "beta")

  objectformula <- object@call$formula
  YYY <- as.character(objectformula[[2]][[2]][[2]])
  res <- mydata[YYY]-mean_X%*%mean_beta

  ## Get the block diagonal
  getSigma <- function(i){
    getJMCM(object,name = "Sigma",sub.num = i)
  }
  listsigma <- lapply(X = SS,getSigma)
  sqrtsigma <- lapply(X = listsigma,expm::sqrtm)
  bdsigma <- bdiag(sqrtsigma)

  ## Now standard the result by time Sigma^-1/2
  res <- as.matrix(res)
  stdres <- solve(bdsigma,res)
  ## stdres is the i.i.d. residual to be bootstrapped

  booted <- sapply(1:numboot, function(o) sample(x = stdres,
                                             size = length(stdres),
                                             replace = TRUE))
  bootedls <- as.data.frame(booted)
  ## bootedls is a data.frame that
  ## bootedls[[b]] contains the b-th bootstrapped iid residual

  ## now we need to time each [[b]] with the matrix
  trans_res<-function(Xdata){
    bdsigma%*%Xdata
  }

  tran <- lapply(X = bootedls,FUN = trans_res)
  ## tran is a list of transformed back residual that are
  ## ready to be added to Xb=Y

  Yboot <- function(res){
    mean_X%*%mean_beta+res
  }
  Ynew <- lapply(X = tran,FUN = Yboot)

  ####################################

  m2boot <- function(Yn2){
    mydata[as.character(objectformula[[2]][[2]][[2]])] <-
      as.numeric(Yn2)
    m2 <- jmcm(formula = as.formula(objectformula),
              data = mydata,
              cov.method = object@call$cov.method,
              triple = object@triple)
    getJMCM(m2,name = "theta")
  }

  #####################################

  betanew <- with(data = mydata,
                  lapply(X = Ynew,FUN = m2boot))

  betacov <- cov(t(as.data.frame(betanew)))

  betacov
}
