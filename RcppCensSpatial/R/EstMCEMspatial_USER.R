#' ML estimation of spatial censored linear models via the MCEM algorithm
#'
#' It fits the left, right, or interval spatial censored linear model using the Monte
#' Carlo EM (MCEM) algorithm. It provides estimates and standard errors of the
#' parameters and supports missing values on the dependent variable.
#'
#' @param y vector of responses of length \eqn{n}.
#' @param x design matrix of dimensions \eqn{n\times q}, where \eqn{q} is the number
#' of fixed effects, including the intercept.
#' @param ci vector of censoring indicators of length \eqn{n}. For each observation:
#' \code{1} if censored/missing, \code{0} otherwise.
#' @param lcl,ucl vectors of length \eqn{n} representing the lower and upper bounds
#' of the interval, which contains the true value of the censored observation. Default
#' \code{=NULL}, indicating no-censored data. For each observation: \code{lcl=-Inf} and
#' \code{ucl=c} (left censoring); \code{lcl=c} and \code{ucl=Inf} (right censoring); and
#' \code{lcl} and \code{ucl} must be finite for interval censoring. Moreover, missing data
#' could be defined by setting \code{lcl=-Inf} and \code{ucl=Inf}.
#' @param coords 2D spatial coordinates of dimensions \eqn{n\times 2}.
#' @param phi0 initial value for the spatial scaling parameter.
#' @param nugget0 initial value for the nugget effect parameter.
#' @param type type of spatial correlation function: \code{'exponential'},
#' \code{'gaussian'}, \code{'matern'}, and \code{'pow.exp'} for exponential, gaussian,
#' matérn, and power exponential, respectively.
#' @param kappa parameter for some spatial correlation functions. See \code{\link{CovMat}}.
#' @param lower,upper vectors of lower and upper bounds for the optimization method.
#' If unspecified, the default is \code{c(0.01,0.01)} for lower and \code{c(30,30)} for upper.
#' @param MaxIter maximum number of iterations for the MCEM algorithm. By default \code{=500}.
#' @param nMin initial sample size for Monte Carlo integration. By default \code{=20}.
#' @param nMax maximum sample size for Monte Carlo integration. By default \code{=5000}.
#' @param error maximum convergence error. By default \code{=1e-4}.
#' @param show_se logical. It indicates if the standard errors
#' should be estimated by default \code{=TRUE}.
#'
#' @details The spatial Gaussian model is given by
#'
#' \eqn{Y = X\beta + \xi},
#'
#' where \eqn{Y} is the \eqn{n\times 1} response vector, \eqn{X} is the \eqn{n\times q}
#' design matrix, \eqn{\beta} is the \eqn{q\times 1} vector of regression coefficients
#' to be estimated, and \eqn{\xi} is the error term. Which is normally distributed with
#' zero-mean and covariance matrix \eqn{\Sigma=\sigma^2 R(\phi) + \tau^2 I_n}. We assume
#' that \eqn{\Sigma} is non-singular and \eqn{X} has a full rank \insertCite{diggle2007springer}{RcppCensSpatial}.
#'
#' The estimation process is performed via the MCEM algorithm, initially proposed by
#' \insertCite{wei1990monte;textual}{RcppCensSpatial}. The Monte Carlo (MC) approximation starts
#' with a sample of size \code{nMin}; at each iteration, the sample size increases (\code{nMax-nMin})/\code{MaxIter},
#' and at the last iteration, the sample size is \code{nMax}. The random observations are sampled through the slice
#' sampling algorithm available in package \code{relliptical}.
#'
#' @note The MCEM final estimates correspond to the mean of the estimates obtained at each
#' iteration after deleting the half and applying a thinning of 3.
#'
#' To fit a regression model for non-censored data, just set \code{ci} as a vector of zeros.
#'
#' @return An object of class "sclm". Generic functions \code{print} and \code{summary}
#' have methods to show the results of the fit. The function \code{plot} can extract
#' convergence graphs for the parameter estimates.
#'
#' Specifically, the following components are returned:
#' \item{Theta}{estimated parameters in all iterations, \eqn{\theta = (\beta, \sigma^2, \phi, \tau^2)}.}
#' \item{theta}{final estimation of \eqn{\theta = (\beta, \sigma^2, \phi, \tau^2)}.}
#' \item{beta}{estimated \eqn{\beta}.}
#' \item{sigma2}{estimated \eqn{\sigma^2}.}
#' \item{phi}{estimated \eqn{\phi}.}
#' \item{tau2}{estimated \eqn{\tau^2}.}
#' \item{EY}{MC approximation of the first conditional moment.}
#' \item{EYY}{MC approximation of the second conditional moment.}
#' \item{SE}{vector of standard errors of \eqn{\theta = (\beta, \sigma^2, \phi, \tau^2)}.}
#' \item{InfMat}{observed information matrix.}
#' \item{loglik}{log-likelihood for the MCEM method.}
#' \item{AIC}{Akaike information criterion.}
#' \item{BIC}{Bayesian information criterion.}
#' \item{Iter}{number of iterations needed to converge.}
#' \item{time}{processing time.}
#' \item{call}{\code{RcppCensSpatial} call that produced the object.}
#' \item{tab}{table of estimates.}
#' \item{critFin}{selection criteria.}
#' \item{range}{effective range.}
#' \item{ncens}{number of censored/missing observations.}
#' \item{MaxIter}{maximum number of iterations for the MCEM algorithm.}
#'
#' @author Katherine L. Valeriano, Alejandro Ordoñez, Christian E. Galarza, and Larissa A. Matos.
#'
#' @seealso \code{\link{EM.sclm}}, \code{\link{SAEM.sclm}}, \code{\link{predict.sclm}}
#'
#' @examples
#' # Example 1: left censoring data
#' set.seed(1000)
#' n = 50   # Test with another values for n
#' coords = round(matrix(runif(2*n,0,15),n,2), 5)
#' x = cbind(rnorm(n), rnorm(n))
#' data = rCensSp(c(2,-1), 2, 3, 0.70, x, coords, "left", 0.08, 0, "matern", 1)
#'
#' fit = MCEM.sclm(y=data$y, x=x, ci=data$ci, lcl=data$lcl, ucl=data$ucl,
#'                 coords, phi0=2.50, nugget0=0.75, type="matern",
#'                 kappa=1, MaxIter=30, nMax=1000)
#' fit$tab
#' \donttest{
#' # Example 2: left censoring and missing data
#' yMiss = data$y
#' yMiss[20] = NA
#' ci = data$ci
#' ci[20] = 1
#' ucl = data$ucl
#' ucl[20] = Inf
#'
#' fit1 = MCEM.sclm(y=yMiss, x=x, ci=ci, lcl=data$lcl, ucl=ucl, coords,
#'                  phi0=2.50, nugget0=0.75, type="matern", kappa=1,
#'                  MaxIter=300, nMax=1000)
#' summary(fit1)
#' plot(fit1)}
#' @references \insertAllCited

MCEM.sclm = function(y, x, ci, lcl=NULL, ucl=NULL, coords, phi0, nugget0, type="exponential", kappa=NULL,
                     lower=c(0.01,0.01), upper=c(30,30), MaxIter=500, nMin=20, nMax=5000, error=1e-4, show_se=TRUE){
  n = length(c(y))
  if (!is.numeric(y)) stop("y must be a numeric vector")
  if (!is.numeric(x)) stop("x must be a numeric matrix")
  if (!is.matrix(x)) x = as.matrix(x)
  if (!is.matrix(y)) y = as.matrix(y)
  if (det(t(x)%*%x)==0) stop("the columns of x must be linearly independent")

  #No data
  if ((length(x) == 0) | (length(y) == 0) | (length(ci) == 0)) stop("All parameters must be provided")

  #Validating if exists NA's
  if (sum(ci%in%c(0,1)) < length(ci)) stop("The elements of the vector ci must be 0 or 1")
  if (sum(is.na(x)) > 0) stop("There are some NA values in x")
  if (!all(c(is.finite(x)))) stop("x must contain only finite values.")
  if (sum(is.na(ci)) > 0) stop("There are some NA values in ci")
  miss = which(is.na(y))
  if (sum(ci[miss]) != length(miss)) stop ("NA values in y must be specified through arguments ci, lcl, and ucl")
  if (!all(c(is.finite(coords)))) stop("coords must contain only finite values.")

  #Validating dims data set
  if (ncol(as.matrix(y)) > 1) stop("y must have just one column")
  if (ncol(as.matrix(ci)) > 1) stop("ci must have just one column")
  if (nrow(as.matrix(x)) != n) stop("x does not have the same number of lines than y")
  if (length(ci) != n) stop("ci does not have the same length than y")
  if (nrow(coords)!=n | ncol(coords)!=2) stop("Non-conformable dimensions between coords and y.")

  if (sum(ci) > 0){
    if (is.null(lcl) | is.null(ucl)) stop("lcl and ucl must be provided for censored data")
    if (!is.numeric(lcl) | !is.numeric(ucl)) stop("lcl and ucl must be numeric vectors")
    if (length(miss)>0){
      censor = (ci==1 & !is.na(y))
      if (any(is.infinite(lcl[censor])) & any(is.infinite(ucl[censor]))) stop("lcl or ucl must be finite for censored data")
    } else {
      if (any(is.infinite(lcl[ci==1])) & any(is.infinite(ucl[ci==1]))) stop("lcl or ucl must be finite for censored data")
    }
    if (length(lcl) != n) stop("lcl does not have the same length than y")
    if (length(ucl) != n) stop("ucl does not have the same length than y")
    if (ncol(as.matrix(lcl)) > 1) stop("lcl must have just one column")
    if (ncol(as.matrix(ucl)) > 1) stop("ucl must have just one column")
    if (sum(is.na(lcl))>0 | sum(is.na(ucl))>0) stop("There are some NA values in lcl or ucl")
    if (!all(lcl[ci==1]<ucl[ci==1])) stop ("lcl must be smaller than ucl")
  }

  #Validating supports
  if (length(c(phi0))>1 | !is.numeric(phi0)) stop("Initial value for phi must be provided")
  if (phi0 <= 0) stop("phi0 must be non-negative")
  if (length(c(nugget0))>1 | !is.numeric(nugget0)) stop("Initial value for nugget effect must be provided")
  if (nugget0 <= 0) stop("nugget0 must be non-negative")
  if (is.null(type)) stop("type must be specified")
  if (type!="matern" & type!="gaussian" & type!="pow.exp" & type!="exponential"){
    stop("type should be one of matern, gaussian, pow.exp or exponential")}
  if (type!="exponential" & type!="gaussian"){
    if (is.null(kappa)) stop("kappa must be provided")
    if (length(c(kappa))>1 | !is.numeric(kappa)) stop("kappa must be specified")
    if (type=="pow.exp" & (kappa>2| kappa<=0)) stop("kappa must be a real in (0,2]")
    if (type=="matern" & kappa<=0) stop("kappa must be a real number in (0,Inf)")
    if (type=="matern" & is.infinite(kappa)) stop("kappa must be a real number in (0,Inf)")
  } else { kappa=0 }
  if (length(c(lower))!=2 | length(c(upper))!=2) stop("lower and upper must be vectors of length 2")
  if (any(is.na(lower)) | !is.numeric(lower)) stop("lower is not specified or contains NA")
  if (any(is.na(upper)) | !is.numeric(upper)) stop("upper is not specified or contains NA")
  if (any(lower>=upper)) stop("lower must be smaller than upper")
  if (any(lower<=0)) stop("Values in lower must be non-negative")
  if (!all(is.finite(upper))) stop("upper must contain only finite values")
  if (length(c(MaxIter))>1 | !is.numeric(MaxIter)) stop("MaxIter must be a positive integer value")
  if (MaxIter<=0 | MaxIter%%1!=0) stop("MaxIter must be a positive integer value")
  if (length(c(nMin))>1 | !is.numeric(nMin)) stop("nMin must be a positive integer")
  if (nMin<=1 | nMin%%1!=0) stop("nMin must be a positive integer greater than 1")
  if (length(c(nMax))>1 | !is.numeric(nMax)) stop("nMax must be a positive integer")
  if (nMax<=0 | nMax%%1!=0) stop("nMax must be a positive integer")
  if (nMin > nMax) stop("nMax must be greater than or equal to nMin")
  if (length(c(error))>1 | !is.numeric(error)) stop("error must be specified")
  if (error <= 0) stop("error must be a positive value (suggested to be small)")
  if (!is.logical(show_se)) stop("show_se must be logical (TRUE/FALSE).")

  ci = as.matrix(ci)
  lcl = as.matrix(lcl)
  ucl = as.matrix(ucl)
  coords = as.matrix(coords)

  #---------------------------------------------------------------------#
  #                                Outputs                              #
  #---------------------------------------------------------------------#

  out.Sp = MCEM_Spatial(y, x, ci, lcl, ucl, coords, phi0, nugget0, type,
                        kappa, lower, upper, MaxIter, nMin, nMax, error, show_se)
  # Estimates
  q = ncol(x)
  lab = numeric(q + 3)
  if (sum(abs(x[,1])) == nrow(x)){ for (i in 1:q) lab[i] = paste('beta',i-1,sep='')
  } else { for (i in 1:q) lab[i] = paste('beta',i,sep='') }
  lab[q+1] = 'sigma2';  lab[q+2] = 'phi';  lab[q+3] = 'tau2'

  if (show_se) {
    tab = round(rbind(c(out.Sp$theta), c(out.Sp$SE)), 4)
    colnames(tab) = lab
    rownames(tab) = c("","s.e.")
  } else {
    tab = round(rbind(c(out.Sp$theta)), 4)
    colnames(tab) = lab
    rownames(tab) = c("")
  }
  # Information criteria
  critFin = c(out.Sp$loglik, out.Sp$AIC, out.Sp$BIC)
  critFin = round(t(as.matrix(critFin)), digits=3)
  dimnames(critFin) = list(c("Value"), c("Loglik", "AIC", "BIC"))

  out.Sp$call = match.call()
  out.Sp$tab = tab
  out.Sp$critFin = critFin
  out.Sp$range = Effective.range(0.05, out.Sp$phi, kappa, type)
  out.Sp$ncens = sum(ci)
  out.Sp$MaxIter = MaxIter

  class(out.Sp) <- "sclm"
  return(out.Sp)
}
