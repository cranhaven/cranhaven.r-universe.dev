#' Distance matrix computation
#'
#' It computes the Euclidean distance matrix for a set of coordinates.
#' @param coords 2D spatial coordinates of dimensions \eqn{n\times 2}.
#' @return An \eqn{n\times n} distance matrix.
#' @author Katherine L. Valeriano, Alejandro Ordoñez, Christian E. Galarza, and Larissa A. Matos.
#' @examples
#' n = 100
#' set.seed(1000)
#' x = round(runif(n,0,10), 5)     # X coordinate
#' y = round(runif(n,0,10), 5)     # Y coordinate
#' Mdist = dist2Dmatrix(cbind(x, y))

dist2Dmatrix = function(coords){

  if (length(c(coords)) == 0) stop("coords must be specified")
  coords = as.matrix(coords)
  if (ncol(coords) != 2) stop("coords must contain 2 columns")
  if (!all(c(is.finite(coords)))) stop ("coords must contain only finite values")

  distancesM = crossdist(coords)
  out.ST = (distancesM + t(distancesM))/2
  return(out.ST)
}


#' Covariance matrix for spatial models
#'
#' It computes the spatial variance-covariance matrix considering exponential,
#' gaussian, matérn, or power exponential correlation function.
#'
#' @param phi spatial scaling parameter.
#' @param tau2 nugget effect parameter.
#' @param sig2 partial sill parameter.
#' @param coords 2D spatial coordinates of dimensions \eqn{n\times 2}.
#' @param type type of spatial correlation function: \code{'exponential'},
#' \code{'gaussian'}, \code{'matern'}, and \code{'pow.exp'} for exponential,
#' gaussian, matérn, and power exponential, respectively.
#' @param kappa parameter for some spatial correlation functions. For exponential
#' and gaussian \code{kappa=NULL}, for power exponential \code{0 < kappa <= 2},
#' and for matérn correlation function \code{kappa > 0}.
#'
#' @details The spatial covariance matrix is given by
#'
#' \eqn{\Sigma = [Cov(s_i, s_j )] = \sigma^2 R(\phi) + \tau^2 I_n},
#'
#' where \eqn{\sigma^2 > 0} is the partial sill, \eqn{\phi > 0} is the spatial scaling
#' parameter, \eqn{\tau^2 > 0} is known as the nugget effect in the geostatistical
#' framework, \eqn{R(\phi)} is the \eqn{n\times n} correlation matrix computed from a
#' correlation function, and \eqn{I_n} is the \eqn{n\times n} identity matrix.
#'
#' The spatial correlation functions available are:
#' \describe{
#'     \item{\strong{Exponential}:}{\eqn{Corr(d) = exp(-d/\phi)},}
#'
#'     \item{\strong{Gaussian}:}{\eqn{Corr(d) = exp(-(d/\phi)^2)},}
#'
#'     \item{\strong{Matérn}:}{\eqn{Corr(d) = \frac{1}{2^{(\kappa-1)}\Gamma(\kappa)}\left(\frac{d}{\phi}\right)^\kappa K_\kappa \left( \frac{d}{\phi} \right)},}
#'
#'     \item{\strong{Power exponential}:}{\eqn{Corr(d) = exp(-(d/\phi)^\kappa)},}
#'
#' where \eqn{d \geq 0} is the Euclidean distance between two observations,
#' \eqn{\Gamma(.)} is the gamma function, \eqn{\kappa} is the smoothness parameter,
#' and \eqn{K_\kappa(.)} is the modified Bessel function of the second kind of order
#' \eqn{\kappa}.}
#'
#' @return An \eqn{n\times n} spatial covariance matrix.
#'
#' @author Katherine L. Valeriano, Alejandro Ordoñez, Christian E. Galarza, and Larissa A. Matos.
#'
#' @seealso \code{\link{dist2Dmatrix}}, \code{\link{EM.sclm}}, \code{\link{MCEM.sclm}}, \code{\link{SAEM.sclm}}
#'
#' @examples
#' set.seed(1000)
#' n = 20
#' coords = round(matrix(runif(2*n, 0, 10), n, 2), 5)
#' Cov = CovMat(phi=5, tau2=0.8, sig2=2, coords=coords, type="exponential")

CovMat = function(phi, tau2, sig2, coords, type="exponential", kappa=NULL){

  if (length(c(phi))>1 | !is.numeric(phi)) stop("phi must be specified")
  if (phi <= 0) stop("The spatial parameter (phi) must be non-negative")

  if (length(c(tau2))>1 | !is.numeric(tau2)) stop("tau2 must be specified")
  if (tau2 <= 0) stop("The nugget effect (tau2) must be non-negative")

  if (length(c(sig2))>1 | !is.numeric(sig2)) stop("sig2 must be specified")
  if (sig2 <= 0) stop("The partial sill (sig2) must be non-negative")

  if (length(c(coords)) == 0) stop("coords must be specified")
  coords = as.matrix(coords)
  if (ncol(coords) != 2) stop("coords must contain 2 columns")
  if (!all(c(is.finite(coords)))) stop ("coords must contain only finite values")

  if (is.null(type)) stop("type must be specified")
  if (type!="matern" & type !="gaussian" & type != "pow.exp" & type != "exponential"){
    stop("type should be one of matern, gaussian, pow.exp, exponential")}

  if (type!="exponential" & type!="gaussian"){
    if (is.null(kappa)) stop("kappa mut be provided for pow.exp and matern")
    if (length(c(kappa))>1 | !is.numeric(kappa)) stop("kappa must be specified")
    if (type=="pow.exp" & (kappa > 2| kappa<=0)) stop("kappa must be a real in (0,2]")
    if (type=="matern" & kappa <= 0) stop("kappa must be a real number in (0,Inf)")
    if (type=="matern" & is.infinite(kappa)) stop("kappa must be a real number in (0,Inf)")
  } else { kappa = 0 }

  dist = dist2Dmatrix(coords)
  Covariance = varianceMat(phi, tau2, sig2, kappa, dist, type)$Sigma
  out.ST = (Covariance + t(Covariance))/2
  return(out.ST)
}


#' Prediction in spatial models with censored/missing responses
#'
#' It performs spatial prediction in a set of new \code{S} spatial locations.
#'
#' @param object object of class \code{'sclm'} given as output of \code{\link{EM.sclm}},
#' \code{\link{MCEM.sclm}}, or \code{\link{SAEM.sclm}} function.
#' @param locPre matrix of coordinates for which prediction is performed.
#' @param xPre matrix of covariates for which prediction is performed.
#' @param ... further arguments passed to or from other methods.
#'
#' @details This function predicts using the mean squared error (MSE) criterion, which
#' takes the conditional expectation E(Y|X) as the best linear predictor.
#'
#' @return The function returns a list with:
#' \item{coord}{matrix of coordinates.}
#' \item{predValues}{predicted values.}
#' \item{sdPred}{predicted standard deviations.}
#'
#' @author Katherine L. Valeriano, Alejandro Ordoñez, Christian E. Galarza, and Larissa A. Matos.
#'
#' @seealso \code{\link{EM.sclm}}, \code{\link{MCEM.sclm}}, \code{\link{SAEM.sclm}}
#'
#' @examples \donttest{set.seed(1000)
#' n = 120
#' coords = round(matrix(runif(2*n,0,15),n,2), 5)
#' x = cbind(rbinom(n,1,0.50), rnorm(n), rnorm(n))
#' data = rCensSp(c(1,4,-1), 2, 3, 0.50, x, coords, "left", 0.10, 20)
#'
#' ## Estimation
#' data1 = data$Data
#'
#' # Estimation: EM algorithm
#' fit1 = EM.sclm(y=data1$y, x=data1$x, ci=data1$ci, lcl=data1$lcl,
#'                ucl=data1$ucl, coords=data1$coords, phi0=2.50, nugget0=1)
#'
#' # Estimation: SAEM algorithm
#' fit2 = SAEM.sclm(y=data1$y, x=data1$x, ci=data1$ci, lcl=data1$lcl,
#'                  ucl=data1$ucl, coords=data1$coords, phi0=2.50, nugget0=1)
#'
#' # Estimation: MCEM algorithm
#' fit3 = MCEM.sclm(y=data1$y, x=data1$x, ci=data1$ci, lcl=data1$lcl,
#'                  ucl=data1$ucl, coords=data1$coords, phi0=2.50, nugget0=1,
#'                  MaxIter=300)
#' cbind(fit1$theta, fit2$theta, fit3$theta)
#'
#' # Prediction
#' data2 = data$TestData
#' pred1 = predict(fit1, data2$coords, data2$x)
#' pred2 = predict(fit2, data2$coords, data2$x)
#' pred3 = predict(fit3, data2$coords, data2$x)
#'
#' # Cross-validation
#' mean((data2$y - pred1$predValues)^2)
#' mean((data2$y - pred2$predValues)^2)
#' mean((data2$y - pred3$predValues)^2)}
#' @export
predict.sclm = function(object, locPre, xPre, ...){

  if (is.null(object)) stop("object must be specified")

  if (!is.null(locPre) & !is.null(xPre)){
    locPre = as.matrix(locPre)
    xPre = as.matrix(xPre)
    if (!all(c(is.finite(locPre)))) stop("locPre must contain only finite values")
    if (!all(c(is.finite(xPre)))) stop("xPre must contain only finite values")
    if(ncol(xPre)!=length(c(object$beta))) stop("Non-conformable dimensions between xPred and beta")
    if (nrow(locPre)!=nrow(xPre) | ncol(locPre)!=2) stop("Non-conformable dimensions between locPre and xPre")
  } else { stop("locPre and xPre must be specified") }

  ypred = predict.new(object, xPre, locPre)
  out.ST = ypred

  return(out.ST)
}


#' @export
summary.sclm = function(object, ...){
  cat('----------------------------------------------------------------\n')
  cat('           Censored Linear Spatial Regression Model   \n')
  cat('----------------------------------------------------------------\n')
  cat("Call:\n")
  print(object$call)
  cat('\nEstimated parameters:\n')
  print(object$tab)
  cat('\n')
  cat(paste('The effective range is',round(object$range,4),'units.\n'))
  cat('\nModel selection criteria:\n')
  print(object$critFin)
  cat('\nDetails:\n')
  cat('Number of censored/missing values:', object$ncens, '\n')
  cat('Convergence reached?:', (object$Iter < object$MaxIter), '\n')
  cat('Iterations:', object$Iter,'/',object$MaxIter, '\n')
  cat('Processing time:', round(object$time,4), units(object$time), '\n')
}


#' @export
print.sclm = function(x, ...){
  cat('----------------------------------------------------------------\n')
  cat('           Spatial Censored Linear Regression Model   \n')
  cat('----------------------------------------------------------------\n')
  cat("Call:\n")
  print(x$call)
  cat('\nEstimated parameters:\n')
  print(x$tab)
  cat('\n')
  cat(paste('The effective range is',round(x$range,4),'units.\n'))
  cat('\nModel selection criteria:\n')
  print(x$critFin)
  cat('\nDetails:\n')
  cat('Number of censored/missing values:', x$ncens,'\n')
  cat('Convergence reached?:',(x$Iter < x$MaxIter),'\n')
  cat('Iterations:',x$Iter,'/',x$MaxIter,'\n')
  cat('Processing time:',round(x$time,4),units(x$time),'\n')
}


#' @export
plot.sclm = function(x, ...){
  plot.convergence(x)
}
