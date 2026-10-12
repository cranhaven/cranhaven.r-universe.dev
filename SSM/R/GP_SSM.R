#' Compute the unscaled covariance matrix.
#'
#' Computes the unscaled covariance matrix for an SSM object for a given
#' length parameter.
#'
#' The covariance matrix uses the correlation function specified by the SSM slot
#' \code{type}.  \code{"matern32"} uses a Matern 3/2 correlation while
#' \code{"exp"} uses a squared exponential.
#' @param ssm An SSM object.
#' @param r A number. The length parameter used by the correlation function.
#' @keywords internal
#' @return The unscaled covariance matrix.
compute.covariance <- function(ssm, r){
  # This function computes the covariance matrix for the data (but does not
  # include the sigma parameter). Computes squared exponential if matern32
  # not specified.
  ## arguments:
  #        r:                A scaling parameter
  #        ssm:              The ssm object
  if (r == 0) return (diag(1, ssm@design_size))
  if (ssm@type == "matern32") return (exp(- ssm@distance * sqrt(3) * r) *
                                        (1 + ssm@distance * sqrt(3) * r))
  #  "ssm" type not fully implemented.
  #  if (ssm@type == "ssm"){
  #    adjusted_smoothness <- ssm@local_smoothness %*% t(ssm@local_smoothness) *
  #    max(ssm@distance)^2 / max(ssm@local_smoothness)^2
  #    return(exp(-adjusted_smoothness * ssm@distance^2 * r))
  #  }
  return(exp(-ssm@distance^2 * r))
}

#' Compute the concentrated likelihood of a covariance matrix.
#'
#' Computes the concentrated likelihood of the covariance matrix of an SSM
#' object, given a length parameter and the SSM Leave-One-Out errors.
#' @param r A number. The length parameter of the correlation function.
#' @param ssm An SSM object.
#' @return A number. The concentrated likelihood.
#' @keywords internal
concentrated.likelihood <- function(r, ssm){
  # Computes the concentrated likelihood
  covariance.matrix <- compute.covariance(ssm, r)
  eRe <- tryCatch(solve(covariance.matrix,
                        ssm@residuals %*% t(ssm@residuals)),
                  error = function (e) Inf)
  if (eRe[1] == Inf){
    cat("Covariance inversion numerically unstable, increasing tolerance
        (results will be less accurate.\n")
    eRe <- tryCatch(solve(covariance.matrix,
                          ssm@residuals %*% t(ssm@residuals)),
                    error = function (e) Inf, tol = 1e-30)
  }
  if (eRe[1] == Inf){
    cat("Covariance inversion failed.\n")
    return(0) # If failed, set output to 0
  }
  eRe <- sum(diag(eRe))
  eRe^(-ssm@design_size) / det(covariance.matrix)
}

#' Optimize concentrated likelihood.
#'
#' Wrapper for optimizing \code{concentrated.likelihood}. Used by
#' \code{\link{estimate.GP}}.
#' @param int The maximum endpoint of the interval of optimization.
#' @param ... arguments to pass to the optimize call.
#' @keywords internal
optimize.by.interval.maximum <- function(int, ...)
  optimize(concentrated.likelihood, interval = c(0, int), ...)

#' Estimate the parameters of the metamodel error estimating GP.
#'
#' This function estimates the parameters of the metamodel error estimating
#' Gaussian process using maximum likelihood methods to identify the length
#' parameter \code{r} of the given correlation function.
#'
#' Since the concentrated likelihood function is often flat, this function calls
#' \code{optimize} several times using different search intervals to avoid cases
#' when the \code{optimize} algorithm misses maxima when the interval is too
#' large.
#'
#' The scaling parameter \code{sigma} is found analytically once \code{r}
#' has been determined.
#'
#' @param ssm An SSM object.
#' @param type Character. Specifies the correlation function to use. Either
#'   \code{"exp"} for squared exponential or \code{"Matern32"} for Matern 3/2.
#'   Anything else will result in the use of a squared exponential correlation
#'   function.#'
#' @return An SSM object that is the same as the input except with
#'   estimates for \code{r} and \code{sigma} in the appropriate slots.
#' @keywords internal
estimate.GP <- function(ssm, type){
  # finds maximum likelihood estimate for scale parameter r using concentrated
  # likelihood function
  # if specifed, type overrides the type already set for the ssm object
  if(!missing(type)) ssm@type = type
  cat("finding maximum likelihood estimates using \"", ssm@type,
      "\" correlation function.\n", sep = "")
  # if (ssm@type == "ssm"){
  #  if(length(ssm@local_smoothness) < 1){
  #    cat("Computing smoothness at each design point.\n")
  #    ssm@local_smoothness <- smoothness.over.design(ssm)
  #  }
  #}

  if(is.na(match(ssm@type, c("ssm", "exp", "matern32")))){
    ssm@type <- "exp"
    cat("Specified correlation function is not implemented.
        Computing squared exponential covariance instead.\n")
  }
  ssm   <- compute.residuals(ssm)
  # Likelihoods are often flat and global maxima can be missed by the algorithm
  # Therefore, we use several intervals and select the best result
  r_list <- c(1,10,100,1000,10000)
  r <- sapply(r_list, optimize.by.interval.maximum, ssm = ssm, maximum = TRUE)
  ssm@r <- as.numeric(r[1, which.max(r[2, ])])
  #   ssm@r <- optimize(concentrated.likelihood,
  #                     interval = c(0,10000),
  #                     ssm = ssm,
  #                     maximum = TRUE)$maximum
  ssm@covariance <- compute.covariance(ssm, ssm@r)
  eRe <- tryCatch(solve(ssm@covariance, ssm@residuals %*% t(ssm@residuals)),
                  error = function (e) Inf)
  if (eRe[1] == Inf){
    cat("Covariance inversion numerically unstable, increasing tolerance
        (results will be less accurate.\n")
    eRe <- tryCatch(solve(ssm@covariance, ssm@residuals %*% t(ssm@residuals)),
                    error = function (e) Inf, tol = 1e-30)
  }
  ssm@sigma <- sum(diag(eRe)) / ssm@design_size
  return(ssm)
}

#' Compute unscaled covariance matrix from a supplied distance matrix and length
#' parameter.
#'
#' This is a legacy function and is not used.
#' @param distance A matrix. A symmetric matrix of distances between design
#'   points.
#' @param r A number. The length parameter for the correlation function
#' @param type (optional) Character. Specifies the correlation function. Default
#'   behaviour is to use \code{"exp"} for a squared exponential correlation.
#'   \code{"Matern32"} uses a Matern 3/2 correlation.
#' @param ssm (obsolete) An SSM object. Used for the removed
#'   \code{"ssm"} correlation function which tried to use the local smoothness
#'   of the SSM to measure distances.
#' @keywords internal
compute.covariance.from.distance <- function(distance, r, type = "exp", ssm){
  if (type == "matern32") return (exp(- distance * sqrt(3) * r) *
                                    (1 + distance * sqrt(3) * r))
  #if (type == "ssm"){
  #  adjusted_smoothness <- ssm@local_smoothness %*% t(ssm@local_smoothness) *
  #    max(ssm@distance^2) / max(ssm@local_smoothness)^2
  #  return(exp(-adjusted_smoothness * ssm@distance^2 * r))
  #}
  return(exp(-distance^2 * r))
}

#' Plot the concentrated likelihood of an SSM.
#'
#' Plot the concentrated likelihood used to estimate the parameters of the
#' metamodel error estimating Gaussian process.
#'
#' As a diagnostic it can be helpful to look at the concentrated likelihood
#' function of the correlation function used to estimate the metamodel error.
#' Flat likelihood functions make it difficult to pick a suitable \code{r}
#' length parameter. Note that \code{r} and \code{sigma} can be set manually.
#' @param ssm An SSM object.
#' @param xrange (optional) The range of the x axis. Set to \code{c(0, 1000)}
#'   by default.
#' @param grid (optional) A number. The number of points used to plot the curve.
#' @export
#' @examples
#' data(attitude)
#' X <- transform11(attitude[ 2:7])
#' Y <- attitude[ , 1]
#' s <- fit.ssm(X, Y, GP = TRUE)
#' likelihood.plot(s)
#' likelihood.plot(s, xrange = c(0, 20))
likelihood.plot <- function(ssm, xrange = c(0,1000), grid = 200){
  x<-seq(xrange[1], xrange[2], length.out=grid)
  y <- sapply(x, concentrated.likelihood, ssm = ssm)
  plot(x, y, type = "l")
}


#' Compute second partial derivative of a smooth supersaturated model at all
#' design points.
#'
#' Computes a second partial derivative of a smooth supersaturated model at a
#' design point. Used in the computation of distance measures based on local
#' smoothness.
#' @param indices A vector of integers specifying the two variables by which we
#'   take the partial derivatives with respect to.
#' @param ssm An SSM object.
#' @return A single column vector containing the second partial derivatives with
#' respect to the requested variables, evaluated at all design points.
#' @keywords internal
partial.deriv.ssm <- function(indices, ssm){
  # computes the second derivative of the ssm at all design points with respect
  # to the two indices
  multiplier <- ssm@basis[, indices[1]]
  ssm@basis[ , indices[1]] <- ssm@basis[ , indices[1]] - 1
  ssm@basis[ssm@basis[ , indices[1]] < 0, indices[1]] <- 0
  multiplier <- ssm@basis[, indices[2]] * multiplier
  ssm@basis[ , indices[2]] <- ssm@basis[ , indices[2]] - 1
  ssm@basis[ssm@basis[ , indices[2]] < 0, indices[2]] <- 0
  X <- construct.dmm(ssm@basis, ssm@design, ssm@P)
  X %*% ssm@theta
}

#' Compute the smoothness of an SSM at all design points.
#'
#' This function evaluates the Frobenius norm of the Hessian matrix at all
#' design points. Used in the compuation of distance measures based on local
#' smoothness.
#' @param ssm An SSM object.
#' @keywords internal
#' @return A vector of integers containing the smoothness at each design point.
smoothness.over.design <- function (ssm){
  # computes the smoothness of the ssm at all design points
  combinations <- expand.grid(1:ssm@dimension, 1:ssm@dimension)
  second_derivs <- apply(combinations, 1, partial.deriv.ssm, ssm = ssm)
  second_derivs <- second_derivs^2
  apply(second_derivs, 1, sum)
}


#' Average the values in a vector between two cutoff points specified by a
#' separate vector.
#'
#' The values of \code{smoothnessdesign} that are in the interval specifed by
#' \code{x} are identified and the associated values in \code{smoothness} are
#' averaged. This is used in the computation of line integrals of smoothness
#' used by a particular distance measure when computing metamodel error
#' estimates.
#' @param x A vector of length two. Specifies the endpoints of the desired
#'   interval of integration.
#' @param smoothness A numeric vector.
#' @param smoothnessdesign A numeric vector of the same length as
#'   \code{smoothness}.
#' @keywords internal
#' @return A number.
lineij <- function(x, smoothness, smoothnessdesign){
  # input is the two design points we compute the line integral between, the ssm object and the smoothness over a lattice of points defined by smoothnessdesign
  if (x[1] == x[2]) return (0)
  maxx <- smoothnessdesign <= max(x)
  minx <- smoothnessdesign >= min(x)
  ind <- as.logical(maxx * minx)
  if (!any(ind)) return (0)
  return(mean(smoothness[ind]) * abs(diff(x)))
}

#' Compute the distance matrix of an SSM design.
#'
#' Computes the distance matrix associated with the design held in the
#' \code{design} slot of an SSM object. Used in the construction of the
#' metamodel error estimating Gaussian process.
#'
#' Implemented types of distance measure are:
#' \itemize{
#'   \item{\code{"distance"}}{ Standard Euclidean distance.}
#'   \item{\code{"line"}}{ The line integral of the smoothness between design
#'     points. This is not implemented for data where \code{d} > 1.}
#'   \item{\code{"product"}}{ The product of the Euclidean distance and the
#'     local smoothness at both points.}
#'   \item{\code{"area"}}{ The sum of the local smoothness at the points
#'     multiplied by the Euclidean distance.}
#'   \item{\code{"proddiff"}}{ Multiplies the difference in local smoothness
#'     between points by the Euclidean distance.}
#'   \item{\code{"smoothdiff"}}{ The difference between the local smoothness of
#'     points.}
#' }
#' All measures other than \code{"distance"} are experimental and should be
#' used with caution.
#' @param type (optional) Character. Specifies the distance measure used.
#'   Acceptable values are "distance", "line", "product", "area", "proddiff",
#'   "smoothdiff".
#' @param ssm An SSM object.
#' @param line.grid (optional). An integer. Specifies the number of points used
#'   in the computation of the distance matrix when \code{type = "line"}.
#' @keywords internal
#' @return A matrix.
new.distance <- function(type = "distance", ssm, line.grid = 100){
  # This computes the 'distance' matrix using whichever type is specified.

  # NOTE: line integral 1d only at the moment
  if (ssm@dimension != 1 && type == "line"){
    print("The line integral of smoothness is not implemented for data with more
          than one variable.  Distances computed using Euclidean distance instead.")
    type <- "distance"
  }

  # "distance": this uses the standard euclidean distance (stationary
  # covariance)
  # "line":     this uses the line integral of the smoothness function Psi2
  # "product":  this uses the product of local smoothness and distance
  # "area" :    this sums the local smoothness and multiplies by distance (i.e.
  # twice the area under the quadrilateral)
  # "proddiff":  this multiplies the difference in local smoothness by the
  # distance.
  # "smoothdiff" : the difference of local smoothness only

  if (type == "distance") new.distance <- as.matrix(dist(ssm@design))
  if (type == "line"){
    des <- seq(-1, 1, length.out = line.grid)
    s <- ssm
    s@design <- matrix(des, ncol = 1)
    smoothness <- smoothness.over.design(s)
    xx <- matrix(c(rep(ssm@design, nrow(ssm@design)),
                   sapply(ssm@design,rep,nrow(ssm@design))), ncol=2)
    new.distance <- apply(xx, 1, lineij, ssm = ssm,
                          smoothness = smoothness, smoothnessdesign = des)
    new.distance <- matrix(new.distance, ncol=nrow(ssm@design), byrow=TRUE)
  }

  if (type == "product"){
    distance <- as.matrix(dist(ssm@design))
    smoothness <- smoothness.over.design(ssm)
    smoothness <- matrix(outer(smoothness, smoothness, "*"),
                         ncol = nrow(ssm@design))
    new.distance <- distance * smoothness
  }
  if (type == "proddiff"){
    distance <- as.matrix(dist(ssm@design))
    smoothness <- smoothness.over.design(ssm)
    smoothness <- abs(matrix(outer(smoothness, smoothness, "-"),
                             ncol = nrow(ssm@design)))
    new.distance <- distance * smoothness
  }
  if (type == "area"){
    distance <- as.matrix(dist(ssm@design))
    smoothness <- smoothness.over.design(ssm)
    smoothness <- matrix(outer(smoothness, smoothness, "+"),
                         ncol = nrow(ssm@design))
    new.distance <- distance * smoothness
  }
  if (type == "smoothprod"){
    stop("This method is not yet implemented because it is not a valid
         distance")
  }
  if (type == "smoothdiff"){
    smoothness <- smoothness.over.design(ssm)
    new.distance <- matrix(abs(outer(smoothness, smoothness, "-")),
                           ncol = nrow(ssm@design))
  }
  return(new.distance)
}
