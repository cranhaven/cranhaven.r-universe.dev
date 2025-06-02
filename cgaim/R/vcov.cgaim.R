#' Calculate Variance-Covariance Matrix for a Fitted CGAIM Object
#'
#' Returns the variance covariance matrix of the main parameters of a fitted \code{cgaim} object. These parameters correspond to the index weights \code{alpha} and the scaling coefficients \code{beta}.
#'    
#' @param object A \code{cgaim} or \code{boot.cgaim} object.
#' @param parm The model components for which to get confidence intervals. 
#'    Either \code{"alpha"} (the default) for index weights or \code{"beta"} for scaling coefficients.
#' @param type The type of confidence intervals. Either \code{"normal"} (the default)
#'    or \code{"bootstrap"}. See details.
#' @param B The number of samples to be simulated.
#' @param complete Indicates whether the full variance-covariance matrix should be returned when some of the parameters could not be estimated. If so, the matrix is padded with \code{NA}s.
#' @param ... Additional parameters to be passed to \code{\link{boot.cgaim}} for bootstrap replications.
#'
#' @details 
#' Two types of computation are currently implemented in the function.
#' When \code{type = "normal"}, variance-covariance matrices are computed assuming 
#' components are normally distributed. Beta coefficients are treated as
#' regular linear regression coefficients and alpha
#' coefficients are assumed to follow a Truncated Multivariate Normal distribution. 
#' The latter is obtained by simulating from TMVN (see \code{\link[TruncatedNormal]{tmvnorm}}) 
#' and computing the empirical variance covariance matrix from these simulations. The parameter \code{B} controls the number of simulations from the TMVN (and is not used when \code{parm = "beta"}).
#' 
#' When \code{type = "bootstrap"}, the variance-covariance matrix is computed on Bootstrap replications. In this case \code{\link{boot.cgaim}} is called internally and \code{B} corresponds to the number of replications. Alternatively, the user can directly call \code{\link{boot.cgaim}} and feed the result into \code{vcov.boot.cgaim} (see examples).
#' 
#' @returns A variance-covariance matrix object. 
#' 
#' @seealso \code{\link{boot.cgaim}} for bootstrapping and \code{\link{confint.cgaim}} for confidence intervals. 
#'
#' @references
#'   Masselot, P. and others, 2022. Constrained groupwise additive index models.
#'     Biostatistics.
#'     
#'   Pya, N., Wood, S.N., 2015. Shape constrained additive models. 
#'    Stat. Comput. 25, 543–559. 
#'    
#'   Wood, S.N., 2017. Generalized Additive Models: An Introduction with R, 
#'     2nd ed, Texts in Statistical Science. Chapman and Hall/CRC.
#'
#' @examples 
#' # A simple CGAIM
#' n <- 200
#' x1 <- rnorm(n)
#' x2 <- x1 + rnorm(n)
#' z <- x1 + x2
#' y <- z + rnorm(n)
#' df1 <- data.frame(y, x1, x2) 
#' ans <- cgaim(y ~ g(x1, x2, acons = list(monotone = 1)), data = df1)
#' 
#' # (Truncated) Normal variance-covariance matrix
#' set.seed(1)
#' vcov(ans, B = 1000)
#' set.seed(1)
#' vcov(ans, parm = "alpha", B = 1000) # Same result
#' vcov(ans, parm = "beta", B = 1000)
#' 
#' # Confidence intervals by bootstrap (more computationally intensive, B should be increased)
#' set.seed(2)
#' vcov(ans, type = "boot", B = 10)
#' 
#' # Alternatively, bootstrap samples can be performed beforehand
#' set.seed(2) 
#' boot1 <- boot.cgaim(ans, B = 10)
#' vcov(boot1)
#'
#' @order 1
#' @export
vcov.cgaim <- function(object, parm = c("alpha", "beta"), 
  type = c("normal", "bootstrap"), B = 100, complete = TRUE, ...)
{
  #----- Header
  
  # Check parm
  parm <- match.arg(parm)
  
  # Check type
  type <- match.arg(type)
  
  #----- Compute vcov if "normal"
  if (type == "normal"){
    # For alpha: simulate from truncated normal
    if(parm == "alpha"){
      
      # Simulate alpha from truncated multivariate normal
      simures <- simul_tmvnorm(object, B = B)
      
      # Compute vcov
      vres <- stats::var(simures)
    }
    
    # For beta
    if (parm == "beta"){
      
      # use the usual vcov matrix
      vres <- vcov_beta(object)
    }
    
    # Set names
    rownames(vres) <- colnames(vres) <- names(unlist(object[[parm]]))
    
    # Fill potentially for aliased coefficients
    if (isTRUE(complete)){
      
      # Check if any aliased coefficients
      aliased <- is.na(unlist(object[[parm]]))
      
      # Complete
      vresa <- matrix(NA, length(aliased), length(aliased),
        dimnames = list(names(unlist(object[[parm]])), 
          names(unlist(object[[parm]]))))
      vresa[!aliased, !aliased] <- vres
      vres <- vresa
    }
  } else {
    
    # Simulate
    simures <- boot.cgaim(object, B = B, ...)
    
    # Compute CI
    vres <- vcov.boot.cgaim(simures, parm = parm, complete = complete)
  }
  
  #----- Return
  return(vres)
}