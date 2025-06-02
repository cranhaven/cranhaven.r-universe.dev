#' Confidence intervals
#'
#' Computes confidence intervals for the CGAIM components.
#'    
#' @param object A \code{cgaim} or \code{boot.cgaim} object.
#' @param parm The model components for which to get confidence intervals. 
#'    One or several of: \code{"alpha"} for index weights,
#'    \code{"beta"} for scaling coefficients and \code{"g"} for ridge function. By default,
#'    returns confidence intervals for all components.
#' @param level The level of confidence intervals. Default to 0.95.
#' @param type The type of confidence intervals. Either \code{"normal"} (the default)
#'    or \code{"bootstrap"}. See details.
#' @param B The number of samples to be simulated.
#' @param ... Additional parameters to be passed to \code{\link{boot.cgaim}} for bootstrap confidence intervals.
#'
#' @details 
#' Two types of confidence intervals are currently implemented in the function.
#' When \code{type = "normal"}, confidence intervals are computed assuming 
#' components are normally distributed. Beta coefficients are treated as
#' regular linear regression coefficients and g as regular smooth functions
#' estimated by (shape-constrained) generalized additive models. For alpha
#' coefficients, we consider a linear transformation mapping them to a 
#' Truncated Multivariate Normal distribution (i.e. with only bound constraints). 
#' Simulation from the TMVN are performed (see \code{\link[TruncatedNormal]{tmvnorm}}) 
#' and transformed 
#' back into the original coefficient space (i.e. with linear constraints). The parameter \code{B} controls the number of simulations from the TMVN. 
#' Confidence intervals are computed as the percentiles of these simulated
#' coefficients, ensuring the confidence region is entirely within the feasible
#' region defined by the constraints.
#' 
#' When \code{type = "bootstrap"}, confidence intervals are estimated by 
#' percentile bootstrap. \code{\link{boot.cgaim}} is called internally
#' to create \code{B} samples of model components, and confidence intervals are then computed
#' as the percentiles of bootstrap samples. Alternatively, the user can directly
#' call \code{\link{boot.cgaim}} and feed the result into 
#' \code{confint.boot.cgaim}.
#' 
#' @returns A list of confidence intervals. Contains one element per model
#' component in the \code{parm} argument. 
#' 
#' @note Confidence intervals for the g functions are evaluated on the 
#' same \code{n} index values as the functions in \code{object}.
#' 
#' @seealso \code{\link{boot.cgaim}} for bootstrapping. 
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
#'   DiCiccio, T.J., Efron, B., 1996. Bootstrap Confidence Intervals. 
#'     Statistical Science 11, 189–212.
#'     
#'   Carpenter, J., Bithell, J., 2000. Bootstrap confidence intervals: 
#'     when, which, what? A practical guide for medical statisticians. 
#'     Statistics in Medicine 19, 1141–1164.
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
#' # Normal confidence intervals
#' set.seed(1)
#' ci1 <- confint(ans, B = 1000)
#' ci1$alpha
#' ci1$beta
#' 
#' # Select only alphas: identical to above result
#' set.seed(1)
#' confint(ans, B = 1000, parm = "alpha")
#' 
#' # Select only betas: identical to above result
#' set.seed(1)
#' confint(ans, B = 1000, parm = "beta")
#' 
#' # Confidence intervals by bootstrap (more computationally intensive, B should be increased)
#' set.seed(2)
#' ci2 <- confint(ans, type = "boot", B = 10)
#' 
#' # Alternatively, bootstrap samples can be performed beforehand
#' set.seed(2) 
#' boot1 <- boot.cgaim(ans, B = 10)
#' ci3 <- confint(boot1)
#'
#' @order 1
#' @export
confint.cgaim <- function(object, parm, level = 0.95, 
  type = c("normal", "bootstrap"), B = 100, ...)
{
  #----- header
  
  # Check type
  type <- match.arg(type)
  
  # Get useful objects
  p <- length(object$alpha)
  ptot <- ncol(object$gfit)
  d <- length(object$index)
  n <- length(object$fitted)
  
  # Setup parm. If missing get all parameters
  if (missing(parm)){
    parm <- 1:3
  } else {      
    if (is.character(parm)){
      parm <- stats::na.omit(match(parm, c("alpha", "beta", "g")))
    } else {
      parm <- parm[parm %in% 1:3]
    }
    if (!any(parm %in% 1:3)){
      stop(paste0("'parm' must be in 1:3"))
    }
  }
  
  # Setup CI level
  alims <- c((1 - level) / 2, 1 - (1 - level) / 2)
  level.labels <- sprintf("%s%%", alims * 100)
  
  #----- Compute confidence intervals
  if (type == "normal"){
    
    res <- list()
    
    # Alpha
    if (1 %in% parm){
      # Simulate alpha from truncated multivariate normal
      simures <- simul_tmvnorm(object, B = B)
      
      # Compute CI for Alpha
      res$alpha <- t(apply(simures, 2, stats::quantile, alims, na.rm = TRUE))
      
      # Names
      rownames(res$alpha) <- names(unlist(object$alpha))
      colnames(res$alpha) <- level.labels
    }
    
    # Beta
    if (2 %in% parm){
      # Extract standard error for beta
      vb <- vcov_beta(object)
      seb <- sqrt(diag(vb))
      
      # Compute CI for beta
      betahat <- object$beta
      res$beta <- betahat + matrix(seb, ncol = 1) %*% stats::qnorm(alims)
      
      # names
      rownames(res$beta) <- names(object$beta)
      colnames(res$beta) <- level.labels
    }
    
    # Ridge functions
    if (3 %in% parm){
      # Extract bounds
      gbnd <- stats::qnorm(alims[2]) * t(t(object$gse) / object$beta[-1])
      
      # Boundaries
      res$g <- array(NA, dim = c(n, ptot, 2), 
        dimnames = list(NULL, colnames(object$gfit), level.labels))
      res$g[,,1] <- object$gfit - gbnd
      res$g[,,2] <- object$gfit + gbnd
    }
    
  } else {
    
    # Simulate
    simures <- boot.cgaim(object, B = B, ...)
    
    # Compute CI
    res <- confint.boot.cgaim(simures, parm = parm, level = level)
  }
  
  #----- Return
  return(res)
}
