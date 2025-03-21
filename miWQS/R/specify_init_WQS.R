#' Accessory WQS Function: Initial Values for the Training Set
#' @family wqs
#' @description Initial values for WQS model. Included for reproducibility

#' @details The default initial values are: \itemize{
#' \item Intercept=0,
#' \item b1=+- 0.1 , depending on if b1.pos=TRUE,
#' \item equal weights (initial value)
#' \item covariate estimates from covariate only model.
#' }

#' @note Adapted from \pkg{wqs}.

#' @param Z covariates (passed as argument from \code{estimate.wqs})
#' @param y outcome (passed as argument from \code{estimate.wqs})
#' @param C Number of chemicals in X matrix.
#' @param b1.pos passed from \code{estimate.wqs}.
#' @param family distribution of y. See \code{\link[stats]{glm}} for description. Passed from \code{estimate.wqs}.
#' @inheritParams estimate.wqs
#' @return A vector of initial values: \describe{
#'    \item{b0}{the initial intercept value}
#'    \item{b1}{ the initial chemical mixture effect value}
#'    \item{w}{the initial weights. Default is 1/C, where C is the number of chemicals}
#'    \item{z}{the initial covariates. Comes from glm2 ignoring the chemicals.}
#'    }
#'
#' @examples
#' # No, 1, 2 covariates.
#' set.seed(1213);
#' init <- specify.init(Z = NULL, y = rnorm(10),
#'   b1.pos = TRUE, C = 4, family = "gaussian"); init
#' set.seed(1213);
#' init <- specify.init(Z =  sample(c(0, 1), 10, TRUE), y = rnorm(10),
#'   b1.pos = TRUE, C = 4, family = "gaussian"); init
#' set.seed(1213);
#' specify.init(Z = cbind(sample(c(0, 1), 10, TRUE), rnorm(10, 50, 5)),
#'   y = rnorm(10), b1.pos = TRUE, C = 4, family = "gaussian")
#' # Use WQS Dataset to check
#' # library(wqs);
#' data("WQSdata")
#' specify.init(y = WQSdata[, 'y'], Z = WQSdata[, 1],
#'   b1.pos = TRUE, C = 7, family = "gaussian")
#' init <- specify.init(y = WQSdata[, 'y'], Z = WQSdata[, 1:2],
#'   b1.pos = TRUE, C = 7, family = "gaussian")
#'
#' init <- specify.init(Z =  sample(c(0, 1), 10, TRUE), y = rpois(10, 3),
#'   b1.pos = TRUE, C = 4, family = "negbin")
#'

#' @noRd
# No need to be exported.

specify.init <- function(
  Z,
  y,
  b1.pos,
  C,
  family,
  offset = NULL,
  verbose = FALSE
) {

    #Initial weights and overall mixture
    w.0 <- rep(1 / C, C); names(w.0) <- paste0("w", 1:C)  # weights
    b1.0 <- ifelse(b1.pos == TRUE, 0.1, -0.1)  # index

    # If offset is null, convert offset to a vector of 1's
    if (is.null(offset)) { offset <- rep(1, nrow(Z)) }

    # Initial Values for Covariates
    if (is.null(Z)) {
      init <- c(b0 = 0, b1 = b1.0, w.0)
    } else {

      # Covariate initial values come from glm2 (for most families.)
      if (family  != "negbin") {
        fit.init <- glm2::glm2(y ~ ., data = data.frame(y, Z), family = family, offset = log(offset))
        alpha.init <- 0

      } else if(family == "negbin"){
        # Assumes Yi ~ NegBinom(1/alpha, pi)
        # Defualt Initial value for the theta parameter: a moment estimator after an initial fit using a Poisson GLM is used.
        # epsilon: upper limit to assesss convergence between iterations.
        # maxit: number of iterations and alterations between estimating theta/lambda.
        # trace:  = 0: nothing is printed. = 1: traces the glm fit ;  = 2: traces theta estimation.
        fit.init <- MASS::glm.nb(
          y ~ ., data = data.frame(y, Z),
          na.action = getOption("na.action"), # default is na.omit
          control = list(epsilon = 1e-8, maxit = 25, trace = 1),
          link = "log"
        )
        alpha.init <- fit.init$theta

      } else {
        stop("`family` is incorrectly specified. It must be one of these: ")
      }

      init.Z <- coef(fit.init)[-1]  # or, if didn't work    #summary(fit.init)$coefficients[-1,1]
      p <- if (is(Z, "numeric")) { 1 } else { dim(Z)[2]  }  # Edited.
      names(init.Z) <- paste0("z", 1:p)

      # Intercept estimate comes from glm.
      b0.0 <- coef(fit.init)[1] ; names(b0.0) <- NULL
      init <- c(b0 = b0.0, b1 = b1.0, w.0, init.Z, alpha = alpha.init) # Added theta.init 5/22/20
    }

    if (verbose) {
      cat("## Initial Values for Training WQS model are \n")
      print(init)
    }

  return(init)
}

# Log
# Edited: Added line to find p if Z is a vector or matrix
# Edited: added family to glm2
# Edited: Change initial value for intercept to be estimate from covariate glm model.

# 5/22/19: Found that if offset = NULL, it assumes a vector of zero's not ones as default. As I am putting offset in normal scale, need to fix by taking the log().
#
