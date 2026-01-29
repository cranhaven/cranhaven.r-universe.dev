#' Build an `cgeneric` object to implement the PC-prior of a
#' precision matrix as inverse of a correlation matrix.
#' @param n integer to define the size of the matrix
#' @param lambda numeric (positive), the penalization rate parameter
#' @param theta.base numeric vector with the model parameters
#' at the base model
#' @param debug integer, default is zero, indicating the verbose level.
#' Will be used as logical by INLA.
#' @param useINLAprecomp logical, default is TRUE, indicating if it is to
#' be used the shared object pre-compiled by INLA.
#' This is not considered if 'shlib' is provided.
#' @param shlib string, default is NULL, with the path to the shared object.
#' @details
#' The Canonical Partial Correlation - CPC parametrization,
#'  Lewandowski, Kurowicka, and Joe (2009).
#' step 1:  \eqn{q_i} = tanh(\eqn{\theta_i})
#' step 2:
#' \deqn{z = \left[
#' \begin{array}{ccccc}
#'   1 & & & & \\
#'   q_1 & 1 & & & \\
#'   q_2 & q_n & & & \\
#'   \vdots & & \ldots & \ddots & \\
#'   q_{n-1} & q_{2n-3} \ldots & q_m & 1
#'   \end{array}
#'   \right] }
#'
#' step 3: compute L such that the correlation matrix is
#' \deqn{C = LL'}, a \eqn{n \times n} (lower triangle) matrix
#' \deqn{L_{i,j} = \left\{\begin{array}{cc}
#' 0 & i>j\\
#' 1 & i=j=1\\
#' z_{i,j} & j=1 \\
#' prod_{k=1}^{j-1}\sqrt(1-z_{k,j^2}) & 1<i=j \\
#' z_{i,j}prod_{k=1}^{j-1}\sqrt(1-z_{k,j}^2) &  1<j<i
#' \end{array}\right.}
#'
#' The prior of the correlation matrix is given as
#'  \deqn{p(C) = |J_m|*l*exp(-l*r)/(2*pi^(m-1)}
#'  following a bijective transformation from
#'  \deqn{\theta[1:m] \in R^{m} to \{r, \phi[1:(m-1)]\}}
#'  where \eqn{\phi[1:(m-1)]} are angles and
#'  r is the radius of a
#'  [m-sphere](https://en.wikipedia.org/wiki/N-sphere).
#'  That is
#'  \deqn{r ~ Exponential(\lambda)}
#'  \deqn{\phi[j] ~ Uniform(0, pi), j=1...m-2}
#'  \deqn{\phi[m-1] ~ Uniform(0, 2pi)}
#'  \eqn{J_m} is the Jacobian of this transformation
#' @references
#' Lewandowski, Daniel, Dorota Kurowicka, and Harry Joe.
#' 2009. “Generating Random Correlation Matrices Based
#' on Vines and Extended Onion Method.”
#' Journal of Multivariate Analysis 100: 1989–2001.
#' @return a `cgeneric` object, see [cgeneric()] for details.
cgeneric_pc_prec_correl <-
  function(n,
           lambda,
           theta.base,
           debug = FALSE,
           useINLAprecomp = TRUE,
           shlib = NULL) {

    if(is.null(shlib)) {
      if (useINLAprecomp) {
        shlib <- INLA::inla.external.lib("graphpcor")
      } else {
        shlib <- system.file("libs", package = "graphpcor")
        if (Sys.info()["sysname"] == "Windows") {
          shlib <- file.path(shlib, "graphpcor.dll")
        } else {
          shlib <- file.path(shlib, "graphpcor.so")
        }
      }
    }

    stopifnot(n>1)
    stopifnot(lambda>0)

    m <- n*(n-1)/2

    lc <- 0

    if(missing(theta.base)) {
      theta.base <- rep(0, m)
      warning("Missing 'theta.base' model. Assuming 'iid' by using:\n",
              paste(theta.base, collapse = ", "))
    }
    stopifnot(length(theta.base)==m)
      H.el <- Hcorrel(
        theta = theta.base,
        p = m,
        parametrization = "itp",
        itheta = which(lower.tri(diag(m))),
        decomposition = "svd")
      if(debug) {
        cat("H elements\n")
        print(utils::str(H.el))
      }
##      lc <- lc - sum(log(H.el$svd$d))

    ## constant:
    lc <- log(lambda) -gamma(1+m/2)-log(m)-(m/2)*log(pi)

    if(debug) {
      cat('log C', lc, '\n')
    }

    cmodel = "inla_cgeneric_pc_prec_correl"

    the_model <- list(
      f = list(
        model = "cgeneric",
        n = as.integer(n),
        cgeneric = list(
          model = cmodel,
          shlib = shlib,
          n = as.integer(n),
          debug = as.logical(debug),
          data = list(
            ints = list(
              n = as.integer(n),
              debug = as.integer(debug)
            ),
            doubles = list(
              lambda = as.numeric(lambda),
              lconst = as.numeric(lc),
              theta0 = as.numeric(theta.base)
            ),
            characters = list(
              model = cmodel,
              shlib = shlib
            ),
            matrices = list(
              hHn = c(m,m,attr(H.el, "h.5"))
              ),
            smatrices = list(
              )
            )
          )
        )
      )

    class(the_model) <- "cgeneric"
    class(the_model$f$cgeneric) <- "inla.cgeneric"

    return(the_model)

}
