#' A Reference Class which contains parameters of a StMoE model.
#'
#' ParamStMoE contains all the parameters of a StMoE model.
#'
#' @field X Numeric vector of length \emph{n} representing the covariates/inputs
#'   \eqn{x_{1},\dots,x_{n}}.
#' @field Y Numeric vector of length \emph{n} representing the observed
#'   response/output \eqn{y_{1},\dots,y_{n}}.
#' @field n Numeric. Length of the response/output vector `Y`.
#' @field K The number of experts.
#' @field p The order of the polynomial regression for the experts.
#' @field q The order of the logistic regression for the gating network.
#' @field alpha Parameters of the gating network. \eqn{\boldsymbol{\alpha} =
#'   (\boldsymbol{\alpha}_{1},\dots,\boldsymbol{\alpha}_{K-1})}{\alpha =
#'   (\alpha_{1},\dots,\alpha_{K-1})} is a matrix of dimension \eqn{(q + 1, K -
#'   1)}, with `q` the order of the logistic regression for the gating network.
#'   `q` is fixed to 1 by default.
#' @field beta Polynomial regressions coefficients for each expert.
#'   \eqn{\boldsymbol{\beta} =
#'   (\boldsymbol{\beta}_{1},\dots,\boldsymbol{\beta}_{K})}{\beta =
#'   (\beta_{1},\dots,\beta_{K})} is a matrix of dimension \eqn{(p + 1, K)},
#'   with `p` the order of the polynomial regression. `p` is fixed to 3 by
#'   default.
#' @field sigma2 The variances for the `K` mixture components (matrix of size
#'   \eqn{(1, K)}).
#' @field lambda The skewness parameters for each experts (matrix of size
#'   \eqn{(1, K)}).
#' @field delta delta is equal to \eqn{\delta =
#'   \frac{\lambda}{\sqrt{1+\lambda^2}}}{\delta = \lambda /
#'   (1+\lambda^2)^(1/2)}.
#' @field nu The degree of freedom for the Student distribution for each
#'   experts (matrix of size \eqn{(1, K)}).
#' @field df The degree of freedom of the StMoE model representing the
#'   complexity of the model.
#' @export
ParamStMoE <- setRefClass(
  "ParamStMoE",
  fields = list(

    X = "numeric",
    Y = "numeric",
    n = "numeric",
    phiBeta = "list",
    phiAlpha = "list",

    K = "numeric", # Number of components
    p = "numeric", # Dimension of beta (order of polynomial regression)
    q = "numeric", # Dimension of w (order of logistic regression)
    df = "numeric", # Degree of freedom

    alpha = "matrix",
    beta = "matrix",
    sigma2 = "matrix",
    lambda = "matrix",
    delta = "matrix",
    nu = "matrix"
  ),
  methods = list(
    initialize = function(X = numeric(), Y = numeric(1), K = 1, p = 3, q = 1) {

      X <<- X
      Y <<- Y
      n <<- length(Y)
      phiBeta <<- designmatrix(x = X, p = p)
      phiAlpha <<- designmatrix(x = X, p = q)

      df <<- (q + 1) * (K - 1) + (p + 1) * K + K + K + K
      K <<- K
      p <<- p
      q <<- q

      alpha <<- matrix(0, q + 1, K - 1)
      beta <<- matrix(NA, p + 1, K)
      sigma2 <<- matrix(NA, 1, K)
      lambda <<- matrix(NA, ncol = K)
      delta <<- matrix(NA, ncol = K)
      nu <<- matrix(NA, ncol = K)
    },

    initParam = function(segmental = FALSE) {
      "Method to initialize parameters \\code{alpha}, \\code{beta} and
      \\code{sigma2}.

      If \\code{segmental = TRUE} then \\code{alpha}, \\code{beta} and
      \\code{sigma2} are initialized by clustering the response \\code{Y}
      uniformly into \\code{K} contiguous segments. Otherwise, \\code{alpha},
      \\code{beta} and \\code{sigma2} are initialized by clustering randomly
      the response \\code{Y} into \\code{K} segments."

      # Initialize the regression parameters (coefficents and variances):
      if (!segmental) {

        klas <- sample(1:K, n, replace = TRUE)

        for (k in 1:K) {

          Xk <- phiBeta$XBeta[klas == k,]
          yk <- Y[klas == k]

          beta[, k] <<- solve(t(Xk) %*% Xk) %*% t(Xk) %*% yk

          sigma2[k] <<- sum((yk - Xk %*% beta[, k]) ^ 2) / length(yk)
        }
      } else {# Segmental : segment uniformly the data and estimate the parameters

        nk <- round(n / K) - 1

        klas <- rep.int(0, n)

        for (k in 1:K) {
          i <- (k - 1) * nk + 1
          j <- (k * nk)
          yk <- matrix(Y[i:j])
          Xk <- phiBeta$XBeta[i:j, ]

          beta[, k] <<- solve(t(Xk) %*% Xk, tol = 0) %*% (t(Xk) %*% yk)

          muk <- Xk %*% beta[, k, drop = FALSE]

          sigma2[k] <<- t(yk - muk) %*% (yk - muk) / length(yk)

          klas[i:j] <- k
        }
      }

      # Intialize the softmax parameters
      Z <- matrix(0, nrow = n, ncol = K)
      Z[klas %*% ones(1, K) == ones(n, 1) %*% seq(K)] <- 1
      tau <- Z
      res <- IRLS(phiAlpha$XBeta, tau, ones(nrow(tau), 1), alpha)
      alpha <<- res$W

      # Initialize the skewness parameter Lambdak (by equivalence delta)
      lambda <<- -1 + 2 * rand(1, K)
      delta <<- lambda / sqrt(1 + lambda ^ 2)

      # Intitialization of the degrees of freedom
      nu <<- 50 * rand(1, K)
    },

    MStep = function(statStMoE, calcAlpha = FALSE, calcBeta = FALSE,
                     calcSigma2 = FALSE, calcLambda = FALSE, calcNu = FALSE,
                     verbose_IRLS = FALSE) {
      "Method which implements the M-step of the EM algorithm to learn the
      parameters of the StMoE model based on statistics provided by the object
      \\code{statStMoE} of class \\link{StatStMoE} (which contains the E-step)."

      reg_irls <- 0

      if (calcAlpha) {

        res_irls <- IRLS(phiAlpha$XBeta, statStMoE$tik, ones(nrow(statStMoE$tik), 1), alpha, verbose_IRLS)
        reg_irls <- res_irls$reg_irls
        alpha <<- res_irls$W
      }

      # Update the regression coefficients
      if (calcBeta) {

        for (k in 1:K) {
          TauikWik <- (statStMoE$tik[, k] * statStMoE$wik[, k]) %*% ones(1, p + 1)
          TauikX <- phiBeta$XBeta * (statStMoE$tik[, k] %*% ones(1, p + 1))
          betak <- solve((t(TauikWik * phiBeta$XBeta) %*% phiBeta$XBeta), tol = 0) %*% (t(TauikX) %*% ((statStMoE$wik[, k] * Y) - (delta[k] * statStMoE$E1ik[, k])))
          beta[, k] <<- betak
        }

      }

      # Update the variances sigma2k
      if (calcSigma2) {

        for (k in 1:K) {
          sigma2[k] <<- sum(statStMoE$tik[, k] * (statStMoE$wik[, k] * ((Y - phiBeta$XBeta %*% beta[, k]) ^ 2) - 2 * delta[k] * statStMoE$E1ik[, k] * (Y - phiBeta$XBeta %*% beta[, k]) + statStMoE$E2ik[, k])) / (2 * (1 - delta[k] ^ 2) * sum(statStMoE$tik[, k]))
        }
      }

      # Update the deltak (the skewness parameter)
      if (calcLambda) {

        for (k in 1:K) {
          try(lambda[k] <<- uniroot(f <- function(lmbda) {
            return((lmbda / sqrt(1 + lmbda ^ 2)) * (1 - (lmbda ^ 2 / (1 + lmbda ^ 2))) *
                     sum(statStMoE$tik[, k])
                   + (1 + (lmbda ^ 2 / (1 + lmbda ^ 2))) * sum(statStMoE$tik[, k] * statStMoE$dik[, k] *
                                                                 statStMoE$E1ik[, k] / sqrt(sigma2[k]))
                   - (lmbda / sqrt(1 + lmbda ^ 2)) * sum(statStMoE$tik[, k] * (
                     statStMoE$wik[, k] * (statStMoE$dik[, k] ^ 2) + statStMoE$E2ik[, k] / (sqrt(sigma2[k]) ^ 2)
                   ))
            )
          }, c(-100, 100), extendInt = "yes")$root,
          silent = TRUE)

          delta[k] <<- lambda[k] / sqrt(1 + lambda[k] ^ 2)
        }
      }

      if (calcNu) {

        for (k in 1:K) {
          try(nu[k] <<- suppressWarnings(uniroot(f <- function(nnu) {
            return(-psigamma((nnu) / 2) + log((nnu) / 2) + 1 + sum(statStMoE$tik[, k] * (statStMoE$E3ik[, k] - statStMoE$wik[, k])) /
                     sum(statStMoE$tik[, k]))
          }, c(0, 100))$root), silent = TRUE)
        }
      }

      return(reg_irls)
    }
  )
)
