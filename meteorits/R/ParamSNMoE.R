#' A Reference Class which contains parameters of a SNMoE model.
#'
#' ParamSNMoE contains all the parameters of a SNMoE model.
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
#' @field df The degree of freedom of the SNMoE model representing the
#'   complexity of the model.
#' @export
ParamSNMoE <- setRefClass(
  "ParamSNMoE",
  fields = list(

    X = "numeric",
    Y = "numeric",
    n = "numeric",
    phiBeta = "list",
    phiAlpha = "list",

    K = "numeric", # Number of regimes
    p = "numeric", # Dimension of beta (order of polynomial regression)
    q = "numeric", # Dimension of w (order of logistic regression)
    df = "numeric", # Degree of freedom

    alpha = "matrix",
    beta = "matrix",
    sigma2 = "matrix",
    lambda = "matrix",
    delta = "matrix"
  ),
  methods = list(
    initialize = function(X = numeric(), Y = numeric(1), K = 1, p = 3, q = 1) {

      X <<- X
      Y <<- Y
      n <<- length(Y)
      phiBeta <<- designmatrix(x = X, p = p)
      phiAlpha <<- designmatrix(x = X, p = q)

      df <<- (q + 1) * (K - 1) + (p + 1) * K + K + K
      K <<- K
      p <<- p
      q <<- q

      alpha <<- matrix(0, q + 1, K - 1)
      beta <<- matrix(NA, p + 1, K)
      sigma2 <<- matrix(NA, 1, K)
      lambda <<- matrix(NA, ncol = K)
      delta <<- matrix(NA, ncol = K)
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

          beta[, k] <<- solve(t(Xk) %*% Xk) %*% (t(Xk) %*% yk)

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

      # # Possible initialization using moments
      # ybar <- mean(Y)
      # a1 <- sqrt(2 / pi)
      # b1 <- (4 / pi - 1) * a1
      # m2 <- (1 / (n - 1)) * sum((Y - ybar) ^ 2)
      # m3 <- (1 / (n - 1)) * sum(abs(Y - ybar) ^ 3)
      # DeltakAll <- (a1 ^ 2 + m2 * (b1 / m3) ^ (2 / 3)) ^ (0.5)
      # lambda <<- DeltakAll * ones(1, K)
      # delta <<- lambda / sqrt(1 + lambda ^ 2)
    },

    MStep = function(statSNMoE, verbose_IRLS) {
      "Method which implements the M-step of the EM algorithm to learn the
      parameters of the SNMoE model based on statistics provided by the object
      \\code{statSNMoE} of class \\link{StatSNMoE} (which contains the E-step)."

      res_irls <- IRLS(phiAlpha$XBeta, statSNMoE$tik, ones(nrow(statSNMoE$tik), 1), alpha, verbose_IRLS)

      statSNMoE$piik <- res_irls$piik
      reg_irls <- res_irls$reg_irls

      alpha <<- res_irls$W

      for (k in 1:K) {

        # Update the regression coefficients
        tauik_Xbeta <- (statSNMoE$tik[, k] %*% ones(1, p + 1)) * phiBeta$XBeta

        beta[, k] <<- solve((t(tauik_Xbeta) %*% phiBeta$XBeta)) %*% (t(tauik_Xbeta) %*% (Y - delta[k] * statSNMoE$E1ik[, k]))

        # Update the variances sigma2k
        sigma2[k] <<- sum(statSNMoE$tik[, k] * ((Y - phiBeta$XBeta %*% beta[, k]) ^ 2 - 2 * delta[k] * statSNMoE$E1ik[, k] * (Y - phiBeta$XBeta %*% beta[, k]) + statSNMoE$E2ik[, k])) / (2 * (1 - delta[k] ^ 2) * sum(statSNMoE$tik[, k]))

        # Update the lambdak (the skewness parameter)
        try(lambda[k] <<- uniroot(f <- function(lmbda) {
          return(
            sigma2[k] * (lmbda / sqrt(1 + lmbda ^ 2)) * (1 - (lmbda ^ 2 / (1 + lmbda ^ 2))) * sum(statSNMoE$tik[, k]) + (1 + (lmbda ^ 2 / (1 + lmbda ^ 2))) * sum(
              statSNMoE$tik[, k] * (Y - phiBeta$XBeta %*% beta[, k]) * statSNMoE$E1ik[, k]
            )
            - (lmbda / sqrt(1 + lmbda ^ 2)) * sum(statSNMoE$tik[, k] * (
              statSNMoE$E2ik[, k] + (Y - phiBeta$XBeta %*% beta[, k]) ^ 2
            ))
          )
        },
        interval = c(-100, 100),
        extendInt = "yes")$root,
        silent = TRUE)

        delta[k] <<- lambda[k] / sqrt(1 + lambda[k] ^ 2)

      }

      return(reg_irls)
    }
  )
)
