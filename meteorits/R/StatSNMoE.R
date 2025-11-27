#' A Reference Class which contains statistics of a SNMoE model.
#'
#' StatSNMoE contains all the statistics associated to a [SNMoE][ParamSNMoE] model.
#' It mainly includes the E-Step of the ECM algorithm calculating the posterior
#' distribution of the hidden variables, as well as the calculation of the
#' log-likelhood.
#'
#' @field piik Matrix of size \eqn{(n, K)} representing the probabilities
#'   \eqn{\pi_{k}(x_{i}; \boldsymbol{\Psi}) = P(z_{i} = k |
#'   \boldsymbol{x}; \Psi)}{\pi_{k}(x_{i}; \Psi) = P(z_{i} = k | x; \Psi)} of
#'   the latent variable \eqn{z_{i}, i = 1,\dots,n}.
#' @field z_ik Hard segmentation logical matrix of dimension \eqn{(n, K)}
#'   obtained by the Maximum a posteriori (MAP) rule: \eqn{z\_ik = 1 \
#'   \textrm{if} \ z\_ik = \textrm{arg} \ \textrm{max}_{s} \ \tau_{is};\ 0 \
#'   \textrm{otherwise}}{z_ik = 1 if z_ik = arg max_s \tau_{is}; 0 otherwise},
#'   \eqn{k = 1,\dots,K}.
#' @field klas Column matrix of the labels issued from `z_ik`. Its elements are
#'   \eqn{klas(i) = k}, \eqn{k = 1,\dots,K}.
#' @field tik Matrix of size \eqn{(n, K)} giving the posterior probability
#'   \eqn{\tau_{ik}}{\tauik} that the observation \eqn{y_{i}}{yi} originates
#'   from the \eqn{k}-th expert.
#' @field Ey_k Matrix of dimension \emph{(n, K)} giving the estimated means of the experts.
#' @field Ey Column matrix of dimension \emph{n} giving the estimated mean of the SNMoE.
#' @field Var_yk Column matrix of dimension \emph{K} giving the estimated means of the experts.
#' @field Vary Column matrix of dimension \emph{n} giving the estimated variance of the response.
#' @field loglik Numeric. Observed-data log-likelihood of the SNMoE model.
#' @field com_loglik Numeric. Complete-data log-likelihood of the SNMoE model.
#' @field stored_loglik Numeric vector. Stored values of the log-likelihood at
#'   each ECM iteration.
#' @field BIC Numeric. Value of BIC (Bayesian Information Criterion).
#' @field ICL Numeric. Value of ICL (Integrated Completed Likelihood).
#' @field AIC Numeric. Value of AIC (Akaike Information Criterion).
#' @field log_piik_fik Matrix of size \eqn{(n, K)} giving the values of the
#'   logarithm of the joint probability \eqn{P(y_{i}, \ z_{i} = k |
#'   \boldsymbol{x}, \boldsymbol{\Psi})}{P(y_{i}, z_{i} = k | x, \Psi)}, \eqn{i
#'   = 1,\dots,n}.
#' @field log_sum_piik_fik Column matrix of size \emph{m} giving the values of
#'   \eqn{\textrm{log} \sum_{k = 1}^{K} P(y_{i}, \ z_{i} = k | \boldsymbol{x},
#'   \boldsymbol{\Psi})}{log \sum_{k = 1}^{K} P(y_{i}, z_{i} = k | x, \Psi)},
#'   \eqn{i = 1,\dots,n}.
#' @field E1ik Conditional expectations of \eqn{U_{i}}{Ui} (Matrix of size \eqn{(n, K)}).
#' @field E2ik Conditional expectations of \eqn{U_{i}^{2}}{Ui^2} (Matrix of size \eqn{(n, K)}).
#' @seealso [ParamSNMoE]
#' @export
StatSNMoE <- setRefClass(
  "StatSNMoE",
  fields = list(
    piik = "matrix",
    z_ik = "matrix",
    klas = "matrix",
    Ey_k = "matrix",
    Ey = "matrix",
    Var_yk = "matrix",
    Vary = "matrix",
    loglik = "numeric",
    com_loglik = "numeric",
    stored_loglik = "numeric",
    BIC = "numeric",
    ICL = "numeric",
    AIC = "numeric",
    log_piik_fik = "matrix",
    log_sum_piik_fik = "matrix",
    tik = "matrix",
    E1ik = "matrix",
    E2ik = "matrix"
  ),
  methods = list(
    initialize = function(paramSNMoE = ParamSNMoE()) {
      piik <<- matrix(NA, paramSNMoE$n, paramSNMoE$K)
      z_ik <<- matrix(NA, paramSNMoE$n, paramSNMoE$K)
      klas <<- matrix(NA, paramSNMoE$n, 1)
      Ey_k <<- matrix(NA, paramSNMoE$n, paramSNMoE$K)
      Ey <<- matrix(NA, paramSNMoE$n, 1)
      Var_yk <<- matrix(NA, 1, paramSNMoE$K)
      Vary <<- matrix(NA, paramSNMoE$n, 1)
      loglik <<- -Inf
      com_loglik <<- -Inf
      stored_loglik <<- numeric()
      BIC <<- -Inf
      ICL <<- -Inf
      AIC <<- -Inf
      log_piik_fik <<- matrix(0, paramSNMoE$n, paramSNMoE$K)
      log_sum_piik_fik <<- matrix(NA, paramSNMoE$n, 1)
      tik <<- matrix(0, paramSNMoE$n, paramSNMoE$K)
      E1ik <<- matrix(0, paramSNMoE$n, paramSNMoE$K)
      E2ik <<- matrix(0, paramSNMoE$n, paramSNMoE$K)
    },

    MAP = function() {
      "MAP calculates values of the fields \\code{z_ik} and \\code{klas}
      by applying the Maximum A Posteriori Bayes allocation rule.

      \\eqn{z_{ik} = 1 \\ \\textrm{if} \\ k = \\textrm{arg} \\ \\textrm{max}_{s}
      \\ \\tau_{is};\\ 0 \\ \\textrm{otherwise}}{
      z_{ik} = 1 if z_ik = arg max_{s} \\tau_{is}; 0 otherwise}"

      N <- nrow(tik)
      K <- ncol(tik)
      ikmax <- max.col(tik)
      ikmax <- matrix(ikmax, ncol = 1)
      z_ik <<- ikmax %*% ones(1, K) == ones(N, 1) %*% (1:K) # partition_MAP
      klas <<- ones(N, 1)
      for (k in 1:K) {
        klas[z_ik[, k] == 1] <<- k
      }
    },

    computeLikelihood = function(reg_irls) {
      "Method to compute the log-likelihood. \\code{reg_irls} is the value of
      the regularization part in the IRLS algorithm."

      loglik <<- sum(log_sum_piik_fik) + reg_irls

    },

    computeStats = function(paramSNMoE) {
      "Method used in the ECM algorithm to compute statistics based on
      parameters provided by the object \\code{paramSNMoE} of class
      \\link{ParamSNMoE}."

      # E[yi|xi,zi=k]
      Ey_k <<- paramSNMoE$phiBeta$XBeta[1:paramSNMoE$n,] %*% paramSNMoE$beta + ones(paramSNMoE$n, 1) %*% (sqrt(2 / pi) * paramSNMoE$delta * sqrt(paramSNMoE$sigma2))

      # E[yi|xi]
      Ey <<- matrix(apply(piik * Ey_k, 1, sum))

      # Var[yi|xi,zi=k]
      Var_yk <<- (1 - (2 / pi) * (paramSNMoE$delta ^ 2)) * paramSNMoE$sigma2

      # Var[yi|xi]
      Vary <<- apply(piik * (Ey_k ^ 2 + ones(paramSNMoE$n, 1) %*% Var_yk), 1, sum) - Ey ^ 2

      # BIC, AIC and ICL

      BIC <<- loglik - (paramSNMoE$df * log(paramSNMoE$n) / 2)
      AIC <<- loglik - paramSNMoE$df

      # CL(theta) : complete-data loglikelihood
      zik_log_piik_fk <- z_ik * log_piik_fik
      sum_zik_log_fik <- apply(zik_log_piik_fk, 1, sum)
      com_loglik <<- sum(sum_zik_log_fik)

      ICL <<- com_loglik - (paramSNMoE$df * log(paramSNMoE$n) / 2)

    },

    EStep = function(paramSNMoE) {
      "Method used in the ECM algorithm to update statistics based on parameters
      provided by the object \\code{paramSNMoE} of class \\link{ParamSNMoE}
      (prior and posterior probabilities)."

      piik <<- multinomialLogit(paramSNMoE$alpha, paramSNMoE$phiAlpha$XBeta, ones(paramSNMoE$n, paramSNMoE$K), ones(paramSNMoE$n, 1))$piik

      piik_fik <- zeros(paramSNMoE$n, paramSNMoE$K)

      for (k in (1:paramSNMoE$K)) {

        muk <- paramSNMoE$phiBeta$XBeta %*% paramSNMoE$beta[, k]
        sigma2k <- paramSNMoE$sigma2[k]
        sigmak <- sqrt(sigma2k)
        dik <- (paramSNMoE$Y - muk) / sigmak

        mu_uk <- paramSNMoE$delta[k] * (paramSNMoE$Y - muk)
        sigma2_uk <- (1 - paramSNMoE$delta[k] ^ 2) * paramSNMoE$sigma2[k]
        sigma_uk <- sqrt(sigma2_uk)

        # E1ik = E[Ui|yi,xi,zik=1]
        E1ik[, k] <<- mu_uk + sigma_uk * dnorm(paramSNMoE$lambda[k] * dik, 0, 1) / pnorm(paramSNMoE$lambda[k] * dik, 0, 1)
        E1ik[is.nan(E1ik[, k]), k] <<- mu_uk[is.nan(E1ik[, k])] - sigma_uk * paramSNMoE$lambda[k] * dik[is.nan(E1ik[, k])]
        E1ik[is.infinite(E1ik[, k]), k] <<- mu_uk[is.infinite(E1ik[, k])] - sigma_uk * paramSNMoE$lambda[k] * dik[is.infinite(E1ik[, k])]

        # E2ik = E[Ui^2|y,zik=1]
        E2ik[, k] <<- mu_uk ^ 2 + sigma_uk ^ 2 + sigma_uk * mu_uk * dnorm(paramSNMoE$lambda[k] * dik, 0, 1) / pnorm(paramSNMoE$lambda[k] * dik, 0, 1)
        E2ik[is.nan(E2ik[, k]), k] <<- mu_uk[is.nan(E2ik[, k])] ^ 2 + sigma_uk ^ 2 - sigma_uk * mu_uk[is.nan(E2ik[, k])] * paramSNMoE$lambda[k] * dik[is.nan(E2ik[, k])]
        E2ik[is.infinite(E2ik[, k]), k] <<- mu_uk[is.infinite(E2ik[, k])] ^ 2 + sigma_uk ^ 2 - sigma_uk * mu_uk[is.infinite(E2ik[, k])] * paramSNMoE$lambda[k] * dik[is.infinite(E2ik[, k])]

        # weighted skew normal linear expert likelihood
        piik_fik[, k] <- piik[, k] * (2 / sigmak) * dnorm(dik, 0, 1) * pnorm(paramSNMoE$lambda[k] * dik)

      }

      log_piik_fik <<- log(piik_fik + .Machine$double.xmin)

      log_sum_piik_fik <<- matrix(log(rowSums(piik_fik)))

      # E[Zik|y,x] and E[U^2|y,zik=1]
      # tik <<- piik_fik / (rowSums(piik_fik) %*% ones(1, paramSNMoE$K))
      log_Tauik <- log_piik_fik - logsumexp(log_piik_fik, 1) %*% ones(1, paramSNMoE$K)
      tik <<- exp(log_Tauik)
    }
  )
)
