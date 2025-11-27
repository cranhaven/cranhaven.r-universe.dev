#' A Reference Class which contains statistics of a TMoE model.
#'
#' StatTMoE contains all the statistics associated to a [TMoE][ParamTMoE] model.
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
#' @field Ey Column matrix of dimension \emph{n} giving the estimated mean of the TMoE.
#' @field Var_yk Column matrix of dimension \emph{K} giving the estimated means of the experts.
#' @field Vary Column matrix of dimension \emph{n} giving the estimated variance of the response.
#' @field loglik Numeric. Observed-data log-likelihood of the TMoE model.
#' @field com_loglik Numeric. Complete-data log-likelihood of the TMoE model.
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
#' @field Wik Conditional expectations \eqn{w_{ik}}{wik}.
#' @seealso [ParamTMoE]
#' @export
StatTMoE <- setRefClass(
  "StatTMoE",
  fields = list(
    piik = "matrix",
    z_ik = "matrix",
    klas = "matrix",
    Wik = "matrix",
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
    tik = "matrix"
  ),
  methods = list(
    initialize = function(paramTMoE = ParamTMoE()) {
      piik <<- matrix(NA, paramTMoE$n, paramTMoE$K)
      z_ik <<- matrix(NA, paramTMoE$n, paramTMoE$K)
      klas <<- matrix(NA, paramTMoE$n, 1)
      Wik <<- matrix(0, paramTMoE$n, paramTMoE$K)
      Ey_k <<- matrix(NA, paramTMoE$n, paramTMoE$K)
      Ey <<- matrix(NA, paramTMoE$n, 1)
      Var_yk <<- matrix(NA, 1, paramTMoE$K)
      Vary <<- matrix(NA, paramTMoE$n, 1)
      loglik <<- -Inf
      com_loglik <<- -Inf
      stored_loglik <<- numeric()
      BIC <<- -Inf
      ICL <<- -Inf
      AIC <<- -Inf
      log_piik_fik <<- matrix(0, paramTMoE$n, paramTMoE$K)
      log_sum_piik_fik <<- matrix(NA, paramTMoE$n, 1)
      tik <<- matrix(0, paramTMoE$n, paramTMoE$K)
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

    computeStats = function(paramTMoE) {
      "Method used in the ECM algorithm to compute statistics based on
      parameters provided by the object \\code{paramTMoE} of class
      \\link{ParamTMoE}."

      # E[yi|xi,zi=k]
      Ey_k <<- paramTMoE$phiBeta$XBeta[1:paramTMoE$n,] %*% paramTMoE$beta

      # E[yi|xi]
      Ey <<- matrix(apply(piik * Ey_k, 1, sum))

      # Var[yi|xi,zi=k]
      Var_yk <<- paramTMoE$df / (paramTMoE$df - 2) * paramTMoE$sigma2

      # Var[yi|xi]
      Vary <<- apply(piik * (Ey_k ^ 2 + ones(paramTMoE$n, 1) %*% Var_yk), 1, sum) - Ey ^ 2

      # BIC, AIC and ICL
      BIC <<- loglik - (paramTMoE$df * log(paramTMoE$n) / 2)
      AIC <<- loglik - paramTMoE$df

      # CL(theta) : complete-data loglikelihood
      zik_log_piik_fk <- z_ik * log_piik_fik
      sum_zik_log_fik <- apply(zik_log_piik_fk, 1, sum)
      com_loglik <<- sum(sum_zik_log_fik)

      ICL <<- com_loglik - (paramTMoE$df * log(paramTMoE$n) / 2)

    },

    EStep = function(paramTMoE) {
      "Method used in the ECM algorithm to update statistics based on parameters
      provided by the object \\code{paramTMoE} of class \\link{ParamTMoE}
      (prior and posterior probabilities)."

      piik <<- multinomialLogit(paramTMoE$alpha, paramTMoE$phiAlpha$XBeta, ones(paramTMoE$n, paramTMoE$K), ones(paramTMoE$n, 1))$piik

      piik_fik <- zeros(paramTMoE$n, paramTMoE$K)

      for (k in (1:paramTMoE$K)) {

        muk <- paramTMoE$phiBeta$XBeta %*% paramTMoE$beta[, k]
        sigma2k <- paramTMoE$sigma2[k]
        sigmak <- sqrt(sigma2k)
        dik <- (paramTMoE$Y - muk) / sigmak

        nuk <- paramTMoE$nu[k]
        Wik[, k] <<- (nuk + 1) / (nuk + dik ^ 2)

        # Weighted t linear expert likelihood
        piik_fik[, k] <- piik[, k] *  (1 / sigmak * dt((paramTMoE$Y - muk) / sigmak, nuk)) #pdf('tlocationscale', y, muk, sigmak, nuk);
      }

      log_piik_fik <<- log(piik_fik + .Machine$double.xmin)

      log_sum_piik_fik <<- matrix(log(rowSums(piik_fik)))

      tik <<- piik_fik / (rowSums(piik_fik) %*% ones(1, paramTMoE$K))
    }
  )
)
