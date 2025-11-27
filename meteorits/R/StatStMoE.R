#' A Reference Class which contains statistics of a StMoE model.
#'
#' StatStMoE contains all the statistics associated to a [StMoE][ParamStMoE]
#' model. It mainly includes the E-Step of the ECM algorithm calculating the
#' posterior distribution of the hidden variables, as well as the calculation of
#' the log-likelhood.
#'
#' @field piik Matrix of size \eqn{(n, K)} representing the probabilities
#'   \eqn{\pi_{k}(x_{i}; \boldsymbol{\Psi}) = P(z_{i} = k | \boldsymbol{x};
#'   \Psi)}{\pi_{k}(x_{i}; \Psi) = P(z_{i} = k | x; \Psi)} of the latent
#'   variable \eqn{z_{i}, i = 1,\dots,n}.
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
#' @field Ey_k Matrix of dimension \emph{(n, K)} giving the estimated means of
#'   the experts.
#' @field Ey Column matrix of dimension \emph{n} giving the estimated mean of the StMoE.
#' @field Var_yk Column matrix of dimension \emph{K} giving the estimated means
#'   of the experts.
#' @field Vary Column matrix of dimension \emph{n} giving the estimated variance
#'   of the response.
#' @field loglik Numeric. Observed-data log-likelihood of the StMoE model.
#' @field com_loglik Numeric. Complete-data log-likelihood of the StMoE model.
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
#' @field dik It represents the value of \eqn{d_{ik}}{dik}.
#' @field wik Conditional expectations \eqn{w_{ik}}{wik}.
#' @field E1ik Conditional expectations \eqn{e_{1,ik}}{e1ik}.
#' @field E2ik Conditional expectations \eqn{e_{2,ik}}{e2ik}.
#' @field E3ik Conditional expectations \eqn{e_{3,ik}}{e3ik}.
#' @field stme_pdf Skew-t mixture of experts density.
#' @seealso [ParamStMoE]
#' @export
StatStMoE <- setRefClass(
  "StatStMoE",
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
    wik = "matrix",
    dik = "matrix",
    stme_pdf = "matrix",
    E1ik = "matrix",
    E2ik = "matrix",
    E3ik = "matrix"
  ),
  methods = list(
    initialize = function(paramStMoE = ParamStMoE()) {
      piik <<- matrix(NA, paramStMoE$n, paramStMoE$K)
      z_ik <<- matrix(NA, paramStMoE$n, paramStMoE$K)
      klas <<- matrix(NA, paramStMoE$n, 1)
      Ey_k <<- matrix(NA, paramStMoE$n, paramStMoE$K)
      Ey <<- matrix(NA, paramStMoE$n, 1)
      Var_yk <<- matrix(NA, 1, paramStMoE$K)
      Vary <<- matrix(NA, paramStMoE$n, 1)
      loglik <<- -Inf
      com_loglik <<- -Inf
      stored_loglik <<- numeric()
      BIC <<- -Inf
      ICL <<- -Inf
      AIC <<- -Inf
      log_piik_fik <<- matrix(0, paramStMoE$n, paramStMoE$K)
      log_sum_piik_fik <<- matrix(NA, paramStMoE$n, 1)
      tik <<- matrix(0, paramStMoE$n, paramStMoE$K)
      wik <<- matrix(0, paramStMoE$n, paramStMoE$K)
      dik <<- matrix(0, paramStMoE$n, paramStMoE$K)
      stme_pdf <<- matrix(0, paramStMoE$n, 1)
      E1ik <<- matrix(0, paramStMoE$n, paramStMoE$K)
      E2ik <<- matrix(0, paramStMoE$n, paramStMoE$K)
      E3ik <<- matrix(0, paramStMoE$n, paramStMoE$K)
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
      z_ik <<-
        ikmax %*% ones(1, K) == ones(N, 1) %*% (1:K) # partition_MAP
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

    computeStats = function(paramStMoE) {
      "Method used in the ECM algorithm to compute statistics based on
      parameters provided by the object \\code{paramStMoE} of class
      \\link{ParamStMoE}."

      Xi_nuk = sqrt(paramStMoE$nu / pi) * (gamma(paramStMoE$nu / 2 - 1 / 2)) / (gamma(paramStMoE$nu / 2))

      # E[yi|xi,zi=k]
      Ey_k <<- paramStMoE$phiBeta$XBeta[1:paramStMoE$n,] %*% paramStMoE$beta + ones(paramStMoE$n, 1) %*% (paramStMoE$delta * sqrt(paramStMoE$sigma2) * Xi_nuk)

      # E[yi|xi]
      Ey <<- matrix(apply(piik * Ey_k, 1, sum))

      # Var[yi|xi,zi=k]
      Var_yk <<- (paramStMoE$nu / (paramStMoE$nu - 2) - (paramStMoE$delta ^ 2) * (Xi_nuk ^ 2)) * paramStMoE$sigma2

      # Var[yi|xi]
      Vary <<- apply(piik * (Ey_k ^ 2 + ones(paramStMoE$n, 1) %*% Var_yk), 1, sum) - Ey ^ 2

      # BIC, AIC and ICL
      BIC <<- loglik - (paramStMoE$df * log(paramStMoE$n) / 2)
      AIC <<- loglik - paramStMoE$df

      ## CL(theta) : complete-data loglikelihood
      zik_log_piik_fk <- z_ik * log_piik_fik
      sum_zik_log_fik <- apply(zik_log_piik_fk, 1, sum)
      com_loglik <<- sum(sum_zik_log_fik)

      ICL <<- com_loglik - (paramStMoE$df * log(paramStMoE$n) / 2)

    },

    univStMoEpdf = function(paramStMoE) {
      piik <<- multinomialLogit(paramStMoE$alpha, paramStMoE$phiAlpha$XBeta, ones(paramStMoE$n, paramStMoE$K), ones(paramStMoE$n, 1))$piik

      piik_fik <- zeros(paramStMoE$n, paramStMoE$K)
      dik <<- zeros(paramStMoE$n, paramStMoE$K)
      mik <- zeros(paramStMoE$n, paramStMoE$K)

      for (k in (1:paramStMoE$K)) {
        dik[, k] <<- (paramStMoE$Y - paramStMoE$phiBeta$XBeta %*% paramStMoE$beta[, k]) / sqrt(paramStMoE$sigma2[k])
        mik[, k] <- paramStMoE$lambda[k] %*% dik[, k] * sqrt(paramStMoE$nu[k] + 1) / (paramStMoE$nu[k] + dik[, k] ^ 2)
        piik_fik[, k] <- piik[, k] * (2 / sqrt(paramStMoE$sigma2[k])) * dt(dik[, k], paramStMoE$nu[k]) * pt(mik[, k], paramStMoE$nu[k] + 1)
      }

      stme_pdf <<- matrix(rowSums(piik_fik)) # Skew-t mixture of experts density
    },

    EStep = function(paramStMoE, calcTau = FALSE, calcE1 = FALSE, calcE2 = FALSE, calcE3 = FALSE) {
      "Method used in the ECM algorithm to update statistics based on parameters
      provided by the object \\code{paramStMoE} of class \\link{ParamStMoE}
      (prior and posterior probabilities)."

      if (calcTau) {

        piik <<- multinomialLogit(paramStMoE$alpha, paramStMoE$phiAlpha$XBeta, ones(paramStMoE$n, paramStMoE$K), ones(paramStMoE$n, 1))$piik
        piik_fik <- zeros(paramStMoE$n, paramStMoE$K)
      }

      dik <<- zeros(paramStMoE$n, paramStMoE$K)
      mik <- zeros(paramStMoE$n, paramStMoE$K)
      wik <<- zeros(paramStMoE$n, paramStMoE$K)
      Integgtx <- zeros(paramStMoE$n, paramStMoE$K)

      for (k in (1:paramStMoE$K)) {

        fx = function(x) {
          return((psigamma((paramStMoE$nu[k] + 2) / 2) - psigamma((paramStMoE$nu[k] + 1) / 2) + log(1 + (x ^ 2) / (paramStMoE$nu[k])) + ((paramStMoE$nu[k] + 1) * x ^ 2 - paramStMoE$nu[k] - 1) / ((paramStMoE$nu[k] + 1) * (paramStMoE$nu[k] + 1 + x ^ 2))) * dt(x, paramStMoE$nu[k] + 1))
        }


        muk <- paramStMoE$phiBeta$XBeta %*% paramStMoE$beta[, k]
        sigmak <- sqrt(paramStMoE$sigma2[k])
        deltak <- paramStMoE$delta[k]


        dik[, k] <<- (paramStMoE$Y - muk) / sigmak
        mik[, k] <- paramStMoE$lambda[k] %*% dik[, k] * sqrt((paramStMoE$nu[k] + 1) / (paramStMoE$nu[k] + dik[, k] ^ 2))

        # E[Wi|yi,zik=1]
        wik[, k] <<- ((paramStMoE$nu[k] + 1) / (paramStMoE$nu[k] + dik[, k] ^ 2)) * pt(mik[, k] * sqrt((paramStMoE$nu[k] + 3) / (paramStMoE$nu[k] + 1)), paramStMoE$nu[k] + 3) / pt(mik[, k], paramStMoE$nu[k] + 1)


        if (calcE1) {

          univStMoEpdf(paramStMoE)
          # E[Wi Ui |yi,zik=1]
          E1ik[, k] <<- deltak * abs(paramStMoE$Y - muk) * wik[, k] +
            (sqrt(1 - deltak ^ 2) / (pi * stme_pdf)) * ((dik[, k] ^ 2 / (paramStMoE$nu[k] * (1 - deltak ^ 2)) + 1) ^ (-(paramStMoE$nu[k] / 2 + 1)))
        }

        if (calcE2) {

          E2ik[, k] <<- deltak ^ 2 * ((paramStMoE$Y - muk) ^ 2) * wik[, k] +
            (1 - deltak ^ 2) * sigmak ^ 2 + ((deltak * (paramStMoE$Y - muk) * sqrt(1 - deltak ^ 2)) / (pi * stme_pdf)) * (((dik[, k] ^ 2) / (paramStMoE$nu[k] * (1 - deltak ^ 2)) + 1) ^ (-(paramStMoE$nu[k] / 2 + 1)))
        }

        if (calcE3) {

          Integgtx[, k] <- as.numeric(sapply(mik[, k], function(x) try(integrate(f = fx, lower = -Inf, upper = x)$value, silent = TRUE)))
          E3ik[, k] <<- wik[, k] - log((paramStMoE$nu[k] + dik[, k] ^ 2) / 2) - (paramStMoE$nu[k] + 1) / (paramStMoE$nu[k] + dik[, k] ^ 2) + psigamma((paramStMoE$nu[k] + 1) / 2) + ((paramStMoE$lambda[k] * dik[, k] * (dik[, k] ^ 2 - 1)) / sqrt((paramStMoE$nu[k] + 1) * ((paramStMoE$nu[k] + dik[, k] ^ 2) ^ 3))) * dt(mik[, k], paramStMoE$nu[k] + 1) / pt(mik[, k], paramStMoE$nu[k] + 1) + (1 / pt(mik[, k], paramStMoE$nu[k] + 1)) * Integgtx[, k]
        }

        if (calcTau) {

          # Weighted skew normal linear expert likelihood
          piik_fik[, k] <- piik[, k] * (2 / sigmak) * dt(dik[, k], paramStMoE$nu[k]) * pt(mik[, k], paramStMoE$nu[k] + 1)

        }

      }

      if (calcTau) {

        stme_pdf <<- matrix(rowSums(piik_fik)) # Skew-t mixture of experts density

        log_piik_fik <<- log(piik_fik + .Machine$double.xmin)

        log_sum_piik_fik <<- matrix(logsumexp(log_piik_fik, 1))

        #E[Zi=k|yi]
        log_tik <- log_piik_fik - log_sum_piik_fik %*% ones(1, paramStMoE$K)
        tik <<- exp(log_tik)
      }

    }
  )
)
