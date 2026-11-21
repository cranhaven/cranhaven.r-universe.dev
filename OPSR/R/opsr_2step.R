#' Heckman Two-Step Estimation
#'
#' This is a utility function, used in [`opsr`] and should not be used directly.
#' Tow-step estimation procedure to generate reasonable starting values.
#'
#' @param W matrix with explanatory variables for selection process.
#' @param Xs list of matrices with expalanatory varialbes for outcome process for each regime.
#' @param Z vector with ordinal outcomes (in integer increasing fashion).
#' @param Ys list of vectors with continuous outcomes for each regime.
#'
#' @return Named vector with starting values passed to [`opsr.fit`].
#'
#' @section Remark:
#' Since the Heckman two-step estimator includes an estimate in the second step
#' regression, the resulting OLS standard errors and heteroskedasticity-robust
#' standard errors are incorrect \insertCite{Greene:2002}{OPSR}.
#'
#' @details
#' These estimates can be retrieved by specifying `.get2step = TRUE` in [`opsr`].
#'
#' @references
#' \insertRef{Greene:2002}{OPSR}
#'
#' @seealso [`opsr.fit`], [`opsr_prepare_coefs`]
#' @export
opsr_2step <- function(W, Xs, Z, Ys) {
  nReg <- length(Xs)

  fit_sel <- function() {
    fit_selection <- suppressWarnings(
      MASS::polr(factor(Z) ~ W, method = "probit")
    )

    kappa <- unname(fit_selection$zeta)
    names(kappa) <- paste0("kappa", 1:length(kappa))
    kappa_ <- c(-Inf, kappa, Inf)  # add boundaries for step 2

    gamma <- unname(fit_selection$coefficients)
    names(gamma) <- paste0("s_", colnames(W))
    list(kappa = kappa, kappa_ = kappa_, gamma = gamma)
  }

  ## MASS::polr() errors if Z has only 2 levels
  fit_sel2 <- function() {
    fit_selection <- suppressWarnings(
      stats::glm(factor(Z) ~ W, family = stats::binomial(link = "probit"))
    )

    kappa <- unname(fit_selection$coefficients[1])
    names(kappa) <- "kappa1"
    kappa_ <- c(-Inf, kappa, Inf)
    gamma <- unname(fit_selection$coefficients[-1])
    names(gamma) <- paste0("s_", colnames(W))
    list(kappa = kappa, kappa_ = kappa_, gamma = gamma)
  }

  ## step 1
  if (nReg == 2) {
    step1 <- fit_sel2()
  } else {
    step1 <- fit_sel()
  }
  kappa <- step1$kappa
  kappa_ <- step1$kappa_
  gamma <- step1$gamma


  ## step 2
  step2 <- function(x, imr, y) {
    fit <- suppressWarnings(
      stats::lm(y ~ -1 + x + imr)  # intercept already included in x
    )
    params <- coefficients(fit)
    beta <- unname(params[!(names(params) %in% "imr")])
    k <- length(params - 1)
    SSE <- sum(residuals(fit)**2)
    n <- length(residuals(fit))
    sigma <- sqrt(SSE / (n - (1 + k)))
    rho <- params[["imr"]] / sigma
    list(beta = beta, sigma = sigma, rho = rho)
  }

  ## inverse mills ratio
  W_gamma <- W %*% gamma
  Tj_1 <- kappa_[Z]
  Tj <- kappa_[Z + 1]
  IMR <- -(stats::dnorm(Tj-W_gamma) - stats::dnorm(Tj_1-W_gamma))/(stats::pnorm(Tj-W_gamma) - stats::pnorm(Tj_1-W_gamma))
  IMR_j <- lapply(seq_len(nReg), function(j) IMR[Z == j])

  ## apply
  params_o <- lapply(seq_len(nReg), function(j) {
    x <- Xs[[j]]
    imr <- IMR_j[[j]]
    y <- Ys[[j]]
    step2(x, imr, y)
  })

  ## prepare output vector
  beta <- unlist(lapply(seq_len(nReg), function(j) {
    beta_j <- params_o[[j]]$beta
    names(beta_j) <- paste0("o", j, "_", colnames(Xs[[j]]))
    beta_j
  }))

  sigma <- unlist(lapply(seq_len(nReg), function(j) {
    params_o[[j]]$sigma
  }))
  names(sigma) <- paste0("sigma", 1:nReg)

  rho <- unlist(lapply(seq_len(nReg), function(j) {
    params_o[[j]]$rho
  }))
  names(rho) <- paste0("rho", 1:nReg)

  c(kappa, gamma, beta, sigma, rho)
}
