#' Random Number Generation from the EVBS Distribution
#'
#' Generates random variates from the Extreme-Value Birnbaum-Saunders
#' (EVBS) distribution by transforming Generalized Extreme Value (GEV)
#' variates.
#'
#' @param n Sample size (number of variates to generate).
#' @param alpha Shape parameter \eqn{\alpha > 0}.
#' @param beta Scale parameter \eqn{\beta > 0}.
#' @param gama Tail-shape parameter \eqn{\gamma} (real).
#'
#' @return A numeric vector of \code{n} EVBS variates. Returns \code{NA}
#'   if \code{n} is \code{NA}.
#'
#' @details
#' If \eqn{Z \sim \mathrm{GEV}(0, 1, \gamma)}, then
#' \eqn{T = \beta\{1 + \alpha^2 Z^2/2 + \alpha Z \sqrt{1 + \alpha^2 Z^2/4}\}}
#' follows the EVBS distribution. GEV variates are drawn via
#' \code{rgev} from the \pkg{SpatialExtremes} package.
#'
#' @references
#' Ferreira, M., Gomes, M. I., and Leiva, V. (2012). On an extreme value
#' version of the Birnbaum-Saunders distribution. \emph{REVSTAT}, 10,
#' 181--210.
#'
#' @seealso \code{\link{evbsreg.fit.mc}}, \code{\link{evbsreg.fit}}.
#'
#' @examples
#' set.seed(2023)
#' x <- revbs(100, alpha = 0.5, beta = 1, gama = 0.2)
#' summary(x)
#'
#' @importFrom SpatialExtremes rgev
#' @export
revbs <- function(n, alpha, beta, gama) {
  if (is.na(n)) return(NA)
  z <- SpatialExtremes::rgev(n, 0, 1, gama)
  beta * (1 + ((alpha^2 * z^2) / 2) +
            alpha * z * sqrt(1 + ((alpha^2 * z^2) / 4)))
}


#' Monte Carlo Simulation Study for the EVBS Regression Model
#'
#' Runs a Monte Carlo simulation that evaluates the finite-sample
#' properties of the joint maximum likelihood estimator of the EVBS
#' regression model under one of three scenarios.
#'
#' @param m Number of Monte Carlo replicates (the paper uses \code{5000};
#'   set to a smaller value such as \code{500} for a quick check).
#' @param n Sample size for each replicate (the paper uses 60, 120, 180).
#' @param beta0 True intercept \eqn{\beta_0}.
#' @param beta1 True slope \eqn{\beta_1}.
#' @param alpha True shape parameter \eqn{\alpha}.
#' @param gama True tail-shape parameter \eqn{\gamma}.
#' @param scenario Character string selecting the design:
#'   \describe{
#'     \item{\code{"canonical"}}{covariate \eqn{x \sim U(0,1)}, no
#'       contamination (baseline).}
#'     \item{\code{"leverage"}}{10\% of covariate values drawn from
#'       \eqn{U(5,10)} to introduce high-leverage points.}
#'     \item{\code{"robustness"}}{10\% of observations generated with the
#'       shape parameter shifted by \eqn{-0.5} (alpha contamination).}
#'   }
#' @param semente Integer RNG seed for reproducibility (default
#'   \code{2023}).
#'
#' @return A numeric matrix of dimension \eqn{10 \times 4}. Columns are the
#'   four parameters \code{Beta0}, \code{Beta1}, \code{Alpha}, \code{Gama};
#'   rows are: \code{Parametro} (true value), \code{EMV} (mean estimate),
#'   \code{VIES-ABS} (absolute bias), \code{VIES-REL} (relative bias),
#'   \code{VAR} (empirical variance), \code{EQM} (mean squared error),
#'   \code{E-PADRAO} (empirical standard error), \code{EP-FISHER} (mean
#'   Fisher standard error), \code{RAIZ-EQM} (root mean squared error), and
#'   \code{TAXA-COB} (empirical 95\% coverage rate).
#'
#' @references
#' Ospina, R., Lima, J. I. C., Barros, M., and Macedo, A. M. S. (2026).
#' Local influence diagnostics for the extreme-value Birnbaum-Saunders
#' regression model. \emph{Submitted}.
#'
#' @seealso \code{\link{revbs}}, \code{\link{evbsreg.fit}}.
#'
#' @examples
#' \donttest{
#' ## Quick check with m = 50 replicates
#' res <- evbsreg.fit.mc(m = 50, n = 60,
#'                        beta0 = 0.5, beta1 = 0.5,
#'                        alpha = 0.5, gama = 0.20,
#'                        scenario = "canonical")
#' print(res)
#' }
#'
#' @importFrom stats runif qnorm
#' @export
evbsreg.fit.mc <- function(m, n, beta0, beta1, alpha, gama,
                            scenario = c("canonical", "leverage", "robustness"),
                            semente = 2023) {

  scenario <- match.arg(scenario)
  set.seed(semente)

  # Build covariate and (per-obs) alpha vector based on scenario
  if (scenario == "canonical") {
    x       <- runif(n)
    ttalpha <- rep(alpha, n)
  } else if (scenario == "leverage") {
    p <- 0.90
    r <- round(n * p); s <- n - r
    x1 <- runif(r)
    x2 <- runif(s, 5, 10)
    x  <- c(x1, x2)
    ttalpha <- rep(alpha, n)
  } else { # robustness (contamination)
    p <- 0.90
    d <- -0.5
    r <- round(n * p); s <- n - r
    x1 <- runif(r)
    x2 <- runif(s)
    x  <- c(x1, x2)
    talpha1 <- rep(alpha,     r)
    talpha2 <- rep(alpha + d, s)
    ttalpha <- c(talpha1, talpha2)
  }

  est     <- matrix(0, m, 4)
  viesabs <- matrix(0, m, 4)
  eqm     <- matrix(0, m, 4)
  erropd  <- matrix(0, m, 4)
  ind     <- matrix(0, m, 4)

  umx     <- cbind(rep(1, n), x)
  tbeta0  <- beta0; tbeta1 <- beta1; talpha <- alpha; tgama <- gama

  t <- rep(0, n)

  i <- 1
  while (i <= m) {
    for (j in seq_len(n)) {
      t[j] <- revbs(1, ttalpha[j], exp(tbeta0 + tbeta1 * x[j]), tgama)
    }
    out <- tryCatch(suppressWarnings(evbsreg.fit(umx, t)),
                    error = function(e) NULL)
    if (is.null(out)) next  # skip failed replicate

    b0 <- out$betahat[1]; b1 <- out$betahat[2]
    al <- out$alphahat;   gm <- out$gamahat

    est[i, ]     <- c(b0, b1, al, gm)
    viesabs[i, ] <- c(b0 - tbeta0, b1 - tbeta1, al - talpha, gm - tgama)
    eqm[i, ]     <- c((b0 - tbeta0)^2, (b1 - tbeta1)^2,
                      (al - talpha)^2, (gm - tgama)^2)

    se_b0 <- out$stderrors[1]; se_b1 <- out$stderrors[2]
    se_al <- out$stderroralpha; se_gm <- out$stderrorgama
    erropd[i, ] <- c(se_b0, se_b1, se_al, se_gm)

    ind[i, ] <- as.integer(
      c(abs((b0 - tbeta0) / se_b0),
        abs((b1 - tbeta1) / se_b1),
        abs((al - talpha) / se_al),
        abs((gm - tgama)  / se_gm)) < qnorm(0.975)
    )

    i <- i + 1
  }

  mle <- colMeans(est)
  vies <- colMeans(viesabs)
  relvies <- c(
    mean((est[, 1] - tbeta0) / tbeta0),
    mean((est[, 2] - tbeta1) / tbeta1),
    mean((est[, 3] - talpha) / talpha),
    mean((est[, 4] - tgama)  / tgama)
  )
  varhat <- c(
    mean((est[, 1] - mle[1])^2),
    mean((est[, 2] - mle[2])^2),
    mean((est[, 3] - mle[3])^2),
    mean((est[, 4] - mle[4])^2)
  )
  se_emp <- sqrt(varhat)
  se_fisher <- colMeans(erropd)
  eqm_mean <- colMeans(eqm)
  rmse <- sqrt(eqm_mean)
  cob <- colMeans(ind)

  true_pars <- c(tbeta0, tbeta1, talpha, tgama)
  rownames_table <- c("Parametro","EMV","VIES-ABS","VIES-REL","VAR",
                      "EQM","E-PADRAO","EP-FISHER","RAIZ-EQM","TAXA-COB")

  res <- rbind(true_pars, mle, vies, relvies, varhat, eqm_mean,
               se_emp, se_fisher, rmse, cob)
  rownames(res) <- rownames_table
  colnames(res) <- c("Beta0", "Beta1", "Alpha", "Gama")
  round(res, 3)
}
