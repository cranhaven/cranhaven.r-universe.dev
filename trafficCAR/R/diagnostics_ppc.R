#' Posterior predictive checks for trafficCAR fits
#'
#' Computes simple posterior predictive checks comparing observed
#' statistics to replicated data: mean, variance, and tail probabilities.
#'
#' @param fit A `traffic_fit` object.
#' @param stats Statistics to compute: "mean", "var", "tail".
#' @param probs Tail probabilities for "tail" statistic.
#' @importFrom stats var
#'
#' @return An object of class `traffic_ppc`.
#' @export
ppc_summary <- function(fit,
                        stats = c("mean", "var", "tail"),
                        probs = c(0.05, 0.95)) {
  stats <- match.arg(stats, several.ok = TRUE)

  y <- fit$y
  if (is.null(y)) stop("`fit` must contain observed data `y`.")

  mu_draws <- fit$mu_draws
  if (is.null(mu_draws) && !is.null(fit$samples)) {
    mu_draws <- fit$samples$mu
  }

  if (is.null(mu_draws)) {
    stop("Posterior draws of `mu` not found in `fit`.")
  }

  if (!is.matrix(mu_draws)) {
    stop("`mu_draws` must be a matrix (draws x observations).")
  }

  n_draws <- nrow(mu_draws)
  n <- length(y)

  if (ncol(mu_draws) != n) {
    stop("Dimensions of `mu_draws` do not match `y`.")
  }

  out_obs <- list()
  out_rep <- list()
  pvals <- list()

  if ("mean" %in% stats) {
    obs <- mean(y)
    rep <- rowMeans(mu_draws)

    out_obs$mean <- obs
    out_rep$mean <- rep
    pvals$mean <- mean(rep >= obs)
  }

  if ("var" %in% stats) {
    obs <- var(y)
    rep <- apply(mu_draws, 1, var)

    out_obs$var <- obs
    out_rep$var <- rep
    pvals$var <- mean(rep >= obs)
  }

  if ("tail" %in% stats) {
    obs_tail <- sapply(probs, function(p) {
      mean(y <= quantile(y, p))
    })

    rep_tail <- sapply(probs, function(p) {
      apply(mu_draws, 1, function(r) mean(r <= quantile(r, p)))
    })

    out_obs$tail <- obs_tail
    out_rep$tail <- rep_tail
    pvals$tail <- apply(rep_tail, 2, function(x, obs)
      mean(x >= obs), obs = obs_tail)
  }

  out <- list(
    observed = out_obs,
    replicated = out_rep,
    p_values = pvals,
    stats = stats,
    probs = probs,
    n_draws = n_draws
  )
  class(out) <- "traffic_ppc"
  out
}




#' @method print traffic_ppc
#' @export
print.traffic_ppc <- function(x, ...) {
  cat("Posterior predictive checks\n")
  cat("Draws:", x$n_draws, "\n\n")

  for (nm in names(x$observed)) {
    cat(nm, ":\n")
    print(c(
      observed = x$observed[[nm]],
      p_value = x$p_values[[nm]]
    ))
    cat("\n")
  }

  invisible(x)
}


