


# estimation function that takes covariance matrix, thresholds, estimation method, tolerance, out, and estimation specific parameters such as burn_in and return estimates of genetic and/or full liability

estimation_function = function(tbl, cov, out = NA, tol = 0.01, method = NA, burn_in = NA, useMixture = FALSE, target_id = NA) {
  if (is.na(method)) {
    stop("Please specify the estimation method.")
  }

  if (method == "Gibbs") {
    res <- Gibbs_estimator(cov = cov, tbl = tbl, out = out, tol = tol, burn_in = burn_in)
  }

  if (method == "PA") {
    if (useMixture) {
      # note: if useMixture is TRUE, K_i and K_pop are expected to be in tbl as columns.
      res <- PA_algorithm(
        mu = rep(0, nrow(cov)),
        covmat = cov,
        lower = pull(tbl, lower),
        upper = pull(tbl, upper),
        K_i = pull(tbl, K_i),
        K_pop = pull(tbl, K_pop),
        target_id = target_id)
    } else {
      res <- PA_algorithm(
        mu = rep(0, nrow(cov)),
        covmat = cov,
        lower = pull(tbl, lower),
        upper = pull(tbl, upper),
        K_i = rep(NA, nrow(tbl)),
        K_pop = rep(NA, nrow(tbl)),
        target_id = target_id)
    }

  }
  return(res)
}


#' Wrapper around the Gibbs Sampler that returns formatted liability estimates for the proband
#'
#' @param cov Covariance (kinship matrix times heritability with corrected diagonal) matrix
#' @param tbl Tibble with lower and upper bounds for the Gibbs sampler
#' @param out Vector indicating if genetic ans/or full liabilities should be estimated
#' @param tol Convergence criteria, tolerance
#' @param burn_in Number of burn-in iterations
#'
#' @returns Formatted liability estimate(s) and standard error(s) of the mean for the proband.
#' @export
#'
#' @importFrom dplyr pull
#' @examples
#' # uninformative sampling:
#' Gibbs_estimator(cov = diag(3), tbl = tibble::tibble(lower = rep(-Inf, 3),
#' upper = rep(Inf, 3)), out = 1:2, tol = 0.01, burn_in = 1000)
Gibbs_estimator = function(cov, tbl, out, tol = 0.01, burn_in = 1000) {

  # Setting the variables needed for Gibbs sampler
  fixed <- (pull(tbl,upper) - pull(tbl,lower)) < 1e-04
  std_err <- rep(Inf, length(out))
  names(std_err) <- c("genetic", "full")[out]
  n_gibbs <- 1


  # Running Gibbs sampler
  while (any(std_err > tol)) {

    if (n_gibbs == 1) {

      est_liabs <- rtmvnorm.gibbs(n_sim = 1e+05, covmat = cov, lower = pull(tbl, lower), upper = pull(tbl, upper),
                                  fixed = fixed, out = out, burn_in = burn_in) %>%
        `colnames<-`(c("genetic", "full")[out]) %>%
        tibble::as_tibble()

    } else {

      est_liabs <- rtmvnorm.gibbs(n_sim = 1e+05, covmat = cov, lower = pull(tbl, lower), upper = pull(tbl, upper),
                                  fixed = fixed, out = out, burn_in = burn_in) %>%
        `colnames<-`(c("genetic", "full")[out]) %>%
        tibble::as_tibble() %>%
        bind_rows(est_liabs)
    }

    # Computing the standard error
    std_err <- batchmeans::bmmat(est_liabs)[,2]
    # Adding one to the counter
    n_gibbs <- n_gibbs + 1
  }

  # If all standard errors are below the tolerance,
  # the estimated liabilities as well as the corresponding
  # standard error can be returned
  res <- tibble::as_tibble(batchmeans::bmmat(est_liabs), rownames = "out") %>%
    tidyr::pivot_longer(., cols = c(est,se)) %>%
    mutate(name = paste0(out, "_", name), .keep = "unused") %>%
    tidyr::pivot_wider()
  return(res)
}

