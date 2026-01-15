#' Function to simulate survival data from a mixture of normal distribution.
#'
#' `simulate_data()` simulates data from a mixture model.
#'
#' @param n Number of observations desired.
#'
#' @param mixture_components Number of mixtures to include in the generation of the data.
#'
#' @param k number of covariates generated (the total of covariates will be intercept + (k - 1) covariates).
#'
#' @param percentage_censored Percentage of censored observations (defined as decimal value between 0 and 1). This will generate a delta vector in which 1 is an event that ocurred and 0 is a censored observation.
#'
#' @param starting_seed Seed to start the random number generation.
#'
#' @returns A list with two elements: `data` and `real_values`. The `data` element is a tibble with the simulated data. The `real_values` is a tibble with the real values of the parameters used to generate the data.
#' 
#' @export
simulate_data <- function(n = 4000, mixture_components = 2, k = 2,
                          percentage_censored = 0.4,
                          starting_seed = sample(1:2^28, 1)) {
  if (!is.numeric(percentage_censored)) {
    stop("The parameter percentage_censored should be numeric.")
    if (percentage_censored < 0 | percentage_censored > 1) {
      stop("The parameter percentage_censored should be greater or equal than 0 and lower or equal to 1.")
    }
  }

  set.seed(starting_seed)

  betas <- matrix(nrow = mixture_components, ncol = k)

  for (c in 1:k) {
    if (c == 1) {
      betas[, c] <- round(stats::runif(mixture_components, 2, 5), 3)
    } else {
      for (r in 1:mixture_components) {
        betas[r, c] <- round(5 - betas[r, 1] + stats::rnorm(1, sd = 0.4), 3)
      }
    }
  }

  rownames_beta <- NULL

  for (g in 1:mixture_components) {
    rownames_beta[g] <- paste0("G", g)
  }

  rownames(betas) <- rownames_beta

  colnames_beta <- NULL

  for (par in 1:k) {
    colnames_beta[par] <- paste0("beta", (par - 1))
  }

  colnames(betas) <- colnames_beta

  phis <- stats::rgamma(mixture_components, 3, 0.8)
  etas <- stats::rgamma(mixture_components, 1, 1)
  etas <- etas / sum(etas)

  # Sorting the output
  phis <- phis[order(etas, decreasing = T)]
  betas <- betas[order(etas, decreasing = T), ]
  etas <- etas[order(etas, decreasing = T)]

  # Design matrix X
  X_design <- data.frame(cat = factor(sample(1:k, n, TRUE)))

  X <- stats::model.matrix(~cat, X_design) |> as.data.frame()

  # Iniciando base de dados
  data <- tibble::tibble(
    id = 1:n,
    grupo = sample(1:mixture_components, n, T, etas), # randomly allocating each observating to a group
    delta = stats::rbinom(n, 1, 1 - percentage_censored)
  )

  # Creating y variable
  data$y <- as.numeric(simulate_y(
    as.matrix(X), as.matrix(betas),
    phis, data$delta, data$grupo, starting_seed
  ))

  # Creating time till event ocurrence
  data$t <- exp(data$y)

  data <- data[, c("id", "grupo", "delta", "t")] |>
    dplyr::bind_cols(tibble::as_tibble(X_design))

  # Exporting real parameters values
  new_params_theta <- NULL

  # Nome dos parâmetros
  for (g in 1:mixture_components) {
    new_params_theta <- c(
      new_params_theta,
      paste0("eta_", g)
    )

    for (c in 1:k) {
      new_params_theta <- c(
        new_params_theta,
        paste0("beta", c - 1, "_", g)
      )
    }

    new_params_theta <- c(
      new_params_theta,
      paste0("phi_", g)
    )
  }

  new_row_theta <- NULL

  for (g in 1:mixture_components) {
    new_row_theta <- c(new_row_theta, etas[g])

    for (c in 1:k) {
      new_row_theta <- c(
        new_row_theta,
        betas[g, c]
      )
    }

    new_row_theta <- c(new_row_theta, phis[g])
  }

  # Criando matriz com os valores da iteração atual
  new_params_theta <- matrix(c(new_row_theta),
    nrow = 1,
    ncol = length(new_row_theta),
    dimnames = list("r1", new_params_theta)
  ) |>
    tibble::as_tibble()

  return(list(
    data = data,
    real_values = new_params_theta
  ))
}
