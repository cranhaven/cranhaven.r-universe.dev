#' Lognormal mixture model - Gibbs sampler
#'
#' `survival_ln_mixture()` fits a Bayesian lognormal mixture model with Gibbs sampling (optional EM algorithm to find local maximum at the likelihood function), as described in LOBO, Viviana GR; FONSECA, Thaís CO; ALVES, Mariane B. Lapse risk modeling in insurance: a Bayesian mixture approach. Annals of Actuarial Science, v. 18, n. 1, p. 126-151, 2024.
#'
#' @param formula A formula specifying the outcome terms on the left-hand side,
#' and the predictor terms on the right-hand side. The outcome must be a [survival::Surv]
#' object.
#'
#' @param data A __data frame__ containing both the predictors and the outcome.
#'
#' @param intercept A logical. Should an intercept be included in the processed data?
#'
#' @param iter A positive integer specifying the number of iterations for each chain (including warmup).
#'
#' @param em_iter A positive integer specifying the number of iterations for the EM algorithm. The EM algorithm is performed before the Gibbs sampler to find better initial values for the chains. On simulations, values lower than 200 seems to work nice.
#'
#' @param warmup A positive integer specifying the number of warmup (aka burnin) iterations per chain.
#' The number of warmup iterations should be smaller than iter.
#'
#' @param thin A positive integer specifying the period for saving samples.
#'
#' @param chains A positive integer specifying the number of Markov chains.
#'
#' @param cores A positive integer specifying the maximum number of cores to run the chains. Setting this to a value bigger than 1 will automatically trigger the parallel mode
#'
#' @param mixture_components number of mixture componentes >= 2.
#'
#' @param show_progress Indicates if the code shows the progress of the EM algorithm and the Gibbs Sampler.
#'
#' @param starting_seed Starting seed for the sampler. If not specified by the user, uses a random integer between 1 and 2^28 This way we ensure, when the user sets a seed in R, that this is passed into the C++ code.
#'
#' @param use_W Specifies is the W (groups weight's matrix for each observation) should be used from EM. It holds W constant through the code, resulting in a faster Bayesian Inference (close to what Empirical Bayes would do). It may helps generating credible intervals for the survival and hazard curves, using the information from the previous EM iteration. Make sure the EM have converged before setting this parameter to true. In doubt, leave this as FALSE, the default.
#'
#' @param number_em_search Number of different EM's to search for maximum likelihoods. Recommended to leave, at least, at 100. This value can be set to 0 to disable the search for maximum likelihood initial values.
#'
#' @param iteration_em_search Number of iterations for each of the EM's used to find the maximum likelihoods. Recommended to leave at small values, such as from 1 to 5.
#'
#' @param fast_groups Use fast computation of groups allocations probabilities, defaults to TRUE. Setting it to FALSE can increase the computation time (a lot) but it's worth trying if the chains are not converging.
#'
#' @param ... Not currently used, but required for extensibility.
#'
#' @note Categorical predictors must be converted to factors before the fit,
#' otherwise the predictions will fail.
#'
#' @return
#'
#' A `survival_ln_mixture` object, which is a list with the following componentes:
#'
#' \item{posterior}{A [posterior::draws_matrix] with the posterior of the parameters of the model.}
#' \item{nobs}{A integer holding the number of observations used to generate the fit.}
#' \item{blueprint}{The blueprint component of the output of [hardhat::mold]}
#'
#'
#' @examples
#'
#' # Formula interface
#' library(survival)
#' set.seed(1)
#' mod <- survival_ln_mixture(Surv(time, status == 2) ~ NULL, lung, intercept = TRUE)
#'
#' @export
survival_ln_mixture <- function(formula, data, intercept = TRUE, iter = 1000, warmup = floor(iter / 10), thin = 1, chains = 1, cores = 1, mixture_components = 2, show_progress = FALSE, em_iter = 0, starting_seed = sample(1:2^28, 1), use_W = FALSE, number_em_search = 200, iteration_em_search = 1, fast_groups = TRUE, ...) {
  rlang::check_dots_empty(...)
  UseMethod("survival_ln_mixture")
}

#' @export
#' @rdname survival_ln_mixture
survival_ln_mixture.default <- function(formula, ...) {
  stop("`survival_ln_mixture()` is not defined for a '", class(formula)[1], "'.", call. = FALSE)
}

# Formula method
#' @export
#' @rdname survival_ln_mixture
survival_ln_mixture.formula <- function(formula, data, intercept = TRUE, ...) {
  blueprint <- hardhat::default_formula_blueprint(intercept = intercept)
  processed <- hardhat::mold(formula, data, blueprint = blueprint)
  survival_ln_mixture_bridge(processed, ...)
}

# ------------------------------------------------------------------------------
# Bridge

survival_ln_mixture_bridge <- function(processed, ...) {
  predictors <- as.matrix(processed$predictors)
  outcome <- processed$outcome[[1]]

  if (!survival::is.Surv(outcome)) {
    rlang::abort("Response must be a survival object (created with survival::Surv)")
  }
  if (attr(outcome, "type") != "right") rlang::abort("Only right-censored data allowed")

  outcome_times <- outcome[, 1]
  outcome_status <- outcome[, 2]

  fit <- survival_ln_mixture_impl(predictors, outcome_times, outcome_status, ...)

  new_survival_ln_mixture(
    posterior = fit$posterior,
    nobs = fit$nobs,
    predictors_name = fit$predictors_name,
    mixture_groups = fit$mixture_groups,
    blueprint = processed$blueprint
  )
}


# ------------------------------------------------------------------------------
# Implementation
survival_ln_mixture_impl <- function(predictors, outcome_times,
                                     outcome_status, iter = 1000,
                                     warmup = floor(iter / 10),
                                     thin = 1,
                                     chains = 1, cores = 1,
                                     mixture_components = 2,
                                     show_progress = FALSE,
                                     em_iter = 0,
                                     starting_seed = sample(1:2^28, 1),
                                     use_W = FALSE,
                                     number_em_search = 200,
                                     iteration_em_search = 1,
                                     fast_groups = TRUE) {
  number_of_predictors <- ncol(predictors)

  if (any(is.na(predictors))) {
    "There is one or more NA values in the predictors variable."
  }

  if (number_of_predictors < 1) {
    rlang::abort(
      c(
        "The model must contain at least one predictor.",
        i = "When using outcome ~ NULL, intercept must be explicitly set to TRUE."
      )
    )
  }

  if (any(outcome_times == 0)) {
    rlang::abort("One or more events happened at time zero.")
  }

  if (any(is.na(outcome_times))) {
    rlang::abort("There is one or more NA values at event times.")
  }

  if (any(is.na(outcome_status))) {
    rlang::abort("There is one or more NA values at the status")
  }

  if (!is.logical(show_progress)) {
    rlang::abort("The parameter show_progress must be TRUE or FALSE.")
  }

  if (!is.logical(use_W)) {
    rlang::abort("The parameter use_W must be TRUE or FALSE.")
  }

  if (!is.logical(fast_groups)) {
    rlang::abort("The parameter fast_groups must be TRUE or FALSE.")
  }

  if (number_em_search < 0 | (number_em_search %% 1) != 0) {
    rlang::abort("The parameter number_em_search should be a non-negative integer.")
  }

  if (iteration_em_search <= 0 | (iteration_em_search %% 1) != 0) {
    rlang::abort("The parameter iteration_em_search should be a positive integer.")
  }

  if (use_W & (em_iter <= 0)) {
    rlang::abort("In order to set the parameter use_W to true, em_iter must be greater than 0.")
  }

  if (thin <= 0 | (thin %% 1) != 0) {
    rlang::abort("The parameter thin should be a positive integer.")
  }

  if (warmup < 0 | (warmup %% 1) != 0) {
    rlang::abort("The parameter warmup should be a positive integer.")
  }

  if (em_iter < 0 | (em_iter %% 1) != 0) {
    rlang::abort("The parameter em_iter should be a non-negative integer.")
  }

  if (iter <= 0 | (iter %% 1) != 0) {
    rlang::abort("The parameter iter should be a positive integer.")
  }

  if (mixture_components <= 0 | (mixture_components %% 1) != 0) {
    rlang::abort("The parameter mixture_components should be a positive integer.")
  }

  if (starting_seed < 1 | starting_seed > 2^28 |
    (starting_seed %% 1) != 0) {
    rlang::abort("The starting seed should be a natural number between 1 and 2^28")
  }

  if (warmup >= iter) {
    rlang::abort("The warm-up iterations should be lower than the number of iterations.")
  }

  if (cores < 1 | (cores %% 1) != 0) {
    rlang::abort("The number of cores should be a natural number, at least 1.")
  }

  better_initial_values <- as.logical((em_iter > 0) & (number_em_search > 0))

  posterior_dist <- run_posterior_samples(iter, em_iter, chains, cores, mixture_components, outcome_times, outcome_status, predictors, starting_seed, show_progress, warmup, thin, use_W, better_initial_values, number_em_search, iteration_em_search, fast_groups)

  # returning the function output
  list(
    posterior = posterior_dist,
    nobs = length(outcome_times),
    predictors_name = colnames(predictors),
    mixture_groups = seq_len(mixture_components)
  )
}

#' corrige o problema do label switch para uma cadeia da posteriori, ordenando grupos por proporções de mistura
#'
#' @param posterior_dist uma matriz de dimensao numero_iteracoes x mixture_components de amostras a posteriori

#' @return matriz de dimensão numero_iteracoes x numero_componetes mas com os labels
#' reorganizados de forma que os etas das componetes são ordenados de forma decrescente.
#'
#' @noRd
label_switch_one_chain <- function(posterior_dist) {
  obj <- dplyr::as_tibble(posterior_dist)

  obj_etas <- obj |>
    dplyr::select(dplyr::starts_with("eta_"))

  etas_median <- as.numeric(apply(obj_etas, 2, stats::median))

  etas_order <- order(etas_median, decreasing = T)

  new_obj <- NULL

  for (j in 1:length(etas_order)) {
    char <- as.character(etas_order[j])

    sub_obj <- obj |>
      dplyr::select(dplyr::ends_with(as.character(char)))

    cols_remove <- NULL

    colnames_obj <- colnames(sub_obj)

    for (c in 1:length(colnames_obj)) {
      char_colname <- strsplit(colnames_obj[c], "_")[[1]][
        length(strsplit(colnames_obj[c], "_")[[1]])
      ]

      if (char_colname != char) {
        cols_remove <- c(cols_remove, c)
      }
    }

    if (length(cols_remove) > 0) {
      sub_obj <- sub_obj[, -cols_remove]
    }

    for (c in 1:ncol(sub_obj)) {
      names(sub_obj)[c] <- paste0(
        substr(
          names(sub_obj)[c],
          1, nchar(names(sub_obj)[c]) - nchar(char)
        ), j
      )
    }

    new_obj <- dplyr::bind_cols(new_obj, sub_obj)
  }

  return(new_obj)
}

#' Nomeia as colunas da amostra a posteriori
#'
#' @param posterior distribuição a posteriori amostrada
#'
#' @param predictors_names nome das variáveis preditoras. Deve ser um vetor com tamanho numero_covariaveis.
#' @param mixture_components número de componentes envolvidos no ajuste
#'
#' @return matriz
#'
#' @noRd
give_colnames <- function(posterior, predictors_names, mixture_components) {
  number_params <- length(predictors_names)
  new_names <- NULL

  for (i in 1:mixture_components) {
    for (j in 1:3) { # de 1 a 3 porque 3 grupos de parâmetros são ajustados no modelo: uma proporção de mistura, uma precisão e um grupo de efeitos das covariáveis
      if (j == 1) { # primeiro grupo, eta: efeitos das covariáveis
        for (c in 1:number_params) {
          new_names <- c(
            new_names,
            paste0(predictors_names[c], "_", i)
          )
        }
      } else if (j == 2) { # segundo grupo, phi: precisão
        new_names <- c(
          new_names,
          paste0("phi_", i)
        )
      } else { # terceiro grupo, j = 3, proporções de mistura
        new_names <- c(
          new_names,
          paste0("eta_", i)
        )
      }
    }
  }

  posterior_dist <- posterior
  colnames(posterior_dist) <- new_names

  return(posterior_dist)
}

#' Permuta as colunas para ficar de acordo: primeiro efeitos das covariáveis dos grupos, precisões e, por fim, proporções de misturas
#'
#' @param posterior distribuição a posteriori amostrada
#'
#' @return matriz
#'
#' @noRd
permute_columns <- function(posterior) {
  posterior_dist <- dplyr::as_tibble(posterior)
  posterior_dist <- dplyr::bind_cols(
    posterior |>
      dplyr::select(
        -tidyselect::starts_with("eta"),
        -tidyselect::starts_with("phi")
      ),
    posterior |>
      dplyr::select(
        tidyselect::starts_with("phi"),
        tidyselect::starts_with("eta")
      )
  )

  return(posterior_dist)
}

#' Roda as cadeias especificadas pelo usuário de forma sequencial, em apenas um core
#'
#' @param iter número de iterações do amostrador de Gibbs
#'
#' @param em_iter número de iterações do algoritmo EM
#'
#' @param chains número de cadeias a serem utilizadas
#'
#' @param cores número de cores utilizados para amostrar as cadeias
#'
#' @param mixture_components número de componentes envolvidos na análise
#'
#' @param outcome_times tempos observados
#'
#' @param outcome_status indicador de censura, 1 se foi observado evento e 0 para censura
#'
#' @param predictors matriz de preditores
#'
#' @param starting_seed semente inicial do algoritmo
#'
#' @param show_progress indica se o algoritmo deve mostrar iterações realizadas
#'
#' @param warmup aquecimento das cadeias
#'
#' @param thin thinning das cadeias
#'
#' @param use_W indica se deve utilizar Empirical Bayes, mantendo a matriz W do EM constante
#'
#' @return matriz
#'
#' @noRd


run_posterior_samples <- function(iter, em_iter, chains, cores,
                                  mixture_components, outcome_times,
                                  outcome_status, predictors, starting_seed,
                                  show_progress, warmup, thin, use_W,
                                  better_initial_values, number_em_search,
                                  iterations_em_search, fast_groups) {
  set.seed(starting_seed)
  seeds <- sample(1:2^28, chains)

  list_posteriors <- NULL

  RcppParallel::setThreadOptions(cores)

  posterior <- lognormal_mixture_gibbs(
    iter, em_iter, mixture_components,
    outcome_times, outcome_status,
    predictors,
    seeds, show_progress,
    chains, use_W,
    better_initial_values, number_em_search, iterations_em_search,
    fast_groups
  )

  for (i in 1:chains) {
    posterior_chain_i <- as.data.frame(posterior[, , i])

    posterior_chain_i <- give_colnames(
      posterior_chain_i,
      colnames(predictors),
      mixture_components
    )

    posterior_chain_i <- label_switch_one_chain(posterior_chain_i)

    posterior_chain_i <- permute_columns(posterior_chain_i)

    remover_menor_theta <- -which(
      colnames(posterior_chain_i) == colnames(
        posterior_chain_i |>
          dplyr::select(dplyr::starts_with("eta_"))
      )[mixture_components]
    )

    posterior_chain_i <- posterior_chain_i[, remover_menor_theta]

    list_posteriors[[i]] <- posterior_chain_i |>
      posterior::as_draws_matrix()
  }

  draws_return <- list_posteriors[[1]]

  if (length(list_posteriors) >= 2) {
    for (i in 2:length(list_posteriors)) {
      draws_return <- posterior::bind_draws(draws_return,
        list_posteriors[[i]],
        along = "chain"
      )
    }
  }

  # warming up
  draws_return <- posterior::subset_draws(
    draws_return,
    iteration = (warmup + 1):(posterior::niterations(draws_return))
  )

  # thinning draws
  draws_return <- posterior::thin_draws(draws_return, thin)

  return(draws_return)
}
