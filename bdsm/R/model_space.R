#' Initialize model space matrix
#'
#' This function builds a representation of the model space, by creating a
#' dataframe where each column represents values of the parameters for a given
#' model. Real value means that the parameter is included in the model. A
#' parameter not present in the model is marked as \code{NA}.
#'
#' Currently the set of features is assumed to be all columns which remain after
#' excluding \code{timestamp_col}, \code{entity_col} and \code{dep_var_col}.
#'
#' A power set of all possible exclusions of linear dependence on the given
#' feature is created, i.e. if there are 4 features we end up with 2^4 possible
#' models (for each model we independently decide whether to include or not a
#' feature).
#'
#' @param df Data frame with data for the SEM analysis.
#' @param timestamp_col Column which determines time periods. For now only
#' natural numbers can be used as timestamps
#' @param entity_col Column which determines entities (e.g. countries, people)
#' @param dep_var_col Column with dependent variable
#' @param init_value Initial value for parameters present in the model. Default
#' is \code{1}.
#'
#' @return matrix of model parameters
#'
#' @examples
#' library(magrittr)
#'
#' data_prepared <- economic_growth[,1:7] %>%
#'   feature_standardization(timestamp_col = year, entity_col = country) %>%
#'   feature_standardization(timestamp_col = year, entity_col = country,
#'                         cross_sectional = TRUE, scale = FALSE)
#'
#' initialize_model_space(data_prepared, year, country, gdp)
#' @export
initialize_model_space <- function(df, timestamp_col, entity_col,
                                   dep_var_col, init_value = 1) {
  regressors <- df %>%
    regressor_names(timestamp_col = {{ timestamp_col }},
                    entity_col = {{ entity_col }},
                    dep_var_col = {{ dep_var_col }})
  regressors_n <- length(regressors)

  counts <- df %>% dplyr::select({{ timestamp_col }}, {{ entity_col }}) %>%
    sapply(function(x) dplyr::n_distinct(x))

  timestamps_n <- counts[[1]] - 1
  entities_n <- counts[[2]]

  regressors_subsets_matrix <-
    (rje::powerSetMat(regressors_n) * init_value)

  linear_params_matrix <-
    t(cbind(regressors_subsets_matrix, regressors_subsets_matrix))

  rownames(linear_params_matrix) <-
    c(paste('beta', regressors, sep="_"), paste('phi_1', regressors, sep="_"))

  dep_var_matrix <-
    t(matrix(init_value, nrow = 2^regressors_n, ncol = 3 + timestamps_n))
  rownames(dep_var_matrix) <- c(c('alpha', 'phi_0', 'err_var'),
                                paste('dep_var', 1:timestamps_n, sep = '_'))

  phis_n <- regressors_n * (timestamps_n - 1)
  psis_n <- regressors_n * (timestamps_n - 1) * timestamps_n / 2

  psis_phis_matrix <-
    matrix(init_value, nrow = phis_n + psis_n, ncol = 2^regressors_n)

  . <- NULL
  rbind(dep_var_matrix, linear_params_matrix, psis_phis_matrix) %>%
    replace(. == 0, NA)
}

#' Helper function to extract names from a vector defining a model
#'
#' For now it is assumed that we can only exclude linear relationships between
#' regressors and the dependent variable.
#'
#' The vector needs to have named rows, i.e. it is assumed it comes from a
#' model space (see \link[bdsm]{initialize_model_space} for details).
#'
#' @param params a vector with parameters describing the model
#'
#' @return
#' Names of regressors which are assumed to be linearly connected with dependent
#' variable within the model described by the \code{params} vector.
#'
#' @examples
#' params <- c(alpha = 1, beta_gdp = 1, beta_gdp_lagged = 1, phi_0 = 1, err_var = 1)
#' regressor_names_from_params_vector(params)
#'
#' @export
regressor_names_from_params_vector <- function(params) {
  regressors_subset <-
    t(params %>% stats::na.omit()) %>% as.data.frame() %>%
    dplyr::select(tidyselect::matches('beta'))

  names(regressors_subset) <- gsub("beta_", "", names(regressors_subset))
  names(regressors_subset)
}

#' Finds MLE parameters for each model in the given model space
#'
#' Given a dataset and an initial value for parameters, initializes a model
#' space with parameters equal to initial value for each model. Then for each
#' model performs a numerical optimization and finds parameters which maximize
#' the likelihood.
#'
#' @param df Data frame with data for the SEM analysis.
#' @param timestamp_col The name of the column with time stamps
#' @param entity_col Column with entities (e.g. countries)
#' @param dep_var_col Column with the dependent variable
#' @param init_value The value with which the model space will be initialized.
#' This will be the starting point for the numerical optimization.
#' @param exact_value Whether the exact value of the likelihood should be
#' computed (\code{TRUE}) or just the proportional part (\code{FALSE}). Check
#' \link[bdsm]{SEM_likelihood} for details.
#' @param run_parallel If \code{TRUE} the optimization is run in parallel using
#' the \link[parallel]{parApply} function. If \code{FALSE} (default value) the
#' base apply function is used. Note that using the parallel computing requires
#' setting the default cluster. See README.
#' @param control a list of control parameters for the optimization which are
#' passed to \link[stats]{optim}. Default is
#' \code{list(trace = 2, maxit = 10000, fnscale = -1, REPORT = 100, scale = 0.05)}, but note
#' that \code{scale} is used only for adjusting the \code{parscale} element added later in the function code.
#'
#' @importFrom parallel parApply
#'
#' @return
#' List of parameters describing analyzed models
#'
#' @examples
#' \donttest{
#' library(magrittr)
#'
#' data_prepared <- economic_growth[,1:7] %>%
#'    feature_standardization(timestamp_col = year, entity_col = country) %>%
#'    feature_standardization(timestamp_col = year, entity_col = country,
#'                            cross_sectional = TRUE, scale = FALSE)
#'
#' model_space <- optimal_model_space(df = data_prepared, dep_var_col = gdp,
#'                                    timestamp_col = year, entity_col = country,
#'                                    init_value = 0.5)
#' }
#'
#' @export
optimal_model_space <-
  function(df, timestamp_col, entity_col, dep_var_col, init_value,
           exact_value = TRUE, run_parallel = FALSE,
           control = list(trace = 2, maxit = 10000, fnscale = -1,
                          REPORT = 100, scale = 0.05)) {
    matrices_shared_across_models <- df %>%
      matrices_from_df(timestamp_col = {{ timestamp_col }},
                       entity_col = {{ entity_col }},
                       dep_var_col = {{ dep_var_col }},
                       which_matrices = c("Y1", "Y2", "Z", "res_maker_matrix"))

    model_space <- df %>%
      initialize_model_space(timestamp_col = {{ timestamp_col }},
                             entity_col = {{ entity_col}},
                             dep_var_col = {{ dep_var_col }},
                             init_value = init_value)

    optimization_wrapper <- function(params, data) {
      regressors_subset <-
        regressor_names_from_params_vector(params)

      model_specific_matrices <- df %>%
        matrices_from_df(timestamp_col = {{ timestamp_col }},
                         entity_col = {{ entity_col }},
                         dep_var_col = {{ dep_var_col }},
                         lin_related_regressors = regressors_subset,
                         which_matrices = c("cur_Y2", "cur_Z"))

      data$cur_Z <- model_specific_matrices$cur_Z
      data$cur_Y2 <- model_specific_matrices$cur_Y2

      params_no_na <- params %>% stats::na.omit()

      # parscale argument somehow (don't know yet how) changes step size during
      # optimization. Most likely optimization methods used in Gauss are
      # scale-free and these used in R are not
      # TODO: search for methods (or implement methods) in R which are scale-free
      control$parscale = control$scale * params_no_na
      control$scale = NULL

      optimized <- stats::optim(params_no_na, SEM_likelihood, data = data,
                                exact_value = exact_value,
                                method = "BFGS",
                                control = control)

      params[!is.na(params)] <- optimized[[1]]
      params
    }

    model_space <- do.call(
      ifelse(run_parallel, "parApply", "apply"),
      list(
        X = model_space, MARGIN = 2,
        FUN = optimization_wrapper,
        data = matrices_shared_across_models
      )
    )

    model_space
}
