#' Model-Assisted Projection Estimator
#'
#' @description The function addresses the problem of combining information from two or more independent surveys, a common challenge in survey sampling. It focuses on cases where: \cr
#' \itemize{
#'    \item **Survey 1:** A large sample collects only auxiliary information.
#'    \item **Survey 2:** A much smaller sample collects both the variables of interest and the auxiliary variables.
#' }
#' The function implements a model-assisted projection estimation method based on a working model. The working models that can be used include several machine learning models that can be seen in the details section
#'
#' @references
#' \enumerate{
#'  \item Kim, J. K., & Rao, J. N. (2012). Combining data from two independent surveys: a model-assisted approach. Biometrika, 99(1), 85-100.
#' }
#'
#' @param formula A model formula. All variables used must exist in both \code{data_model} and \code{data_proj}.
#' @param cluster_ids Column name (character) or formula specifying cluster identifiers from highest to lowest level. Use \code{~0} or \code{~1} if there are no clusters.
#' @param weight Column name in \code{data_proj} representing the survey weights.
#' @param strata Column name for stratification; use \code{NULL} if no strata are used.
#' @param domain Character vector specifying domain variable names in both datasets.
#' @param summary_function A function to compute domain-level estimates (default: \code{"mean"}, \code{"total"}, \code{"variance"}).
#' @param working_model A parsnip model object specifying the working model (see `@details`).
#' @param data_model Data frame (small sample) containing both target and auxiliary variables.
#' @param data_proj Data frame (large sample) containing only auxiliary variables.
#' @param model_metric A \code{yardstick::metric_set()} function, or \code{NULL} to use default metrics.
#' @param cv_folds Number of folds for k-fold cross-validation.
#' @param tuning_grid Either a data frame with tuning parameters or a positive integer specifying the number of grid search candidates.
#' @param parallel_over Specifies parallelization mode: \code{"resamples"}, \code{"everything"}, or \code{NULL}.
#' If "resamples", then tuning will be performed in parallel over resamples alone. Within each resample, the preprocessor (i.e. recipe or formula) is processed once, and is then reused across all models that need to be fit.
#' If "everything", then tuning will be performed in parallel at two levels. An outer parallel loop will iterate over resamples. Additionally, an inner parallel loop will iterate over all unique combinations of preprocessor and model tuning parameters for that specific resample. This will result in the preprocessor being re-processed multiple times, but can be faster if that processing is extremely fast.
#' @param seed Integer seed for reproducibility.
#' @param return_yhat Logical; if \code{TRUE}, returns predicted \code{y} values for \code{data_model}.
#' @param ... Additional arguments passed to \code{\link[survey]{svydesign}}.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{model} – The fitted working model object.
#'   \item \code{prediction} – A vector of predictions from the working model.
#'   \item \code{df_result} – A data frame with:
#'     \itemize{
#'       \item \code{domain} – Domain identifier.
#'       \item \code{ypr} – Projection estimator results for each domain.
#'       \item \code{var_ypr} – Estimated variance of the projection estimator.
#'       \item \code{rse_ypr} – Relative standard error (in \%).
#'     }
#' }
#'
#' @export
#' @import bonsai
#' @import tidymodels
#' @import lightgbm
#' @import ranger
#'
#'
#' @details
#' The following working models are supported via the \pkg{parsnip} interface:
#' \itemize{
#'   \item \code{linear_reg()} – Linear regression
#'   \item \code{logistic_reg()} – Logistic regression
#'   \item \code{linear_reg(engine = "stan")} – Bayesian linear regression
#'   \item \code{logistic_reg(engine = "stan")} – Bayesian logistic regression
#'   \item \code{poisson_reg()} – Poisson regression
#'   \item \code{decision_tree()} – Decision tree
#'   \item \code{nearest_neighbor()} – k-Nearest Neighbors (k-NN)
#'   \item \code{naive_bayes()} – Naive Bayes classifier
#'   \item \code{mlp()} – Multi-layer perceptron (neural network)
#'   \item \code{svm_linear()} – Support vector machine with linear kernel
#'   \item \code{svm_poly()} – Support vector machine with polynomial kernel
#'   \item \code{svm_rbf()} – Support vector machine with radial basis function (RBF) kernel
#'   \item \code{bag_tree()} – Bagged decision tree
#'   \item \code{bart()} – Bayesian Additive Regression Trees (BART)
#'   \item \code{rand_forest(engine = "ranger")} – Random forest (via ranger)
#'   \item \code{rand_forest(engine = "aorsf")} – Accelerated oblique random forest (AORF; Jaeger et al. 2022, 2024)
#'   \item \code{boost_tree(engine = "lightgbm")} – Gradient boosting (LightGBM)
#'   \item \code{boost_tree(engine = "xgboost")} – Gradient boosting (XGBoost)
#' }
#' For a complete list of supported models and engines, see \href{https://www.tmwr.org/pre-proc-table}{Tidy Modeling With R}.
#'
#' @examples
#' \dontrun{
#' library(sae.projection)
#' library(dplyr)
#' library(bonsai)
#'
#' df_svy22_income <- df_svy22 %>% filter(!is.na(income))
#' df_svy23_income <- df_svy23 %>% filter(!is.na(income))
#'
#' # Linear regression
#' lm_proj <- ma_projection(
#'   income ~ age + sex + edu + disability,
#'   cluster_ids = "PSU", weight = "WEIGHT", strata = "STRATA",
#'   domain = c("PROV", "REGENCY"),
#'   working_model = linear_reg(),
#'   data_model = df_svy22_income,
#'   data_proj = df_svy23_income,
#'   nest = TRUE
#' )
#'
#' df_svy22_neet <- df_svy22 %>% filter(between(age, 15, 24))
#' df_svy23_neet <- df_svy23 %>% filter(between(age, 15, 24))
#'
#' # LightGBM regression with hyperparameter tunning
#' show_engines("boost_tree")
#' lgbm_model <- boost_tree(
#'   mtry = tune(), trees = tune(), min_n = tune(),
#'   tree_depth = tune(), learn_rate = tune(),
#'   engine = "lightgbm"
#' )
#'
#' lgbm_proj <- ma_projection(
#'   formula = neet ~ sex + edu + disability,
#'   cluster_ids = "PSU",
#'   weight = "WEIGHT",
#'   strata = "STRATA",
#'   domain = c("PROV", "REGENCY"),
#'   working_model = lgbm_model,
#'   data_model = df_svy22_neet,
#'   data_proj = df_svy23_neet,
#'   cv_folds = 3,
#'   tuning_grid = 3,
#'   nest = TRUE
#' )
#' }
#' @md
ma_projection <- function(
    formula, cluster_ids, weight, strata = NULL, domain,
    summary_function = "mean", working_model,
    data_model, data_proj, model_metric,
    cv_folds = 3, tuning_grid = 10, parallel_over = "resamples",
    seed = 1, return_yhat = FALSE, ...) {

  domain_chr <- domain
  cluster_ids <- .check_variable(cluster_ids, data_model, data_proj)
  weight <- .check_variable(weight, data_model, data_proj)
  strata <- .check_variable(strata, data_model, data_proj)
  domain <- .check_variable(domain, data_model, data_proj)

  if ((methods::is(domain_chr, 'formula'))) {
    domain_chr <- colnames(stats::model.frame(domain, data_model, na.action = NULL))
  }

  y_name <- as.character(rlang::f_lhs(formula))
  y <- data_model[[y_name]]

  summary_function <- match.arg(summary_function, c("mean", "total", "varians"))
  if (summary_function == "mean") {
    FUN <- survey::svymean
  } else if (summary_function == "total") {
    FUN <- survey::svytotal
  } else if (summary_function == "varians") {
    FUN <- survey::svyvar
  } else {
    cli::cli_abort('summary_function must be "mean", "total", or "varians" not {fun}')
  }

  # spesifikasi model -------------------------------------
  if (class(y) %in% c("integer", "numeric")) {
    type <- "regression"
    if (missing(model_metric)) model_metric <- yardstick::metric_set(yardstick::rmse)
  } else {
    type <- "classification"
    if (missing(model_metric)) model_metric <- yardstick::metric_set(yardstick::f_meas)
  }

  model_spec <- parsnip::set_mode(working_model, mode = type)
  model_rec <- recipes::recipe(formula, data = data_model) %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors())

  model_wf <- workflows::workflow() %>%
    workflows::add_model(model_spec) %>%
    workflows::add_recipe(model_rec)

  # tunning model -------------------------------------
  all_tune <- grepl("tune", as.character(sapply(working_model[["args"]], rlang::quo_get_expr)))
  no_tune <- all(all_tune == FALSE)

  if (no_tune) {
    final_fit <- parsnip::fit(model_wf, data_model)
  } else {
    # set k-fold cv
    set.seed(seed)
    if (type == "regression") {
      model_fold <- rsample::vfold_cv(data_model, v = cv_folds)
    } else if (type == "classification") {
      model_fold <- rsample::vfold_cv(data_model, v = cv_folds, strata = y_name)
    }

    all_cores <- parallel::detectCores(logical = FALSE)
    cl <- parallel::makePSOCKcluster(all_cores)
    doParallel::registerDoParallel(cl)

    set.seed(seed)
    tune_results <- model_wf %>%
      tune::tune_grid(
        resamples = model_fold,
        grid = tuning_grid,
        metrics = model_metric,
        control = tune::control_grid(parallel_over = parallel_over, verbose = TRUE, save_pred = TRUE, save_workflow = TRUE)
      )

    parallel::stopCluster(cl)

    best_param <- tune::select_best(tune_results)
    final_fit <- tune::finalize_workflow(model_wf, best_param)
    final_fit <- parsnip::fit(final_fit, data_model)
  }

  final_model <- tune::extract_fit_engine(final_fit)

  # prediksi y pada data survei 1 -------------------------------------
  data_proj$ypr <- stats::predict(final_fit, new_data = data_proj)[[1]]
  prediction <- stats::predict(final_fit, new_data = data_model)[[1]]


  # bias correction from survey 2 -------------------------------------
  if (type == "regression") {
    data_model$bias <- y - prediction
  } else if (type == "classification") {
    data_model$bias <- ifelse(y == prediction, 0, 1)
    data_proj$ypr <- as.numeric(levels(data_proj$ypr))[data_proj$ypr]

    data_model[[y_name]] <- as.numeric(levels(y))[y]
  }

  svy2_design <- survey::svydesign(ids = cluster_ids, weights = weight, strata = strata, data = data_model, ...)
  est_bias <- survey::svyby(
    formula = ~bias,
    by = domain,
    design = svy2_design,
    FUN = FUN,
    vartype = c("var")
  )
  colnames(est_bias)[colnames(est_bias) == "var"] <- "var_bias"

  # estimasi yhat pada data survei 1 ----------------------------------
  svy1_design <- survey::svydesign(ids = cluster_ids, weights = weight, strata = strata, data = data_proj, ...)
  est_ypr <- survey::svyby(
    formula = ~ypr,
    by = domain,
    design = svy1_design,
    FUN = FUN,
    vartype = c("var")
  )
  colnames(est_ypr)[colnames(est_ypr) == "var"] <- "var_ypr"

  # hitung yhat pr, var yhatpr dan rse yhatpr -------------------------
  df_result <- dplyr::left_join(est_ypr, est_bias, by = domain_chr)
  ypr <- df_result$ypr + df_result$bias
  var_ypr <- df_result$var_ypr + df_result$var_bias
  rse_ypr <- sqrt(var_ypr) * 100 / ypr

  df_result$ypr <- ypr
  df_result$var_ypr <- var_ypr
  df_result$rse_ypr <- rse_ypr
  df_result[, c("bias", "var_bias")] <- NULL

  all_result <- list(
    working_model = final_model,
    prediction = data_proj$ypr
  )

  if (return_yhat) {
    est_y <- survey::svyby(
      formula = stats::as.formula(paste0("~", y_name)),
      by = domain,
      design = svy2_design,
      FUN = FUN,
      vartype = c("var", "cvpct")
    )
    colnames(est_y)[colnames(est_y) == "cv%"] <- "rse"
    all_result$return_yhat <- est_y
  }

  all_result$projection <- df_result
  return(all_result)
}








.check_variable <- function(variable, data_model, data_proj) {
  if (is.null(variable)) {
    return(variable)
  }

  if (methods::is(variable, "formula")) {
    if (variable == ~1 | variable == ~0) return(variable)

    tryCatch({
      dat <- stats::model.frame(variable, data_model, na.action = NULL)
      return(variable)
    },
    error = function(x){
      cli::cli_abort('variable "{variable}" is not found in the data_model')
    })

    tryCatch({
      dat <- stats::model.frame(variable, data_proj, na.action = NULL)
      return(variable)
    },
    error = function(x){
      cli::cli_abort('variable "{variable}" is not found in the data_proj')
    })

  }else if (methods::is(variable, "character")) {
    if (length(variable) > 1) {
      if (mean(variable %in% colnames(data_model)) == 1) {
        if (mean(variable %in% colnames(data_proj)) == 1) {
          return(stats::as.formula(paste0("~", paste0(variable, collapse = " + "))))
        } else {
          cli::cli_abort('variable "{setdiff(variable, colnames(data_proj))}" is not found in the data_proj')
        }
      } else {
        cli::cli_abort('variable "{setdiff(variable, colnames(data_model))}" is not found in the data_model')
      }
    } else if (variable %in% colnames(data_model)) {
      if (variable %in% colnames(data_proj)) {
        return(stats::as.formula(paste0("~", variable)))
      } else {
        cli::cli_abort('variable "{variable}" is not found in the data_proj')
      }
    } else {
      cli::cli_abort('variable "{variable}" is not found in the data_model')
    }
  } else {
    cli::cli_abort('variable "{variable}" must be character or formula')
  }
}
