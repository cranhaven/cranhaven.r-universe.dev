#' @import RCurl
#' @importFrom stats runif setNames na.omit rgamma
#' @importFrom methods slot new slotNames slot<-
#' @importFrom utils read.csv

# MARK: setClass Model
setClass(
  "Model",
  slots = list(
    parallel = "logical",
    num_fits = "numeric",
    seed = "numeric",
    defaults = "ANY",
    model_type = "logical",
    keep = "list",
    fit_model = "list",
    manual_param_init = "logical",
    skills = "character",
    folds = "numeric",
    forgets = "logical",
    fixed = "ANY",
    MODELS_BKT = "character",
    MODEL_ARGS = "character",
    FIT_ARGS = "character",
    CV_ARGS = "character",
    DEFAULTS = "list",
    DEFAULTS_BKT = "list",
    INITIALIZABLE_PARAMS = "character"
  )
)

# MARK: Init Functions
setMethod("initialize", "Model", function(.Object, parallel = TRUE, num_fits = 5, folds = 5, seed = sample(1:1e8, 1), defaults = NULL, model_type = rep(FALSE, 4), forgets = FALSE, ...) {
  object <- .Object
  object@parallel <- parallel
  object@num_fits <- num_fits
  object@seed <- seed
  object@defaults <- defaults
  object@model_type <- model_type
  object@fit_model <- list()
  object@manual_param_init <- FALSE
  object@skills <- ".*"
  object@folds <- folds
  object@forgets <- forgets

  set.seed(object@seed)

  # MARK: static parameter handle
  object@MODELS_BKT <- c("multilearn", "multiprior", "multipair", "multigs")
  object@MODEL_ARGS <- c("parallel", "num_fits", "seed", "defaults", object@MODELS_BKT)
  object@FIT_ARGS <- c("skills", "num_fits", "defaults", "fixed", "parallel", "forgets", "preload", object@MODELS_BKT)
  object@CV_ARGS <- c(object@FIT_ARGS, "folds", "seed")

  object@DEFAULTS <- list(
    num_fits = 5,
    defaults = NULL,
    parallel = TRUE,
    skills = ".*",
    seed = sample(0:1e8, 1),
    folds = 5,
    forgets = FALSE,
    fixed = NULL,
    model_type = rep(FALSE, length(object@MODELS_BKT))
  )

  object@DEFAULTS_BKT <- list(
    order_id = "order_id",
    skill_name = "skill_name",
    correct = "correct",
    user_id = "user_id",
    multilearn = "template_id",
    multiprior = "Anon Student Id",
    multipair = "problem_id",
    multigs = "template_id",
    folds = "template_id"
  )

  object@INITIALIZABLE_PARAMS <- c("prior", "learns", "guesses", "slips", "forgets")

  return(object)
})

# MARK: fit
#' fit bkt model
#'
#' Fit a BKT (Bayesian Knowledge Tracing) model.
#' This function fits the BKT model using the provided data and various options, such as
#' skill filtering, forget model, and parallelization. The function uses the model object
#' created by `bkt()` and fits the data according to the specified parameters.
#' @param object A BKT model object. The model to be cross-validated.
#' @param data Data frame. The dataset to be used for cross-validation. If `data` is not provided,
#'   `data_path` should be used to load the dataset from a file.
#' @param data_path Character. The file path to the dataset. This will be used if `data` is not provided.
#' @param parallel Logical. Indicates whether to use parallel computation.
#'   If set to `TRUE`, multithreading will be used to speed up model training.
#' @param seed Numeric. Seed for the random number generator, which ensures reproducibility
#'   of results.
#' @param num_fits Integer. Number of fit iterations. The best model is selected from
#'   the total iterations.
#' @param forgets Logical. Whether to include a forgetting factor in the model.
#'   If set to `TRUE`, the model will account for the possibility that learners may forget knowledge.
#' @param fixed List. A nested list specifying which parameters to fix for specific skills during
#'   model fitting. Each skill can have certain parameters, such as "guesses" and "slips", set to
#'   `TRUE` (to fix) or `FALSE` (to let them vary). For example:
#'   \code{list("skill_name" = list("guesses" = TRUE, "slips" = TRUE))}.
#' @param model_type Logical vector. Specifies model variants to use. There are four possible
#'   variants: 'multilearn', 'multiprior', 'multipair', and 'multigs'. Each corresponds to
#'   a different modeling strategy.
#' @param ... Other parameters.
#' @return A fitted BKT model object, which can be used for predictions, cross-validation,
#'   or parameter analysis.
#' @examples
#' \donttest{
#' model <- bkt(seed = 42, parallel = FALSE, num_fits = 1)
#' result <- fit(
#'   model,
#'   data_path = "data.csv"
#' )
#' }
#' @export
fit <- function(object, data_path = NULL, data = NULL, parallel = FALSE, seed = NULL, num_fits = 1, forgets = FALSE, fixed = NULL, model_type = NULL, ...) {
  if (!object@manual_param_init) {
    object@fit_model <- list()
  }

  object <- partial_fit(object, data_path = data_path, data = data, forgets = forgets, ...)

  return(object)
}

# MARK: partial_fit
partial_fit <- function(object, data_path = NULL, data = NULL, ...) {
  object <- ._check_data(object, data_path, data)

  args <- list(...)

  object <- ._check_args(object, object@FIT_ARGS, args)
  object <- ._update_param(object, c("skills", "num_fits", "defaults", "fixed", "parallel", "forgets"), args)

  if (is.null(object@fit_model) || length(object@fit_model) == 0) {
    object@fit_model <- list()
  }

  if (length(object@fit_model) == 0 || (object@manual_param_init && length(object@fit_model) > 0)) {
    object <- ._update_param(object, "model_type", ._update_defaults(object, args))
  }

  object@manual_param_init <- TRUE
  all_data <- ._data_helper(object, data_path, data, object@defaults, object@skills, object@model_type)

  object <- ._update_param(object, "skills", list(skills = names(all_data)))
  for (skill in names(all_data)) {
    object@fit_model[[skill]] <- ._fit(object, all_data[[skill]], skill, object@forgets, preload = ifelse("preload" %in% names(args), args$preload, FALSE))
  }

  object@manual_param_init <- FALSE

  return(object)
}

# MARK: ._check_data
._check_data <- function(object, data_path, data) {
  if (is.null(data_path) && is.null(data)) {
    stop("No data specified")
  } else if (!is.null(data_path) && !is.null(data)) {
    stop("Cannot specify both data location and data")
  } else if (!is.null(data_path) && !file.exists(data_path)) {
    stop("Data path is invalid or file not found")
  }
  return(object)
}

# MARK: ._check_args
._check_args <- function(object, expected_args, args) {
  for (arg in names(args)) {
    if (!(arg %in% expected_args)) {
      stop("Provided arguments are not recognized. They must be one or more of: ", paste(expected_args, collapse = ", "))
    }
  }
  return(object)
}

# MARK: ._update_param
._update_param <- function(object, params, args, keep = FALSE) {
  if (is.list(args)) {
    for (param in params) {
      if (!param %in% names(args) && (!param %in% names(object@keep) || !object@keep[[param]])) {
        arg <- object@DEFAULTS[[param]]
        default_arg <- if (is.function(arg)) arg() else arg
        if (is.null(slot(object, param))) {
          slot(object, param) <- default_arg
        }
      } else if (param %in% names(args) && !is.null(args[[param]])) {
        slot(object, param) <- args[[param]]
      }
      object@keep[[param]] <- keep
    }
  } else {
    slot(object, params) <- args
    object@keep[[params]] <- keep
  }

  if ("seed" %in% params) {
    set.seed(object@seed)
  }

  return(object)
}

# MARK:._update_defaults
._update_defaults <- function(object, args) {
  # Update the default column names
  model_types <- rep(FALSE, 4)

  for (d in names(args)) {
    if (d %in% object@MODELS_BKT) {
      if (is.logical(args[[d]])) {
        model_types[which(object@MODELS_BKT == d)] <- args[[d]]
      } else if (is.character(args[[d]])) {
        if (is.null(object@defaults)) {
          object@defaults <- list()
        }
        object@defaults[[d]] <- args[[d]]
        model_types[which(object@MODELS_BKT == d)] <- TRUE
      } else {
        stop("model type must either be boolean for automatic column inference or string specifying column")
      }
    } else if (d %in% object@DEFAULTS_BKT) {
      if (is.null(object@defaults)) {
        object@defaults <- list()
      }
      object@defaults[[d]] <- args[[d]]
    }
  }

  return(model_types)
}

# MARK: fetch_dataset
#' Fetch a dataset
#'
#' Fetch a dataset from an online source.
#' This function downloads a dataset from a provided URL and saves it to a specified location
#' on the local system. The dataset must be publicly accessible, without requiring any
#' password or authentication. It can then be used for further analysis or modeling.
#'
#' @param object A BKT model object. The model can use the fetched dataset for fitting or other tasks.
#' @param link Character. The URL where the dataset is located. This must be a publicly accessible URL.
#' @param loc Character. The local file path where the dataset will be saved. The dataset will
#'   be stored at this location after download.
#' @return None. The function downloads the data file to the specified location.
#' @examples
#' \donttest{
#' model <- bkt()
#' fetch_dataset(model, "http://example.com/dataset.csv", "data.csv")
#' }
#' @export
fetch_dataset <- function(object, link, loc) {
  name <- basename(link)
  file_path <- file.path(loc, name)
  if (file.exists(file_path)) {
    message("File already exists: ", file_path)
  } else {
    file_data <- RCurl::getBinaryURL(link)
    writeBin(file_data, file_path)
    message("File downloaded to: ", file_path)
  }
}

# MARK: ._data_helper
._data_helper <- function(object, data_path, data, defaults, skills, model_type, gs_ref = NULL, resource_ref = NULL, return_df = FALSE, folds = FALSE) {
  data_p <- NULL

  if (!is.null(data)) {
    data_p <- convert_data(data, skills,
      defaults = defaults, model_type = model_type,
      gs_refs = gs_ref, resource_refs = resource_ref, return_df = return_df, folds = folds
    )
  } else if (!is.null(data_path) && is.character(data_path)) {
    data_p <- convert_data_path(data_path, skills,
      defaults = defaults, model_type = model_type,
      gs_refs = gs_ref, resource_refs = resource_ref, return_df = return_df, folds = folds
    )
  }

  if (!return_df) {
    lapply(data_p, function(d) check_data(d))
  } else {
    lapply(data_p[[1]], function(d) check_data(d))
  }

  return(data_p)
}

# MARK: ._fit
._fit <- function(object, data, skill, forgets, preload = FALSE) {
  num_learns <- length(data$resource_names)
  num_gs <- length(data$gs_names)
  check_manual_param_init(object, num_learns, num_gs, skill)
  if (!is.null(object@fixed)) {
    object <- ._check_fixed(object)
  }

  num_fit_initializations <- object@num_fits
  best_likelihood <- -Inf
  best_model <- NULL
  for (i in seq_len(num_fit_initializations)) {
    fitmodel <- random_model_uni(num_learns, num_gs)
    optional_args <- list(fixed = list())
    # fitmodel$prior <- 0.3
    # for (i in 1:13) {
    #   fitmodel$learns[i] <- i * 0.03
    #   fitmodel$forgets[i] <- i * 0.02
    #   fitmodel$As[i, 1, 2] <- 0.02 * i
    #   fitmodel$As[i, 2, 1] <- 0.03 * i
    #   fitmodel$As[i, 2, 2] <- (1 - fitmodel$As[i, 2, 1])
    # }
    # for (i in 1:1) {
    #   fitmodel$guesses[i] <- i * 0.05
    #   fitmodel$slips[i] <- i * 0.04
    #   fitmodel$emissions[i, 1, 1] <- i * 0.03
    #   fitmodel$emissions[i, 1, 2] <- 1 - fitmodel$emissions[i, 1, 1]
    #   fitmodel$emissions[i, 2, 1] <- i * 0.02
    #   fitmodel$emissions[i, 2, 2] <- 1 - fitmodel$emissions[i, 2, 1]
    # }
    # fitmodel$As[1, 2, 1] <- 0.28
    # fitmodel$As[1, 2, 2] <- 0.72
    # fitmodel$pi_0[1, 1] <- 0.95
    # fitmodel$pi_0[2, 1] <- 0.33
    # print(fitmodel)
    if (forgets) {
      fitmodel$forgets <- runif(length(fitmodel$forgets))
    }

    # if (object@model_type[which(Model$MODELS_BKT == 'multiprior')]) {
    #   fitmodel$prior <- 0
    # }

    if (object@manual_param_init && skill %in% names(object@fit_model)) {
      for (var in names(object@fit_model[[skill]])) {
        if (!is.null(object@fixed) && skill %in% names(object@fixed) &&
          var %in% names(object@fixed[[skill]]) &&
          is.logical(object@fixed[[skill]][[var]]) && object@fixed[[skill]][[var]]) {
          optional_args$fixed[[var]] <- object@fit_model[[skill]][[var]]
        } else if (var %in% names(fitmodel)) {
          fitmodel[[var]] <- object@fit_model[[skill]][[var]]
        }
      }
    }

    if (!is.null(object@fixed) && skill %in% names(object@fixed)) {
      for (var in names(object@fixed[[skill]])) {
        if (!is.logical(object@fixed[[skill]][[var]])) {
          optional_args$fixed[[var]] <- object@fixed[[skill]][[var]]
        }
      }
    }

    if (!preload) {
      em_fit_result <- EM_fit(fitmodel, data, parallel = object@parallel, fixed = optional_args$fixed)
      fitmodel <- em_fit_result$model
      log_likelihoods <- em_fit_result$log_likelihoods
      if (log_likelihoods[length(log_likelihoods)] > best_likelihood) {
        best_likelihood <- log_likelihoods[length(log_likelihoods)]
        best_model <- fitmodel
      }
    } else {
      best_model <- fitmodel
    }
  }
  fit_model <- best_model
  fit_model$learns <- fit_model$As[, 2, 1]
  fit_model$forgets <- fit_model$As[, 1, 2]
  fit_model$prior <- fit_model$pi_0[2, 1]
  fit_model$resource_names <- data$resource_names
  fit_model$gs_names <- data$gs_names
  fit_model$likelihood <- best_likelihood
  return(fit_model)
}

# MARK: ._check_fixed
._check_fixed <- function(object, fixed) {
  # Checks the fixed parameter
  if (is.null(object@fixed)) {
  } else if (is.logical(object@fixed) && object@fixed) {
    object@fixed <- object@fit_model
  } else if (is.list(object@fixed)) {
  } else {
    stop("fixed parameter incorrectly specified")
  }
  return(object)
}


# MARK: evaluate
rmse <- function(true_vals, pred_vals) {
  sqrt(mean((true_vals - pred_vals)^2))
}

#' Evaluate
#'
#' Evaluate a BKT (Bayesian Knowledge Tracing) model using a specified metric.
#' This function evaluates a fitted BKT model on a given dataset using a chosen performance metric.
#' It takes either a data frame or a file path to the data and returns the evaluation result
#' based on the specified metric (e.g., RMSE or accuracy).
#'
#' @param object A fitted BKT model object. This is the model to be evaluated.
#' @param data Data frame. The dataset on which the model will be evaluated. If `data` is not provided,
#'   the function will attempt to load the dataset from the file specified by `data_path`.
#' @param data_path Character. The file path to the dataset for evaluation. This will be used if `data` is not provided.
#' @param metric Function or Function List. The evaluation metric used to assess the model performance.
#'   (Root Mean Square Error), but other metrics can also be specified.
#' @return Numeric or List. The result of the evaluation based on the specified metric(s). For example, if `rmse` is used,
#'   the function will return the root mean square error for the model on the dataset.
#' @examples
#' \donttest{
#' model <- bkt(seed = 42, parallel = TRUE, num_fits = 5)
#' result <- fit(model, data_path = "ct.csv", skills = "Plot non-terminating improper fraction")
#' eval_result <- evaluate(result, data_path = "ct_test.csv", metric = rmse)
#' print(eval_result)
#' }
#' @export
evaluate <- function(object, data = NULL, data_path = NULL, metric = rmse) {
  ._check_data(object, data_path, data)

  if (!is.list(metric) && !is.vector(metric)) {
    metric <- list(metric)
  }
  if (is.null(object@fit_model)) {
    stop("model has not been fitted yet")
  } else {
    for (i in seq_along(metric)) {
      m <- metric[[i]]
      if (is.character(m)) {
        stop("not implemented")
        if (!(m %in% metric$SUPPORTED_METRICS)) {
          stop(paste("metric must be one of:", paste(metric$SUPPORTED_METRICS, collapse = ", ")))
        }
        metric[[i]] <- metric$SUPPORTED_METRICS[[m]]
      } else if (!is.function(m)) {
        stop("metric must either be a string, function, or list/vector of strings and functions")
      }
    }
  }
  all_data <- ._data_helper(object, data_path, data, object@defaults, object@skills, object@model_type,
    gs_ref = object@fit_model, resource_ref = object@fit_model
  )
  results <- ._evaluate(object, all_data, metric)
  return(if (length(results) == 1) results[[1]] else results)
}

# MARK: ._evaluate
._evaluate <- function(object, all_data, metric) {
  per_skill <- list()
  true <- c()
  pred <- c()

  for (skill in names(all_data)) {
    predictions <- ._predict(object@fit_model[[skill]], all_data[[skill]])
    correct_predictions <- predictions$correct_predictions
    state_predictions <- predictions$state_predictions
    real_data <- all_data[[skill]]$data
    true <- c(true, colSums(real_data))
    pred <- c(pred, correct_predictions)
  }

  true <- true - 1
  tryCatch(
    {
      res <- lapply(metric, function(m) m(true, pred))
    },
    error = function(e) {
      res <- lapply(metric, function(m) m(true, round(pred)))
    }
  )
  return(res)
}

# MARK: ._predict
._predict <- function(model, data) {
  return(predict_onestep_run(model, data))
}

# MARK: check_manual_param_init
check_manual_param_init <- function(object, num_learns, num_gs, skill) {
  if (!is.null(object@fit_model) && skill %in% names(object@fit_model)) {
    # Check for 'learns'
    if ("learns" %in% names(object@fit_model[[skill]]) &&
      length(object@fit_model[[skill]]$learns) != num_learns) {
      stop("invalid number of learns in initialization")
    }

    # Check for 'guesses'
    if ("guesses" %in% names(object@fit_model[[skill]]) &&
      length(object@fit_model[[skill]]$guesses) != num_gs) {
      stop("invalid number of guess classes in initialization")
    }

    # Check for 'slips'
    if ("slips" %in% names(object@fit_model[[skill]]) &&
      length(object@fit_model[[skill]]$slips) != num_gs) {
      stop("invalid number of slip classes in initialization")
    }
  }
}

# MARK: params
#' Extract Parameters from BKT model
#'
#' Extract fitted parameters from a BKT model.
#' This function retrieves the parameters from a fitted BKT model object. The parameters
#' include model-specific values such as "learns", "guesses", "slips", and "forgets".
#' These parameters are returned in a format that is easy to print or manipulate for further analysis.
#'
#' @param object A fitted BKT model object. The model should have been previously fitted using
#'   the `fit()` function, otherwise no parameters will be available.
#' @return A data frame containing the fitted model parameters. The data frame will typically include
#'   columns such as 'learns', 'guesses', 'slips', and other model-specific values.
#' @examples
#' \donttest{
#' model <- bkt(seed = 42, parallel = TRUE, num_fits = 5)
#' result <- fit(model, data_path = "data.csv", skills = "skill name")
#' params_df <- params(result)
#' print(params_df)
#' }
#' @export
params <- function(object) {
  coefs <- coef_(object)
  formatted_coefs <- list()

  for (skill in names(coefs)) {
    for (param in names(coefs[[skill]])) {
      classes <- format_param(object, skill, param, coefs[[skill]][[param]])

      for (class_ in names(classes)) {
        formatted_coefs <- append(formatted_coefs, list(c(skill, param, class_, classes[[class_]])))
      }
    }
  }

  df <- as.data.frame(do.call(rbind, formatted_coefs), stringsAsFactors = FALSE)
  colnames(df) <- c("skill", "param", "class", "value")
  df$value <- as.numeric(df$value)
  df$value <- sprintf("%.6f", df$value)

  return(df)
}

# MARK: coef_
coef_ <- function(object) {
  if (length(object@fit_model) == 0) {
    stop("model has not been trained or initialized")
  }

  initializable_params <- c("learns", "forgets", "guesses", "slips", "prior") # Equivalent to Model.INITIALIZABLE_PARAMS

  if (object@forgets == FALSE) {
    initializable_params <- initializable_params[initializable_params != "forgets"]
  }

  coefs <- list()

  for (skill in names(object@fit_model)) {
    params <- list()
    for (param in initializable_params) {
      if (!is.null(object@fit_model[[skill]][[param]])) {
        params[[param]] <- object@fit_model[[skill]][[param]]
      }
    }
    if (object@model_type[2] == TRUE) { # handle multiprior params order
      params[["prior"]] <- params[["learns"]][2:length(params[["learns"]])]
      params[["learns"]] <- params[["learns"]][1]
    }
    coefs[[skill]] <- params
  }

  return(coefs)
}

# MARK: format_param
format_param <- function(object, skill, param, value) {
  if (is.numeric(value) && length(value) > 1) {
    ptype <- if (param %in% c("learns", "forgets", "prior")) "resource_names" else "gs_names"
    if (!is.null(object@fit_model[[skill]][[ptype]])) {
      names <- names(object@fit_model[[skill]][[ptype]])
      if (param == "prior" && object@model_type[2] == TRUE) { # handle multiprior
        names <- names[2:length(names)]
      }
      return(setNames(as.list(value), names))
    } else {
      stop(paste("Parameter type", ptype, "not found for skill", skill))
    }
  } else {
    return(list("default" = value))
  }
}

# MARK: crossvalidate
crossvalidate_single_skill <- function(data, skill, metrics) {
  lapply(metrics, function(metric) metric(rnorm(nrow(data)), data$truth))
}
#' Cross Validation
#'
#' Perform cross-validation on a BKT (Bayesian Knowledge Tracing) model.
#' This function trains and evaluates the BKT model using cross-validation. It splits
#' the dataset into training and validation sets, trains the model on the training data,
#' and evaluates it on the validation data according to a specified metric.
#'
#' @param metric Function. The metric function used to evaluate model performance.
#' @param data Data frame. The dataset to be used for cross-validation. If `data` is not provided,
#'   `data_path` should be used to load the dataset from a file.
#' @param data_path Character. The file path to the dataset. This will be used if `data` is not provided.
#' @param object A BKT model object. The model to be cross-validated.
#' @param parallel Logical. Indicates whether to use parallel computation.
#'   If set to `TRUE`, multithreading will be used to speed up model training.
#' @param seed Numeric. Seed for the random number generator, which ensures reproducibility
#'   of results.
#' @param num_fits Integer. Number of fit iterations. The best model is selected from
#'   the total iterations.
#' @param folds Integer. Number of folds used for cross-validation.
#'   This parameter is used during cross-validation to divide the data into parts.
#' @param forgets Logical. Whether to include a forgetting factor in the model.
#'   If set to `TRUE`, the model will account for the possibility that learners may forget knowledge.
#' @param fixed List. A nested list specifying which parameters to fix for specific skills during
#'   model fitting. Each skill can have certain parameters, such as "guesses" and "slips", set to
#'   `TRUE` (to fix) or `FALSE` (to let them vary). For example:
#'   \code{list("skill_name" = list("guesses" = TRUE, "slips" = TRUE))}.
#' @param model_type Logical vector. Specifies model variants to use. There are four possible
#'   variants: 'multilearn', 'multiprior', 'multipair', and 'multigs'. Each corresponds to
#'   a different modeling strategy.
#' @param ... Other parameters.
#' @return A list containing the cross-validation results, including the average performance metric
#'   and any other relevant details from the validation process.
#' @examples
#' \donttest{
#' model <- bkt(seed = 42, parallel = TRUE, num_fits = 5)
#' cv_results <- crossvalidate(model, data_path = "ct.csv", folds = 5)
#' print(cv_results)
#' }
#' @export
crossvalidate <- function(object, data = NULL, data_path = NULL, metric = rmse, parallel = FALSE, seed = NULL, num_fits = 1, folds = 5, forgets = FALSE, fixed = NULL, model_type = NULL, ...) {
  metric_names <- c()

  if (!is.list(metric)) {
    metric <- list(metric)
  }

  if (is.null(data) && is.null(data_path)) {
    stop("no data specified")
  } else {
    for (i in seq_along(metric)) {
      m <- metric[[i]]
      if (is.character(m)) {
        if (!(m %in% metric$SUPPORTED_METRICS)) {
          stop(paste("metric must be one of:", paste(metric$SUPPORTED_METRICS, collapse = ", ")))
        }
        metric[[i]] <- metric$SUPPORTED_METRICS[[m]]
        metric_names <- c(metric_names, m)
      } else if (is.function(m)) {
        metric_names <- c(metric_names, deparse(substitute(m)))
      } else {
        stop("metric must either be a string, function, or list/tuple of strings and functions")
      }
    }
  }

  ._check_args(object, object@CV_ARGS, list(...))
  object <- ._update_param(object, c("skills", "num_fits", "defaults", "parallel", "forgets", "seed", "folds"), list(...))
  object <- ._update_param(object, "model_type", ._update_defaults(object, list(...)))

  metric_vals <- list()

  if (!object@manual_param_init) {
    object@fit_model <- list()
  }

  if (is.character(object@folds)) {
    ._update_defaults(object, list(folds = object@folds))
  }

  all_data <- ._data_helper(object, data_path, data, object@defaults, object@skills, object@model_type, folds = is.character(object@folds))
  object <- ._update_param(object, "skills", list(skills = names(all_data)))

  for (skill in names(all_data)) {
    metric_vals[[skill]] <- ._crossvalidate(object, all_data[[skill]], skill, metric)
  }
  object@manual_param_init <- FALSE
  object@fit_model <- list()
  df <- data.frame(skill = names(metric_vals), dummy = I(unname(metric_vals)))
  # df[metric_names] <- do.call(rbind, df$dummy)
  # df <- df[, !(names(df) %in% "dummy")]
  return(df)
}


# MARK: ._crossvalidate
._crossvalidate <- function(model, data, skill, metric) {
  if (is.character(model@folds)) {
    return(crossvalidate_single_skill(model, data, skill, model@folds, metric, model@seed, use_folds = TRUE))
  } else {
    return(crossvalidate_single_skill(model, data, skill, model@folds, metric, model@seed))
  }
}

# MARK: predict
#' Predict
#'
#' Predict outcomes using a fitted BKT model.
#' This function uses a trained Bayesian Knowledge Tracing (BKT) model to make predictions
#' on new data. The predictions include both the likelihood of a correct response (`correct_predictions`)
#' and the estimated hidden state of the learner's knowledge (`state_predictions`).
#'
#' @param model A trained BKT model object. The model must have been previously fitted using
#'   the `fit()` function. If the model is not fitted, an error will be raised.
#' @param data_path Character. The file path to the dataset on which predictions will be made.
#'   If this is provided, the function will read data from the file.
#' @param data Data frame. A pre-loaded dataset to be used for predictions. This can be used
#'   instead of specifying `data_path`.
#' @return A data frame containing the original data with two additional columns:
#'   `correct_predictions` and `state_predictions`.
#' @examples
#' \donttest{
#' model <- bkt(seed = 42)
#' fit_model <- fit(model, data_path = "ct.csv")
#' predictions <- predict_bkt(fit_model, data_path = "ct_test.csv")
#' head(predictions)
#' }
#' @export
predict_bkt <- function(model, data_path = NULL, data = NULL) {
  ._check_data(model, data_path, data)

  if (is.null(model@fit_model)) {
    stop("Model has not been fitted yet")
  }

  data_helper_result <- ._data_helper(
    model,
    data_path = data_path, data = data,
    defaults = model@defaults, skills = model@skills,
    model_type = model@model_type, gs_ref = model@fit_model,
    resource_ref = model@fit_model, return_df = TRUE
  )
  all_data <- data_helper_result[[1]]
  df <- data_helper_result[[2]]

  df$correct_predictions <- 0.5
  df$state_predictions <- 0.5

  for (skill in names(all_data)) {
    pred_result <- ._predict(model@fit_model[[skill]], all_data[[skill]])
    correct_predictions <- pred_result[[1]]
    state_predictions <- pred_result[[2]][1, ]

    if (!is.null(all_data[[skill]]$multiprior_index)) {
      correct_predictions <- correct_predictions[-all_data[[skill]]$multiprior_index]
      state_predictions <- state_predictions[-all_data[[skill]]$multiprior_index]
    }

    df[df$skill_name == skill, "correct_predictions"] <- correct_predictions
    df[df$skill_name == skill, "state_predictions"] <- state_predictions
  }

  return(df)
}

# MARK: set_coef (coef_)
#' Set Coefficients for BKT Model
#'
#' This function sets or initializes the parameters of a Bayesian Knowledge Tracing (BKT) model.
#' The user can manually specify the values for different parameters associated with specific skills.
#'
#' @param object An object of the BKT model. This is the model for which the parameters will be set or initialized.
#' @param values A list containing the skill names and their corresponding BKT parameters.
#'   Each skill should have its own list of parameters.
#'   The parameters can include 'prior', 'learns', 'forgets', 'guesses', and 'slips'.
#'   Example structure: \code{list("skill_name" = list("learns" = ..., "guesses" = ...))}.
#'
#' @return The updated BKT model object with the newly set coefficients.
#'
#' @details
#' This function allows users to manually specify or update the parameters of a BKT model for different skills.
#' The values should be provided as a named list, with each skill having its own sublist of BKT parameters.
#' The function performs checks to ensure that the provided parameters are valid in terms of type, length, and existence.
#'
#' @examples
#' \donttest{
#' # Initialize a BKT model
#' model <- bkt(seed = 42)
#'
#' # Set custom parameters for a specific skill
#' model <- set_coef(model, list(
#'   "Plot non-terminating improper fraction" = list("prior" = 0.5, "learns" = 0.2)
#' ))
#'
#' # Fit the model with fixed parameters
#' result <- fit(model,
#'   forgets = TRUE,
#'   data_path = "ct.csv",
#'   skills = "Plot non-terminating improper fraction",
#'   fixed = list("Plot non-terminating improper fraction" = list("prior" = TRUE))
#' )
#' }
#' @export
set_coef <- function(object, values) {
  # Sets or initializes parameters in the BKT model
  # Values must be organized by skill and BKT parameters

  object@fit_model <- list()

  for (skill in names(values)) {
    if (!skill %in% names(object@fit_model)) {
      object@fit_model[[skill]] <- list()
    }
    if (!._check_params(object, values[[skill]])) {
      stop("error in length, type or non-existent parameter")
    }
    for (param in names(values[[skill]])) {
      object@fit_model[[skill]][[param]] <- values[[skill]][[param]]
    }
  }

  object@manual_param_init <- TRUE
  return(object)
}

# ._check_params
._check_params <- function(object, params) {
  # Checks if BKT parameters are valid
  valid <- TRUE

  for (param in names(params)) {
    if (param == "prior") {
      valid <- valid && is.numeric(params[[param]])
    } else {
      valid <- valid && is.numeric(params[[param]]) && param %in% object@INITIALIZABLE_PARAMS
    }
  }

  if ("learns" %in% names(params) && "forgets" %in% names(params)) {
    valid <- valid && (length(params[["learns"]]) == length(params[["forgets"]]))
  }

  if ("guesses" %in% names(params) && "slips" %in% names(params)) {
    valid <- valid && (length(params[["slips"]]) == length(params[["guesses"]]))
  }

  return(valid)
}
