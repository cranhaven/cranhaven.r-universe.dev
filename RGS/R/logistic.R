#' Test U-shape relationship in logistic regression
#'
#' This function plots the estimated relationship between the response variable and a set of predictor variables to identify potential U-shaped patterns.
#' Users are required to create and set the datadist option globally before using this function.
#'
#' @param my_data A dataframe containing the data with response and predictor variables.
#' @param variables A character vector specifying the names of the predictor variables to be tested.
#'
#' @importFrom SemiPar spm
#' @importFrom graphics plot
#' @importFrom stats as.formula binomial
#'
#' @return No return value, produces a plot as a side effect.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(rms)
#'
#' data_path <- system.file("extdata", "logistic_data.csv", package = "RGS")
#' my_data <- read.csv(data_path)
#'
#' # create and set the datadist option globally
#' dd <- datadist(my_data)
#' options(datadist = "dd")
#'
#' test_ushape_logistic(my_data, c("u1", "u2"))
#' }
#'
test_ushape_logistic <- function(my_data, variables) {
  for (variable in variables) {
    # Create the formula string
    formula_str <- paste0("my_data$y ~ f(my_data$", variable, ")")
    formula <- as.formula(formula_str)

    # Run SemiPar model
    spm <- spm(formula, omit.missing = TRUE, family = "binomial")
    plot(spm,
         ylab = "logit pi",
         xlab = variable,
         shade = FALSE)
  }
}

#' Find symmetric cutoffs for logistic regression
#'
#' This function finds symmetric cutoff points for a specified predictor variable using logistic regression.
#'
#' @param my_data A dataframe containing the data.
#' @param variable A character string specifying the name of the predictor variable for which to find cutoffs.
#'
#' @return A dataframe containing the symmetric cutoffs and their corresponding predicted log odds ratios.
#'
#' @importFrom rms lrm Predict rcs
#' @importFrom stats quantile uniroot as.formula
find_cutoffs_logistic <- function(my_data, variable) {
  pairs <- data.frame(
    variable_Left = numeric(),
    pred_Left = numeric(),
    variable_Right = numeric(),
    pred_Right = numeric()
  )

  # Calculate log odds ratios
  formula_str <- paste0("y ~ rcs(", variable, ", 3)")
  formula <- as.formula(formula_str)
  model <- lrm(formula, data = my_data)

  cal_lnor <- function(model, x) {
    params <- list(model, x)
    names(params) <- c("object", variable)
    return(do.call(Predict, params))
  }

  pred_lnor <- cal_lnor(model, my_data[[variable]])

  # Find symmetric points
  min_index <- order(pred_lnor$yhat)[1]
  min_x <- pred_lnor[min_index, variable]

  left_xrange <- c(min(my_data[[variable]]), min_x)
  right_xrange <- c(min_x, max(my_data[[variable]]))

  find_x_for_lnor <- function(model, lnor, xrange) {
    ff <- function(x) {
      cal_lnor(model, x)$yhat[1] - lnor
    }

    endpoint_values <- sapply(xrange, ff)
    if (prod(sign(endpoint_values)) >= 0) {
      stop("Function does not change sign in the specified range.")
    }

    root <- uniroot(ff, xrange, tol = 0.0001)$root
    return(root)
  }

  quantiles <- seq(0.025, 0.975, length.out = 100)
  for (q in quantiles) {
    lnor_value <- quantile(pred_lnor$yhat, q)

    x_left <- tryCatch(find_x_for_lnor(model, lnor_value, left_xrange), error = function(e) NA)
    x_right <- tryCatch(find_x_for_lnor(model, lnor_value, right_xrange), error = function(e) NA)

    if (!is.na(x_left) && !is.na(x_right)) {
      pairs <- rbind(pairs, data.frame(
        variable_left = x_left,
        pred_left = lnor_value,
        variable_right = x_right,
        pred_right = lnor_value
      ))
    }
  }

  if (nrow(pairs) > 0) {
    return(pairs)
  } else {
    warning("No symmetric cutoffs found.")
    return(NULL)
  }
}


#' Evaluate logistic regression model with cutoffs
#'
#' This function evaluates the logistic regression model by creating exposure variables based on the cutoffs and fitting the model.
#'
#' @param my_data A dataframe containing the data.
#' @param cutoffs A list of dataframes containing the cutoffs for each variable.
#' @param formula An object of class "formula" describing the model structure.
#'
#' @return A dataframe summarizing the model evaluation metrics.
#'
#' @importFrom stats glm
#' @importFrom stats AIC
evaluate_model_logistic <- function(my_data, cutoffs, formula) {
  # Create exposure variables
  for (var in names(cutoffs)) {
    my_data[[paste0("exposure_", var)]] <- as.integer(my_data[[var]] < cutoffs[[var]]$variable_left | my_data[[var]] > cutoffs[[var]]$variable_right)
  }

  # Fit logistic regression model
  model <- glm(formula, data = my_data, family = binomial)

  # Compute AIC
  aic_value <- AIC(model)

  # Build the result dataframe
  result_df <- data.frame(matrix(nrow = 1, ncol = 0))

  # Dynamically add columns
  for (var in names(cutoffs)) {
    result_df[paste0("left_cutoff_", var)] <- cutoffs[[var]]$variable_left
    result_df[paste0("right_cutoff_", var)] <- cutoffs[[var]]$variable_right
  }

  return(cbind(result_df, AIC = aic_value))
}


#' Find best combination of cutoffs for logistic regression
#'
#' This function searches for the best combination of cutoffs that minimizes the Akaike Information Criterion (AIC).
#'
#' @param my_data A dataframe containing the data.
#' @param cutoffs_list A list of dataframes containing the cutoffs for each variable.
#' @param formula An object of class "formula" describing the model structure.
#'
#' @return A dataframe containing the optimal cutoff values and the AIC value.
find_best_combination_logistic <- function(my_data, cutoffs_list, formula) {
  # Initialize results storage
  results <- data.frame()

  # Recursive function
  evaluate_combinations <- function(i, current_cutoffs) {
    if (i == length(cutoffs_list) + 1) {
      result <- evaluate_model_logistic(my_data, current_cutoffs, formula)
      return(result)
    } else {
      current_results <- NULL
      for (j in 1:nrow(cutoffs_list[[i]])) {
        updated_current_cutoffs <- current_cutoffs
        name <- names(cutoffs_list)[[i]]
        updated_current_cutoffs[[name]] <- cutoffs_list[[i]][j, ]
        next_results <- evaluate_combinations(i + 1, updated_current_cutoffs)
        if (is.null(current_results)) {
          current_results <- next_results
        } else {
          current_results <- rbind(current_results, next_results)
        }
      }
      return(current_results)
    }
  }

  results <- evaluate_combinations(1, list())

  best_combination_idx <- which.min(results$AIC)
  best_combination <- results[best_combination_idx, ]

  return(best_combination)
}


#' Main function for logistic regression analysis
#'
#' This function performs logistic regression analysis to identify optimal cutoffs and evaluate the model.
#' Users are required to create and set the datadist option globally before using this function.
#'
#' @param my_data A dataframe containing the data.
#' @param u_variables A character vector specifying the names of the U-shaped variables.
#' @param covariates A character vector specifying the names of the covariates.
#'
#' @export
#'
#' @importFrom stats as.formula
#'
#' @return A dataframe containing the best combination of cutoffs and associated AIC value.
#'
#' @examples
#' \donttest{
#' library(rms)
#'
#' data_path <- system.file("extdata", "logistic_data.csv", package = "RGS")
#' my_data <- read.csv(data_path)
#'
#' # Define variables
#' u_variables <- c("u1", "u2")
#' covariates <- c("l1", "c1")
#'
#' # create and set the datadist option globally
#' dd <- datadist(my_data)
#' options(datadist = "dd")
#'
#' rgs_for_logistic(my_data, u_variables, covariates)
#' }
#'
rgs_for_logistic <- function(my_data, u_variables, covariates) {
  # Find symmetric points
  cutoffs_list <- lapply(u_variables, function(var) find_cutoffs_logistic(my_data, var))

  # Assign names to cutoffs_list
  names(cutoffs_list) <- u_variables

  # Construct the full formula string
  formula <- as.formula(paste("y ~ ",
                              paste0("exposure_", u_variables, collapse = " + "),
                              ifelse(length(covariates) > 0, paste("+", covariates, collapse = " + "), "")))

  # Find the best combination
  best_combination <- find_best_combination_logistic(my_data, cutoffs_list, formula)

  return(best_combination)
}
