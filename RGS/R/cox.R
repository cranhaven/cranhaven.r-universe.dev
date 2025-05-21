#' Test U-shape relationship
#' This function checks for a U-shaped relationship between survival time and a set of variables.
#' Users are required to create and set the datadist option globally before using this function.
#'
#' @param my_data A dataframe containing the data with survival times and event indicators.
#' @param variables A character vector specifying the names of the variables to be tested.
#'
#' @importFrom stats termplot as.formula
#' @importFrom survival coxph Surv
#' @importFrom rms cph datadist rcs
#'
#' @return No return value, produces a plot as a side effect.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(rms)
#'
#' data_path <- system.file("extdata", "cox_data.csv", package = "RGS")
#' my_data <- read.csv(data_path)
#'
#' # create and set the datadist option globally
#' dd <- datadist(my_data)
#' options(datadist = "dd")
#'
#' test_ushape_cox(my_data, c("u1", "u2"))
#' }
#'
test_ushape_cox <- function(my_data, variables) {
  for (variable in variables) {
    # Create the formula for the Cox model using restricted cubic splines
    formula <- as.formula(paste0("Surv(t, d) ~ rcs(", variable, ", 3)"))

    # Fit the Cox proportional hazards model
    cox_model <- cph(formula, data = my_data, x = TRUE, y = TRUE)

    # Use do.call to construct the Predict call
    pred <- do.call(Predict, list(cox_model, variable))

    # Plot the relationship between the variable and log(Hazard Ratio)
    plt <- plot(pred, xlab = variable, ylab = "log(Hazard Ratio)", shade = FALSE)
    print(plt)
  }
}

#' Find symmetric cutoffs
#' This function finds symmetric cutoff points for a specified variable using Cox regression.
#'
#' @param my_data A dataframe containing the data.
#' @param variable_name A character string specifying the name of the variable for which to find cutoffs.
#'
#' @return A dataframe containing the symmetric cutoffs and their corresponding predicted log hazard ratios.
#'
#' @importFrom rms cph datadist rcs
#' @importFrom stats quantile as.formula
find_cutoffs_cox <- function(my_data, variable_name) {
  if (!(variable_name %in% names(my_data))) {
    stop(paste("Variable", variable_name, "not found in the dataset."))
  }

  # Initialize pairs to store results
  pairs <- data.frame(
      variable_left = numeric(0),
      pred_left = numeric(0),
      variable_right = numeric(0),
      pred_right = numeric(0)
    )

  # Fit the model
  formula <- as.formula(paste0("Surv(t, d) ~ rcs(", variable_name, ", 3)"))
  model <- cph(formula, data = my_data, x = TRUE, y = TRUE)

  # Function to calculate predicted lnhr
  cal_lnhr <- function(model, variable_name, my_data) {
    params <- list(object = model)
    params[[variable_name]] <- my_data[[variable_name]]
    return(do.call(Predict, params))
  }

  # Call the function to get predictions
  pred_lnhr <- cal_lnhr(model, variable_name, my_data)

  # Finding the minimum yhat and corresponding x
  min_index <- order(pred_lnhr$yhat)[1]
  min_x <- pred_lnhr[min_index, variable_name]

  # Calculate left and right x ranges
  left_xrange <- c(min(my_data[[variable_name]]), min_x)
  right_xrange <- c(min_x, max(my_data[[variable_name]]))

  # Function to find x for a given lnhr
  find_x_for_lnhr <- function(model, lnhr, xrange, variable_name, my_data) {
    ff <- function(x) {
      my_data[[variable_name]] <- x
      value <- cal_lnhr(model, variable_name, my_data)$yhat[1] - lnhr
      return(value)
    }

    # Check endpoint values
    endpoint_values <- sapply(xrange, ff)
    if (prod(sign(endpoint_values)) >= 0) {
      stop("Function does not change sign in the specified range.")
    }

    # Find the root
    root <- uniroot(ff, xrange, tol = 0.0001)$root
    return(root)
  }

  # Loop through quantiles to find symmetric cutoffs
  quantiles <- seq(0.025, 0.975, length.out = 100)
  for (q in quantiles) {
    lnhr_value <- quantile(pred_lnhr$yhat, q)

    x_left <- tryCatch(find_x_for_lnhr(model, lnhr_value, left_xrange, variable_name, my_data),
                       error = function(e) NA)
    x_right <- tryCatch(find_x_for_lnhr(model, lnhr_value, right_xrange, variable_name, my_data),
                        error = function(e) NA)
    if (!is.na(x_left) && !is.na(x_right)) {
      pairs <- rbind(pairs, data.frame(
        variable_left = x_left,
        pred_left = lnhr_value,
        variable_right = x_right,
        pred_right = lnhr_value,
        row.names = NULL
      ))
    }
  }

  if (nrow(pairs) > 0) {
    return(pairs)
  } else {
    warning("No symmetric cutoffs found.")
    return(data.frame())
  }
}

#' Evaluate model with cutoffs
#' Evaluates the model by creating exposure variables based on the cutoffs and fitting a Cox model.
#'
#' @param my_data A dataframe containing the data.
#' @param cutoffs A list of dataframes containing the cutoffs for each variable.
#' @param formula An object of class "formula" describing the model structure.
#'
#' @return A dataframe summarizing the model evaluation metrics.
evaluate_model_cox <- function(my_data, cutoffs, formula) {
  # Create exposure variables
  for (var in names(cutoffs)) {
    my_data[[paste0("exposure_", var)]] <- as.integer(my_data[[var]] < cutoffs[[var]]$variable_left | my_data[[var]] > cutoffs[[var]]$variable_right)
  }

  # Fit Cox regression model
  model <- coxph(formula, data = my_data, model = TRUE, x = TRUE, y = TRUE)

  # Compute model evaluation metric (AIC)
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


#' Find best combination of cutoffs
#' Searches for the best combination of cutoffs that minimizes the Akaike Information Criterion (AIC).
#'
#' @param my_data A dataframe containing the data.
#' @param cutoffs_list A list of dataframes containing the cutoffs for each variable.
#' @param formula An object of class "formula" describing the model structure.
#'
#' @return A dataframe containing the optimal cutoff values and the AIC value.
find_best_combination_cox <- function(my_data, cutoffs_list, formula) {
  # Initialize results storage
  results <- data.frame()

  # Recursive function to handle nested loops
  evaluate_combinations <- function(i, current_cutoffs) {
    if (i == length(cutoffs_list) + 1) {
      result <- evaluate_model_cox(my_data, current_cutoffs, formula)
      return(result)
    } else {
      current_results <- NULL  # Initialize results variable
      for (j in 1:nrow(cutoffs_list[[i]])) {
        updated_current_cutoffs <- current_cutoffs
        name <- names(cutoffs_list)[[i]]  # Get the name of the current dataframe
        updated_current_cutoffs[[name]] <- cutoffs_list[[i]][j, ]  # Update the current cutoffs
        # Recursively call function to handle the next dataframe
        next_results <- evaluate_combinations(i + 1, updated_current_cutoffs)
        # Combine results
        if (is.null(current_results)) {
          current_results <- next_results
        } else {
          current_results <- rbind(current_results, next_results)
        }
      }
      return(current_results)
    }
  }

  # Start evaluating combinations
  results <- evaluate_combinations(1, list())

  # Find the best combination
  best_combination_idx <- which.min(results$AIC)
  best_combination <- results[best_combination_idx, ]

  return(best_combination)
}


#' Main function for cox regression analysis
#' Performs regression analysis to identify optimal cutoffs and evaluate the model.
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
#' data_path <- system.file("extdata", "cox_data.csv", package = "RGS")
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
#' rgs_for_cox(my_data, u_variables, covariates)
#' }
#'
rgs_for_cox <- function(my_data, u_variables, covariates) {
  # Find symmetric points
  cutoffs_list <- lapply(u_variables, function(var) find_cutoffs_cox(my_data, var))

  # Assign names to cutoffs_list
  names(cutoffs_list) <- u_variables

  # Construct the full formula string
  formula <- as.formula(paste("Surv(t, d) ~ ",
                              paste0("exposure_", u_variables, collapse = " + "),
                              if(length(covariates) > 0) paste("+", covariates, collapse = " + ")))

  # Find the best combination
  best_combination <- find_best_combination_cox(my_data, cutoffs_list, formula)

  return(best_combination)
}
