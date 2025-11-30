#' Get Parameter Information for a GKw Family Distribution
#'
#' @param family The GKw family distribution name.
#' @return A list with parameter information.
#' @keywords internal
.get_family_param_info <- function(family) {
  # Define parameter information and TMB parameter positions for each family
  family_params <- list(
    gkw = list(
      names = c("alpha", "beta", "gamma", "delta", "lambda"),
      n = 5,
      fixed = list(),
      positions = list(alpha = 1, beta = 2, gamma = 3, delta = 4, lambda = 5)
    ),
    bkw = list(
      names = c("alpha", "beta", "gamma", "delta"),
      n = 4,
      fixed = list(lambda = 1),
      positions = list(alpha = 1, beta = 2, gamma = 3, delta = 4)
    ),
    kkw = list(
      names = c("alpha", "beta", "delta", "lambda"),
      n = 4,
      fixed = list(gamma = 1),
      positions = list(alpha = 1, beta = 2, delta = 3, lambda = 4)
    ),
    ekw = list(
      names = c("alpha", "beta", "lambda"),
      n = 3,
      fixed = list(gamma = 1, delta = 0),
      positions = list(alpha = 1, beta = 2, lambda = 3)
    ),
    mc = list(
      names = c("gamma", "delta", "lambda"),
      n = 3,
      fixed = list(alpha = 1, beta = 1),
      positions = list(gamma = 1, delta = 2, lambda = 3)
    ),
    kw = list(
      names = c("alpha", "beta"),
      n = 2,
      fixed = list(gamma = 1, delta = 0, lambda = 1),
      positions = list(alpha = 1, beta = 2)
    ),
    beta = list(
      names = c("gamma", "delta"),
      n = 2,
      fixed = list(alpha = 1, beta = 1, lambda = 1),
      positions = list(gamma = 1, delta = 2)
    )
  )

  # Return parameter information for the specified family
  return(family_params[[family]])
}



#' Process Formula Parts from a Formula Object
#'
#' @param formula_obj Formula object created with the Formula package.
#' @param param_names Names of the parameters for the specified family.
#' @param fixed_params List of fixed parameters.
#' @param data Data frame containing the variables.
#' @return A list of formula objects for each parameter.
#' @keywords internal
.process_formula_parts <- function(formula_obj, param_names, fixed_params, data) {
  # Get non-fixed parameters
  non_fixed_params <- setdiff(param_names, names(fixed_params))

  # Extract the response variable from the formula
  resp_var <- as.character(formula_obj[[2]])

  # Create list to store formulas for each parameter
  formula_list <- list()

  # Get the max number of RHS parts in the formula
  n_parts <- length(attr(Formula::Formula(formula_obj), "rhs"))

  # Process each non-fixed parameter
  for (i in seq_along(non_fixed_params)) {
    param <- non_fixed_params[i]

    if (i <= n_parts) {
      # Extract the ith part of the formula
      rhs_part <- stats::formula(formula_obj, rhs = i, lhs = 1)[[3]]

      # Check if this part is just a dot
      if (identical(as.character(rhs_part), ".") || identical(as.character(rhs_part), "1")) {
        # Use intercept-only model
        formula_list[[param]] <- Formula::as.Formula(paste(resp_var, "~", "1"))
      } else {
        # Use the specified formula part
        formula_list[[param]] <- Formula::as.Formula(paste(resp_var, "~", deparse(rhs_part)))
      }
    } else {
      # For parameters beyond the number of specified parts, use intercept-only model
      formula_list[[param]] <- Formula::as.Formula(paste(resp_var, "~", "1"))
    }
  }

  return(formula_list)
}




#' Process Link Functions for GKw Regression
#'
#' @param link A character string or list of character strings specifying link functions.
#' @param param_names Names of the parameters for the specified family.
#' @param fixed_params List of fixed parameters.
#' @return A list of link functions.
#' @keywords internal
.process_link <- function(link, param_names, fixed_params) {
  # Default link functions for each parameter
  default_links <- list(
    alpha = "log",
    beta = "log",
    gamma = "log",
    delta = "logit",
    lambda = "log"
  )

  # Supported link functions
  supported_links <- c(
    "log", "logit", "identity", "inverse", "sqrt", "probit", "cloglog",
    "cauchy", "inverse-square"
  )

  # If link is NULL, use default links
  if (is.null(link)) {
    # Get default links for non-fixed parameters
    non_fixed_params <- setdiff(param_names, names(fixed_params))
    link_list <- default_links[non_fixed_params]
    return(link_list)
  }

  # If link is a single character string, apply to all parameters
  if (is.character(link) && length(link) == 1) {
    if (!link %in% supported_links) {
      stop(paste("Unsupported link function:", link))
    }

    # Apply the same link function to all non-fixed parameters
    non_fixed_params <- setdiff(param_names, names(fixed_params))
    link_list <- replicate(length(non_fixed_params), link, simplify = FALSE)
    names(link_list) <- non_fixed_params
    return(link_list)
  }

  # If link is a list, validate and return
  if (is.list(link) || is.character(link) && length(link) > 1) {
    if (is.character(link)) {
      link <- as.list(link)
      names(link) <- setdiff(param_names, names(fixed_params))
    }

    # Check if names of list match parameter names
    link_names <- names(link)
    if (is.null(link_names) || !all(link_names %in% param_names)) {
      stop("Names of link list must match parameter names for the chosen family")
    }

    # Check if all links are supported
    unsupported <- !unlist(link) %in% supported_links
    if (any(unsupported)) {
      stop(paste(
        "Unsupported link function(s):",
        paste(unlist(link)[unsupported], collapse = ", ")
      ))
    }

    # Remove links for fixed parameters
    fixed_param_names <- names(fixed_params)
    link <- link[setdiff(link_names, fixed_param_names)]

    return(link)
  }

  stop("link must be either a character string or a list of character strings")
}


#' Process Link Scales for GKw Regression
#'
#' @param link_scale A numeric value or list specifying scales for link functions.
#' @param link_list List of link functions for each parameter.
#' @param param_names Names of the parameters for the specified family.
#' @param fixed_params List of fixed parameters.
#' @return A list of link scales.
#' @keywords internal
.process_link_scale <- function(link_scale, link_list, param_names, fixed_params) {
  # Default scale values based on link type
  probability_link_types <- c("logit", "probit", "cloglog", "cauchy")

  # Initialize default scales list
  default_scales <- list()

  # Set default scales based on parameter and link type
  for (param in names(link_list)) {
    if (link_list[[param]] %in% probability_link_types) {
      default_scales[[param]] <- 10.0 # Default for probability-type links
    } else {
      default_scales[[param]] <- 1.0 # Default for other links
    }
  }

  # If link_scale is NULL, use default scales
  if (is.null(link_scale)) {
    # Get default scales for non-fixed parameters with links
    non_fixed_params <- intersect(names(link_list), setdiff(param_names, names(fixed_params)))
    return(default_scales[non_fixed_params])
  }

  # If link_scale is a single numeric value, apply to all parameters
  if (is.numeric(link_scale) && length(link_scale) == 1) {
    # Validate scale value
    if (link_scale <= 0) {
      stop("link_scale must be positive")
    }

    # Apply the same scale to all non-fixed parameters with links
    non_fixed_params <- intersect(names(link_list), setdiff(param_names, names(fixed_params)))
    scale_list <- replicate(length(non_fixed_params), link_scale, simplify = FALSE)
    names(scale_list) <- non_fixed_params
    return(scale_list)
  }

  # If link_scale is a list, validate and return
  if (is.list(link_scale) || is.numeric(link_scale) && length(link_scale) > 1) {
    if (is.numeric(link_scale)) {
      link_scale <- as.list(link_scale)
      names(link_scale) <- intersect(names(link_list), setdiff(param_names, names(fixed_params)))
    }

    # Check if names of list match parameter names
    scale_names <- names(link_scale)
    if (is.null(scale_names) || !all(scale_names %in% param_names)) {
      stop("Names of link_scale list must match parameter names for the chosen family")
    }

    # Check if all scales are positive
    non_positive <- !unlist(lapply(link_scale, function(x) is.numeric(x) && x > 0))
    if (any(non_positive)) {
      stop("All link_scale values must be positive numbers")
    }

    # Remove scales for fixed parameters
    fixed_param_names <- names(fixed_params)
    link_scale <- link_scale[setdiff(scale_names, fixed_param_names)]

    # For parameters that have a link but no specified scale, use defaults
    missing_scales <- setdiff(names(link_list), names(link_scale))
    if (length(missing_scales) > 0) {
      link_scale <- c(link_scale, default_scales[missing_scales])
    }

    return(link_scale)
  }

  stop("link_scale must be either a numeric value or a list of numeric values")
}



#' Convert Link Function Names to TMB Integers
#'
#' @param link_list List of link function names
#' @return List of link function integers for TMB
#' @keywords internal
.convert_links_to_int <- function(link_list) {
  link_map <- c(
    "log" = 1,
    "logit" = 2,
    "probit" = 3,
    "cauchy" = 4,
    "cloglog" = 5,
    "identity" = 6,
    "sqrt" = 7,
    "inverse" = 8,
    "inverse-square" = 9
  )

  result <- lapply(link_list, function(link) {
    if (link %in% names(link_map)) {
      return(link_map[link])
    } else {
      warning("Unsupported link function: ", link, ". Using log link instead.")
      return(1) # Default to log
    }
  })

  return(result)
}



#' Format Coefficient Names Based on Family and Model Matrices
#'
#' @param param_names Names of parameters for the family
#' @param model_data Model data list including matrices
#' @param param_positions Parameter position mapping for the family
#' @return Vector of formatted coefficient names
#' @keywords internal
.format_coefficient_names <- function(param_names, model_data, param_positions) {
  # Initialize vector to accumulate all coefficient names
  all_coef_names <- c()
  # For each parameter that has a model matrix
  for (param in param_names) {
    if (param %in% names(model_data$matrices)) {
      # Get parameter position in TMB
      tmb_pos <- param_positions[[param]]
      # Get the model matrix
      X <- model_data$matrices[[param]]
      mat_coef_names <- colnames(X)
      # Create names for coefficients
      if (is.null(mat_coef_names)) {
        # If there are no column names, use generic names
        mat_coef_names <- paste0(param, "_", 1:ncol(X))
      } else {
        # Add parameter name prefix
        mat_coef_names <- paste0(param, ":", mat_coef_names)
      }
      # Add to accumulator vector
      all_coef_names <- c(all_coef_names, mat_coef_names)
    }
  }
  # Return coefficient names
  return(all_coef_names)
}



#' Extract Model Data for GKw Regression
#'
#' @param formula_list List of formulas for each parameter.
#' @param data Data frame containing the variables.
#' @param subset Optional subset specification.
#' @param weights Optional weights.
#' @param na.action Function to handle missing values.
#' @param offset Optional offset.
#' @param contrasts List of contrasts for factors.
#' @param original_call The original function call.
#' @return A list of model data including frames, matrices, etc.
#' @keywords internal
.extract_model_data <- function(formula_list, data, subset, weights, na.action,
                                offset, contrasts, original_call) {
  # Initialize result list
  model_data <- list()

  # Get unique response variable name (should be the same for all formulas)
  resp_names <- unique(vapply(formula_list, function(f) as.character(f[[2]]), character(1)))
  if (length(resp_names) > 1) {
    stop("All formulas must have the same response variable")
  }

  # Extract model frames, responses, and model matrices for each parameter
  model_data$frames <- list()
  model_data$responses <- list()
  model_data$matrices <- list()
  model_data$terms <- list()

  for (param in names(formula_list)) {
    # Construct a list with arguments for model.frame
    mf_args <- list(
      formula = formula_list[[param]],
      data = data,
      subset = subset,
      na.action = na.action,
      drop.unused.levels = TRUE
    )

    # Evaluate and fix weights if provided
    if (!is.null(weights)) {
      weight_val <- tryCatch(weights, error = function(e) NULL)
      if (!is.null(weight_val) && !is.function(weight_val)) {
        mf_args$weights <- weight_val
      }
    }

    # Force the evaluation of the model frame in the proper environment
    model_data$frames[[param]] <- do.call(stats::model.frame, mf_args)

    # Extract response and model matrix
    model_data$responses[[param]] <- stats::model.response(model_data$frames[[param]])
    model_data$terms[[param]] <- attr(model_data$frames[[param]], "terms")
    X <- stats::model.matrix(model_data$terms[[param]], model_data$frames[[param]], contrasts)
    model_data$matrices[[param]] <- X
  }

  # Extract common response variable
  model_data$y <- model_data$responses[[1]]

  # Store model frame for the first parameter (all should have the same response)
  model_data$model <- model_data$frames[[1]]

  return(model_data)
}



#' Validate Data for GKw Regression
#'
#' @param data Numeric vector to validate.
#' @param n_params Number of parameters in the selected model.
#' @return The validated data.
#' @keywords internal
.validate_data <- function(data, n_params) {
  # Check if data is numeric
  if (!is.numeric(data)) {
    stop("Response variable must be numeric")
  }

  # Check for missing values
  if (any(is.na(data))) {
    stop("Response variable contains missing values")
  }

  # Check for values in the (0, 1) interval
  if (any(data <= 0 | data >= 1)) {
    stop("Response variable must be in the open interval (0, 1)")
  }

  # Return the validated data
  return(data)
}


#' Process Fixed Parameters for GKw Regression
#'
#' @param fixed List of fixed parameters or coefficients.
#' @param param_names Names of the parameters for the specified family.
#' @param fixed_params List of fixed parameters from the family definition.
#' @return A list of processed fixed parameters and coefficients.
#' @keywords internal
.process_fixed <- function(fixed, param_names, fixed_params) {
  # If no additional fixed parameters, return the family's fixed parameters
  if (is.null(fixed)) {
    return(fixed_params)
  }

  # Check if fixed is a valid list
  if (!is.list(fixed)) {
    stop("fixed must be a list")
  }

  # Combine user-defined fixed parameters with family's fixed parameters
  fixed_combined <- c(fixed, fixed_params)

  # Check for duplicates
  if (length(unique(names(fixed_combined))) < length(names(fixed_combined))) {
    stop("Duplicate entries in fixed parameters")
  }

  return(fixed_combined)
}



#' #' Map gkwreg parameter index to TMB parameter index
#' #'
#' #' @param object A fitted model object of class "gkwreg"
#' #' @param param_idx Index of the parameter in the gkwreg coefficients vector
#' #' @return The corresponding index in the TMB parameter vector, or NA if mapping fails
#' #'
#' #' @keywords internal
#' .map_gkwreg_to_tmb_param <- function(object, param_idx) {
#'   # Extract necessary information from the model object
#'   cf_names <- names(object$coefficients)
#'   param_name <- cf_names[param_idx]
#'
#'   # Try to extract the TMB parameter mapping
#'   if (!is.null(object$tmb_param_map)) {
#'     # If the model object already has a parameter map, use it
#'     if (param_name %in% names(object$tmb_param_map)) {
#'       return(object$tmb_param_map[param_name])
#'     }
#'   }
#'
#'   # Otherwise, we need to reconstruct the mapping
#'   # This is implementation-specific and depends on how parameters are organized in TMB
#'
#'   # Parse the parameter name to determine the parameter type and position
#'   param_parts <- strsplit(param_name, ":", fixed = TRUE)[[1]]
#'
#'   if (length(param_parts) < 2) {
#'     # Cannot parse parameter name
#'     return(NA)
#'   }
#'
#'   param_type <- param_parts[1] # alpha, beta, gamma, delta, or lambda
#'   covariate <- paste(param_parts[-1], collapse = ":") # The covariate name
#'
#'   # Get the model family
#'   family <- object$family
#'   if (is.null(family)) family <- "gkw" # Default to gkw
#'
#'   # Get parameter information for this family
#'   param_info <- .get_family_param_info(family)
#'
#'   # Check if the parameter type is valid for this family
#'   if (!param_type %in% param_info$names) {
#'     # Parameter not valid for this family
#'     return(NA)
#'   }
#'
#'   # Get the parameter position for this family
#'   param_pos <- param_info$positions[[param_type]]
#'
#'   # Get the model matrices to determine covariate position
#'   if (!is.null(object$x) && param_type %in% names(object$x)) {
#'     X_mat <- object$x[[param_type]]
#'     cov_idx <- which(colnames(X_mat) == covariate)
#'
#'     if (length(cov_idx) == 1) {
#'       # Calculate the TMB parameter index
#'       # This formula depends on how parameters are organized in TMB
#'       # The basic idea is to map (param_type, covariate) to a linear index
#'
#'       # Count parameters for previous types
#'       offset <- 0
#'       for (prev_type in param_info$names) {
#'         if (prev_type == param_type) break
#'
#'         if (prev_type %in% names(object$x)) {
#'           offset <- offset + ncol(object$x[[prev_type]])
#'         }
#'       }
#'
#'       return(offset + cov_idx)
#'     }
#'   }
#'
#'   # If we can't determine the exact mapping, try a simpler approach
#'   # This assumes parameters are ordered as they appear in the coefficients vector
#'   return(param_idx)
#' }
#'
#'
#' # Extract confidence intervals from TMB profile likelihood objects
#' #' @keywords internal
#' .extract_profile_ci <- function(profile_obj, level = 0.95) {
#'   # Check if the profile object has the expected format
#'   if (!is.data.frame(profile_obj) || !all(c("par", "value") %in% names(profile_obj))) {
#'     stop("Profile object does not have the expected format")
#'   }
#'
#'   # Negative log-likelihood profile (-logLik)
#'   prof_data <- profile_obj[, c("par", "value")]
#'
#'   # Find the minimum value of -logLik (maximum of logLik)
#'   min_value <- min(prof_data$value, na.rm = TRUE)
#'
#'   # Calculate threshold based on chi-square distribution
#'   # For CI of level (1-alpha), we use the (1-alpha) quantile of chi-square with 1 d.f.
#'   alpha <- 1 - level
#'   threshold <- min_value + qchisq(level, df = 1) / 2
#'
#'   # Filter points within the confidence interval
#'   ci_points <- prof_data[prof_data$value <= threshold, ]
#'
#'   # If there aren't enough points, return NA
#'   if (nrow(ci_points) < 2) {
#'     return(c(NA, NA))
#'   }
#'
#'   # Extract lower and upper CI limits
#'   ci_lower <- min(ci_points$par, na.rm = TRUE)
#'   ci_upper <- max(ci_points$par, na.rm = TRUE)
#'
#'   return(c(ci_lower, ci_upper))
#' }
