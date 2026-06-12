## Private files that get called from several mlmodels top level functions to
## do a specific task.

## GLOBAL CONSTANTS ============================================================
# -- 1. Setting defaults -------------------------------------------------------
# General function where we set constants as defaults to use within other functions,
# kind of a global constant setting.
#' @keywords internal
.mlmodels_defaults <- function() {
  list(
    scipen         = 2L,      # for coefficient tables
    digits         = 3L,      # default digits in summary tables
    signif_digits  = 4L,      # for p-values
    repetitions    = 999L,    # default bootstrap repetitions
    seed           = NULL
  )
}
# -- 2. Getting defaults -------------------------------------------------------
# Function to pull the default constant for a certain name.
#' @keywords internal
.mlmodels_get_default <- function(name) {
  defaults <- .mlmodels_defaults()
  defaults[[name]]
}

## Format Coefficient Matrix ===================================================
# Internal helper: Smart formatting of coefficient matrices
# Decides number of decimal places based on the magnitude of the values
.format_coef_matrix <- function(coef_mat, 
                                digits = 3, 
                                coef_cols = 1:2) {
  
  if (!is.matrix(coef_mat) || ncol(coef_mat) < max(coef_cols)) {
    return(coef_mat)
  }
  
  # Extract values to analyze for decimal places (usually Estimate and Std. Error)
  checkvals <- abs(coef_mat[, coef_cols, drop = FALSE])
  checkvals <- checkvals[checkvals > 0 & !is.na(checkvals)]
  
  if (length(checkvals) > 0) {
    # Find the largest number of leading zeros + 2 extra decimals
    max_decimals <- max(floor(-log10(checkvals))) + 2
    num_digits   <- max(digits, max_decimals)
  } else {
    num_digits <- digits
  }
  
  # Round only the specified columns (Estimate and Std. Error)
  coef_mat[, coef_cols] <- round(coef_mat[, coef_cols], num_digits)
  
  return(coef_mat)
}

## Is Invertible ===============================================================
# Internal function to detect if a matrix is invertible.
# 
# Argument: matrix - The matrix you want to check
# Returns: Logical with TRUE if it's invertible, or FALSE if it's not.
#
# Used in prediction and other methods to avoid throwing an error.
#
#' @keywords internal
.is_invertible <- function(matrix) {
  tryCatch({
    chol2inv(chol(matrix))
    TRUE
  }, error = function(e) FALSE)
}

# POST-MOLD UTILITIES ==========================================================

# Factor mapping ---------------------------------------------------------------
# Build factor mapping for a single equation
#
# Internal function. Creates a mapping of factor variables to their
# main effect columns and interaction columns after `hardhat::mold()`.
#
# Arguments:
#   mold          A mold object returned by hardhat::mold().
#   equation_name Character string indicating the equation name
#                 (e.g. "value" or "scale").
#
# Returns: A named list containing the factor mapping for this equation.
#' @keywords internal
.build_factor_map <- function(mold, equation_name = "value")
{
  if (is.null(mold) || is.null(mold$predictors) || ncol(mold$predictors) == 0)
    return(list())

  actual_colnames <- colnames(mold$predictors)
  ptypes <- mold$blueprint$ptypes$predictors

  # Initialize as empty list - this is the key fix
  mapping <- list()

  # === Dangerous transformation check ===
  actual_colnames <- colnames(mold$predictors)
  if (length(actual_colnames) == 0) return(list())

  ptypes <- mold$blueprint$ptypes$predictors

  # Find columns with dangerous transformations
  dangerous_names <- actual_colnames[grepl("I\\(|poly\\(", actual_colnames)]
  if (length(dangerous_names) > 0) {

    # Check variables to identify factor variable.
    for (pt_name in names(ptypes)) {
      if (!is.factor(ptypes[[pt_name]])) next

      # Which dangerous columns mention this factor?
      involved <- grepl(pt_name, dangerous_names, fixed = TRUE)
      if (!any(involved)) next

      # Check if any other variable names also contain this factor name
      other_names <- names(ptypes)[names(ptypes) != pt_name]
      other_involved <- grepl(pt_name, other_names, fixed = TRUE)

      if (any(other_involved)) {
        names_to_check <- other_names[other_involved]

        count_factor <- sapply(dangerous_names, function(col) {
          length(gregexpr(pt_name, col, fixed = TRUE)[[1]])
        })

        # Get the count factor to be the same dimensions as the l_matrix
        count_factor <- as.matrix(count_factor)

        # Matrix: rows = dangerous_names, columns = other variables
        l_matrix <- sapply(names_to_check, function(on) {
          grepl(on, dangerous_names, fixed = TRUE)
        })

        f_matrix <- !l_matrix & count_factor < 2

        if(length(f_matrix) == 1)
          caused_by_factor <- f_matrix
        else
          caused_by_factor <- rowSums(f_matrix) > 0

        if (any(caused_by_factor))
        {
          bad_cols <- dangerous_names[caused_by_factor]

          cli::cli_abort(c("Complex transformation involving factor '{pt_name}' in the {.strong {equation_name}} equation is not supported.",
                           "i" = "Found: {.val {paste(bad_cols, collapse = ', ')}}",
                           "i" = "Please use only main effects or simple interactions (* or :) with factors.",
                           "x" = "Mixed interactions like I((factor * continuous)^2) aren't allowed.",
                           "i" = "The proper syntax would be factor * I(continuous)^2.")
          )
        }
      }
      else
      {
        # All dangerous columns are caused by this factor
        cli::cli_abort(c("Complex transformation involving factor '{pt_name}' in the {.strong {equation_name}} is not supported.",
                         "i" = "Found: {.val {paste(bad_cols, collapse = ', ')}}",
                         "i" = "Please use only main effects or simple interactions (* or :) with factors.")
        )
      }
    }
  }

  # === 2. Build mapping for main effects and interactions ===
  for (var_name in names(ptypes)) {
    if (!is.factor(ptypes[[var_name]]))
    {
      mapping[[var_name]] <- NULL
      next
    }

    levels <- levels(ptypes[[var_name]])

    main_effect_cols <- character(0)
    interaction_cols <- character(0)

    for (lvl in levels) {
      # Main effect: exact match "varnamelevel"
      main_pattern <- paste0("^", var_name, lvl, "$")
      main_matches <- actual_colnames[grepl(main_pattern, actual_colnames)]
      main_effect_cols <- c(main_effect_cols, main_matches)

      # Interaction: contains "varnamelevel:" or ":varnamelevel"
      inter_pattern <- paste0(var_name, lvl, ":|:", var_name, lvl)
      inter_matches <- actual_colnames[grepl(inter_pattern, actual_colnames)]
      interaction_cols <- c(interaction_cols, inter_matches)
    }

    dummy_cols <- unique(c(main_effect_cols, interaction_cols))

    # Determine base level (NULL if all levels are present = no intercept case)
    possible_main_dummies <- paste0(var_name, levels)
    missing_levels <- setdiff(possible_main_dummies, main_effect_cols)

    base_level <- if (length(missing_levels) == 1) {
      sub(var_name, "", missing_levels[1])
    } else if (length(missing_levels) == 0) {
      NULL   # All levels present => no intercept
    } else {
      cli::cli_alert_warning(
        "Unexpected number of missing levels for factor '{var_name}' in equation '{equation_name}'."
      )
      NULL
    }
    mapping[[var_name]] <- list(
      equation         = equation_name,
      var_name         = var_name,
      levels           = levels,
      main_effect_cols = main_effect_cols,     # <- separate
      interaction_cols = interaction_cols,     # <- separate
      dummy_cols       = dummy_cols,           # combined for convenience
      base_level       = base_level,
      n_dummies        = length(dummy_cols),
      has_interaction  = length(interaction_cols) > 0
    )
  }

  mapping
}

## Build factor mappings for all equations -------------------------------------
# Build factor mappings for all equations
#
# Internal function. Takes a named list of mold objects and returns
# factor mappings for each equation. Also performs safety checks
# for all-NA and invalid predictor columns.
#
# Argument:
#   molds     A named list of mold objects (e.g. `list(value = ..., scale = ...)`).
#
# Returns:
#   A named list with the same names as `molds`, each containing the factor
#   mapping for that equation.
#
#' @keywords internal
.build_factor_mapping <- function(molds)
{
  if (!is.list(molds) || is.null(names(molds))) {
    cli::cli_abort("`molds` must be a named list of mold objects.",
                   call = NULL)
  }

  mapping <- list()

  for (eq_name in names(molds)) {
    mold <- molds[[eq_name]]

    # Check for all-NA columns first (fail fast)
    .check_for_invalid_predictors(mold, equation_name = eq_name)

    # Only map factors if we passed the check
    mapping[[eq_name]] <- .build_factor_map(mold, equation_name = eq_name)
  }

  mapping
}

# Invalid predictors -----------------------------------------------------------
# Check for invalid predictor columns after molding
#
# Internal helper that aborts with a clear error if any predictor
# column contains invalid values (NA, NaN, Inf, or -Inf).
#
# Argument:
#     mold          A mold object returned by hardhat::mold().
#     equation_name Character string indicating the equation name
#                   ("value" or "scale").
#
# Returns:
#   Invisibly returns TRUE if the check passes.
#'
#' @keywords internal
.check_for_invalid_predictors <- function(mold, equation_name = "value")
{
  if (is.null(mold) || is.null(mold$predictors) || ncol(mold$predictors) == 0) {
    return(invisible(TRUE))
  }

  predictors <- mold$predictors

  # Check each column for any invalid value
  has_invalid <- vapply(predictors, function(col) {
    any(is.na(col) | is.nan(col) | is.infinite(col))
  }, logical(1))

  if (any(has_invalid))
  {
    bad_cols <- colnames(predictors)[has_invalid]
    cli::cli_abort(c("Column(s) in the {.strong {equation_name}} equation contain invalid values (NA, NaN, or Inf).",
                     "i" = "Found: {.val {paste(bad_cols, collapse = ', ')}}",
                     "x" = "This usually happens with invalid transformations involving factors.",
                     "i" = "Please revise your formula.")
    )
  }

  invisible(TRUE)
}

# Reconstruct full sample vector -----------------------------------------------
# Uses the sample's logical vector to extend the reduced vector to full sample
# size, introducing NAs in the obserations not used in estimation.
.reconstruct_full_sample_vector <- function(reduced, sample_logical)
{
  if (length(sample_logical) < length(reduced))
    cli::cli_abort("`sample_logical` must have more observations than `reduced`",
                   call = NULL)
  full <- rep(NA_real_, length(sample_logical))
  full[sample_logical] <- reduced
  full
}

# Log transformations ----------------------------------------------------------
# Detect log transformation on the outcome variable
#
# Returns a consistent list structure whether a log transformation is detected or not.
#
# Arguments:
#   formula     A formula with a lhs expression to check for.
#   data        A data.frame that holds the variables in the estimation.
#
# Returns:
#   A list with all the information of a log transformation.
#
#' @keywords internal
.detect_log_transformation <- function(formula, data) {

  lhs_expr <- rlang::f_lhs(formula)

  # No left-hand side -> not a log transformation
  if (is.null(lhs_expr)) {
    return(list(
      is_log        = FALSE,
      log_fun       = NULL,
      shift         = 0,
      multiplier    = 1,
      complex_inner = FALSE,
      var_names     = character(0),
      n_invalid     = 0L,
      invalid_idx   = rep(FALSE, nrow(data)),
      raw_expr      = NULL
    ))
  }

  # Get the expression on the LHS
  expr <- rlang::quo_get_expr(rlang::quo(!!lhs_expr))

  # Not a log function -> not a log transformation
  if (!is.call(expr) || !as.character(expr[[1]]) %in% c("log", "log10", "log1p")) {
    return(list(
      is_log        = FALSE,
      log_fun       = NULL,
      shift         = 0,
      multiplier    = 1,
      complex_inner = FALSE,
      var_names     = character(0),
      n_invalid     = 0L,
      invalid_idx   = rep(FALSE, nrow(data)),
      raw_expr      = NULL
    ))
  }

  # -- Log transformation detected ---------------------------------------------
  fun_name <- as.character(expr[[1]])
  inner_expr <- expr[[2]]

  shift <- if (fun_name == "log1p") 1 else 0
  multiplier <- 1
  complex_inner <- FALSE
  var_names <- character(0)

  # Check for operations inside the log: y + c, c * y, y + z, y * z, etc.
  if (is.call(inner_expr) && length(inner_expr) == 3) {
    op <- as.character(inner_expr[[1]])
    left  <- inner_expr[[2]]
    right <- inner_expr[[3]]

    left_is_var  <- is.symbol(left)
    right_is_var <- is.symbol(right)

    if (left_is_var && right_is_var) {
      # Complex case: two variables (e.g. log(y + z))
      complex_inner <- TRUE
      var_names <- c(as.character(left), as.character(right))
      shift <- 0
      multiplier <- 1
    } else if (op == "+") {
      # Simple shift: log(y + c)
      if (is.numeric(right)) shift <- right
      else if (is.numeric(left)) shift <- left
    } else if (op == "*") {
      # Simple multiplier: log(c * y)
      if (is.numeric(right)) multiplier <- right
      else if (is.numeric(left)) multiplier <- left
    }
  }

  # Evaluate inner expression to count invalid observations
  inner_val <- tryCatch(
    rlang::eval_tidy(rlang::quo(!!inner_expr), data = data),
    error = function(e) rep(NaN, nrow(data))
  )

  if (fun_name %in% c("log", "log10")) {
    invalid <- inner_val <= 0
  } else if (fun_name == "log1p") {
    invalid <- inner_val <= -1
  } else {
    invalid <- rep(FALSE, nrow(data))
  }

  n_invalid <- sum(invalid, na.rm = TRUE)

  if (complex_inner) {
    cli::cli_alert_warning(
      "Complex transformation inside {fun_name}() detected: {deparse(inner_expr)}. \\
       Full back-transformation support may not be available yet."
    )
  }

  list(
    is_log        = TRUE,
    log_fun       = fun_name,
    shift         = shift,
    multiplier    = multiplier,
    complex_inner = complex_inner,
    var_names     = var_names,
    n_invalid     = n_invalid,
    invalid_idx   = invalid,
    raw_expr      = deparse(inner_expr)
  )
}

# Detect log transformations across multiple formulas
#
# Returns a named list with detection results for each formula.
# Useful for future multi-equation models.
#
# Arguments:
#   formulas_list A list with the different formulas for the different
#                 equations that need checking.
#   data          A data.frame that holds all the data in the estimation.
#
# Returns:
#   A list with the same number of elements as in `formulas_list`, each with an
#   inner list with all the information about the log-transformation for the
#   respective equation.
#
#' @keywords internal
.detect_log_transformations <- function(formulas_list, data) {
  if (!is.list(formulas_list) || is.null(names(formulas_list))) {
    cli::cli_abort("`formulas_list` must be a named list of formulas.",
                   call = NULL)
  }

  lapply(formulas_list, function(f) {
    if (is.null(f)) return(list(is_log = FALSE))
    .detect_log_transformation(f, data)
  })
}

# Constraints parsing ----------------------------------------------------------
# Parse user-friendly constraints into maxLik format
#
# Arguments:
#   constraints   User input. Can be:
#     - NULL (no constraints)
#     - A character vector of string constraints
#     - A named list of string constraints (labels become names)
#     - A raw maxLik list with eqA/eqB or ineqA/ineqB
#   coef_names Character vector of all coefficient names in the model
#
# Returns:
#   A list with three elements:
#     - names: character vector of constraint labels (or NULL)
#     - strings: character vector of original string constraints (or NULL)
#     - maxLik: list ready for maxLik (eqA/eqB or ineqA/ineqB) or NULL
#
#' @keywords internal
.parse_constraints <- function(constraints = NULL, coef_names)
{
  if (is.null(constraints) || length(constraints) == 0) {
    return(list(names = NULL, strings = NULL, maxLik = NULL))
  }

  # Case 1: Raw maxLik format (user knows what they're doing)
  if (is.list(constraints) &&
      (all(c("eqA", "eqB") %in% names(constraints)) ||
       all(c("ineqA", "ineqB") %in% names(constraints)))) {
    return(list(names = NULL, strings = NULL, maxLik = constraints))
  }

  # Case 2: Named list of string constraints
  if (is.list(constraints) && !is.null(names(constraints))) {
    strings <- unlist(constraints, use.names = FALSE)
    names_vec <- names(constraints)
  }
  # Case 3: Simple character vector
  else if (is.character(constraints)) {
    strings <- constraints
    names_vec <- NULL
  }
  else {
    cli::cli_abort("`constraints` must be NULL, a character vector, a named list, or a raw maxLik list.",
                   call = NULL)
  }
  
  # Parse the string constraints into maxLik format
  maxLik_list <- .parse_string_constraints(strings, coef_names)
  
  # Check that we don't have equality and inequality constraints together
  if (!is.null(maxLik_list$eqA) && !is.null(maxLik_list$ineqA) && 
      nrow(maxLik_list$eqA) > 0 && nrow(maxLik_list$ineqA) > 0) {
    cli::cli_abort(
      c("Cannot mix equality and inequality constraints in the same model.",
        "i" = "Use only '=' for equality or '>='/'<=' for inequality."),
      call = NULL
    )
  }

  return(list(
    names   = names_vec,
    strings = strings,
    maxLik  = maxLik_list
  ))
}

# Internal helper: parse string constraints into maxLik matrices
#
# Arguments:
#   strings     A vector with the strings defining the linear constraints.
#   coef_names  Character vector of all coefficient names in the model.
# Returns:
#   A list that conforms with [maxLik][maxLik::maxLik]'s constraint
#   requirements.
#
#' @keywords internal
.parse_string_constraints <- function(strings, coef_names)
{
  eq_rows <- list()
  ineq_rows <- list()

  for (constr in strings) {
    constr <- trimws(constr)

    # Detect operator
    if (grepl(">=", constr)) {
      op <- ">="
      parts <- strsplit(constr, ">=", fixed = TRUE)[[1]]
    } else if (grepl("<=", constr)) {
      op <- "<="
      parts <- strsplit(constr, "<=", fixed = TRUE)[[1]]
    } else if (grepl("=", constr)) {
      op <- "="
      parts <- strsplit(constr, "=", fixed = TRUE)[[1]]
    } else {
      cli::cli_abort("Constraint must contain =, >= or <= : {.val {constr}}", call = NULL)
    }

    if (length(parts) != 2) {
      cli::cli_abort("Invalid constraint format: {.val {constr}}", call = NULL)
    }

    lhs <- trimws(parts[1])
    
    suppressWarnings({
      rhs <- as.numeric(trimws(parts[2]))
    })

    if (is.na(rhs)) {
      cli::cli_abort("Right-hand side must be numeric in constraint: {.val {constr}}",
                     call = NULL)
    }

    row <- .parse_linear_expression(lhs, coef_names)

    if (op == "=") {
      eq_rows[[length(eq_rows) + 1]] <- c(row, -rhs)
    } else if (op == ">=") {
      ineq_rows[[length(ineq_rows) + 1]] <- c(row, -rhs)
    } else if (op == "<=") {
      ineq_rows[[length(ineq_rows) + 1]] <- c(-row, rhs)
    }
  }

  result <- list()

  if (length(eq_rows) > 0) {
    result$eqA <- do.call(rbind, lapply(eq_rows, function(r) r[-length(r)]))
    result$eqB <- sapply(eq_rows, function(r) r[length(r)])
  }

  if (length(ineq_rows) > 0) {
    result$ineqA <- do.call(rbind, lapply(ineq_rows, function(r) r[-length(r)]))
    result$ineqB <- sapply(ineq_rows, function(r) r[length(r)])
  }

  result
}

# Parse a linear expression on the left-hand side of a constraint
#
# Turns a string like "value::hp - 2*value::wt + value::(Intercept)/3"
# into a numeric vector of length = number of coefficients.
#
# Arguments:
#   expr        Character string containing the left-hand side expression.
#   coef_names  Character vector of all coefficient names in the model.
#
# Returns:
#   Numeric vector of length `length(coef_names)` with the multipliers
#   for each coefficient. Zeros for coefficients not present in the expression.
#
#' @keywords internal
.parse_linear_expression <- function(expr, coef_names)
{
  if (!is.character(expr) || length(expr) != 1) {
    cli::cli_abort("`expr` must be a single character string.", call = NULL)
  }

  # Remove all whitespace
  expr <- gsub("\\s+", "", expr)

  # Split by + and - while preserving the signs
  # This is a simple tokenizer for linear expressions
  terms <- unlist(regmatches(expr, gregexpr("[+-]?[^+-]+", expr)))

  # Initialize coefficient vector with zeros
  coef_vec <- setNames(rep(0, length(coef_names)), coef_names)

  for (term in terms) {
    term <- trimws(term)

    # Extract sign
    sign <- 1
    if (substr(term, 1, 1) == "-") {
      sign <- -1
      term <- substr(term, 2, nchar(term))
    } else if (substr(term, 1, 1) == "+") {
      term <- substr(term, 2, nchar(term))
    }

    # Check if term contains * or /
    if (grepl("[*/]", term)) {
      # Split into parts
      parts <- unlist(strsplit(term, "[*/]"))
      if (length(parts) != 2) {
        cli::cli_abort("Invalid term in constraint: {.val {term}}", call = NULL)
      }

      part1 <- trimws(parts[1])
      part2 <- trimws(parts[2])

      # Determine which is the coefficient and which is the constant
      if (grepl("^value::|^scale::", part1)) {
        coef_name <- part1
        constant_str <- part2
      } else if (grepl("^value::|^scale::", part2)) {
        coef_name <- part2
        constant_str <- part1
      } else {
        cli::cli_abort("Term must contain exactly one coefficient: {.val {term}}", call = NULL)
      }

      constant <- as.numeric(constant_str)
      if (is.na(constant)) {
        cli::cli_abort("Invalid constant in term: {.val {term}}", call = NULL)
      }

      # Apply multiplication or division
      if (grepl("\\*", term)) {
        multiplier <- sign * constant
      } else { # division
        multiplier <- sign / constant
      }

      if (!coef_name %in% coef_names) {
        cli::cli_abort("Coefficient {.val {coef_name}} not found in model.", call = NULL)
      }

      coef_vec[coef_name] <- coef_vec[coef_name] + multiplier

    } else {
      # Simple term: either a coefficient or a constant (should not happen)
      if (grepl("^value::|^scale::", term)) {
        coef_name <- term
        if (!coef_name %in% coef_names) {
          cli::cli_abort("Coefficient {.val {coef_name}} not found in model.", call = NULL)
        }
        coef_vec[coef_name] <- coef_vec[coef_name] + sign
      } else {
        # Pure constant - ignore for now (can be used in more complex expressions later)
        next
      }
    }
  }

  return(coef_vec)
}

# Initial Values ---------------------------------------------------------------
# Search for feasible initial values
#
# Internal helper used by `.ml_*.fit()` functions when default starting values
# produce an infeasible log-likelihood. It first tries adjusting the intercepts,
# then sets all coefficients to zero if needed, and finally tries scaling the
# vector to improve the log-likelihood.
#
# Arguments:
#   fn      The log-likelihood function (e.g. `ml_lm_ll` or `ml_logit_ll`).
#   b       Numeric vector of starting values.
#   ...     Further arguments passed to fn.
#
# Returns:
#   A numeric vector of initial values. It has an attribute feasible
#   (TRUE if a feasible vector was found, FALSE otherwise).
#
#' @keywords internal
.initial_values.mlmodel <- function(fn, b, ...)
{
  ll <- fn(b, ...)

  # Make a working copy
  b1 <- b
  attr(b1, "feasible") <- FALSE

  # Check feasibility
  if (any(!is.finite(ll))) {
    cli::cli_alert_info("Log-likelihood infeasible at initial values. Searching for feasible start...")

    # Identify intercept positions
    is_intercept <- grepl("(Intercept)", names(b), fixed = TRUE)

    # Only attempt intercept search if there are any intercepts
    if (any(is_intercept)) {
      for (g in seq(0, 10, by = 0.25)) {
        # Try negative
        b1[is_intercept] <- -g
        if (all(is.finite(fn(b1, ...)))) {
          attr(b1, "feasible") <- TRUE
          cli::cli_alert_info("Found feasible initial values.")
          break
        }
        # Try positive
        b1[is_intercept] <- g
        if (all(is.finite(fn(b1, ...)))) {
          attr(b1, "feasible") <- TRUE
          cli::cli_alert_info("Found feasible initial values.")
          break
        }
      }

      # If still infeasible, set all coefficients to zero and try again
      if (!attr(b1, "feasible")) {
        b1 <- rep(0, length(b))
        names(b1) <- names(b)
        for (g in seq(0, 10, by = 0.25)) {
          b1[is_intercept] <- -g
          if (all(is.finite(fn(b1, ...)))) {
            attr(b1, "feasible") <- TRUE
            cli::cli_alert_info("Found feasible initial values.")
            break
          }
          b1[is_intercept] <- g
          if (all(is.finite(fn(b1, ...)))) {
            attr(b1, "feasible") <- TRUE
            cli::cli_alert_info("Found feasible initial values.")
            break
          }
        }
      }
    } else {
      # No intercepts - set everything to zero and check
      b1 <- rep(0, length(b))
      names(b1) <- names(b)
      if (all(is.finite(fn(b1, ...)))) {
        attr(b1, "feasible") <- TRUE
        cli::cli_alert_info("Found feasible initial values (all zero).")
      }
    }
  } else {
    # Original vector was already feasible
    b1 <- b
    attr(b1, "feasible") <- TRUE
  }

  # If we have feasible values, try scaling to improve log-likelihood
  if (attr(b1, "feasible")) {
    ll0 <- sum(fn(b1, ...))
    scale <- 1

    # Try halving
    ll_half <- fn(b1 / 2, ...)
    if (all(is.finite(ll_half)) && sum(ll_half) > ll0) {
      scale <- 0.5
    } else {
      # Try doubling
      ll_double <- fn(b1 * 2, ...)
      if (all(is.finite(ll_double)) && sum(ll_double) > ll0) {
        scale <- 2
      }
    }

    # Apply scaling iteratively if it improves the likelihood
    if (scale != 1) {
      cli::cli_alert_info("Improving initial values by scaling (factor = {scale}).")
      cli::cli_alert_info("Initial log-likelihood: {round(ll0, 3)}")
      for (i in 1:20) {   # safety limit
        b2 <- b1 * scale
        ll2 <- sum(fn(b2, ...))
        if (!is.finite(ll2) || (ll2 - ll0) <= 0.1) break
        b1 <- b2
        ll0 <- ll2
      }
      cli::cli_alert_info("Final scaled log-likelihood: {round(ll0, 3)}")
    }
  }

  b1
}

# Changing ptypes from a mold --------------------------------------------------
# Function to change ptypes in a mold from int to double.
#
# This function was coded to ensure that you could pass average values in
# predictions for variables in the model that were stored as integers in the data.
# However, it is currently not used, since we decided to directly change the
# data's variable types to double if they were integers.
#
# Argument:
#   mold    An object returned by hardhat::mold.
#
# Returns
#   The mold object with the changed ptype tibble.
#
#' @keywords internal
.mold_fix_integer_to_double <- function(mold) {
  if (is.null(mold$blueprint) || is.null(mold$blueprint$ptypes)) {
    return(mold)
  }
  
  ptypes <- mold$blueprint$ptypes$predictors
  
  for (var in names(ptypes)) {
    if (is.integer(ptypes[[var]])) {
      # Change the prototype from integer to double
      ptypes[[var]] <- double()
    }
  }
  
  # Update the blueprint with the modified ptypes
  mold$blueprint$ptypes$predictors <- ptypes
  
  mold
}

# Function to convert variables stored as integers in a data frame into
# doubles. To be called before using hardhat::mold in the estimator's function.
#
# This is the function we actually use within the ml_* functions to ensure that
# you can do predictions with decimal values for integer variables (predictions
# at means, for example).
#
# Argument:
#   data    A data.frame with the data to check.
#
# Returns:
#   The data.frame with its integer variables changed to doubles.
#
#' @keywords internal
.convert_integers_to_double <- function(data) {
  if (!is.data.frame(data)) {
    return(data)
  }
  
  for (col in colnames(data)) {
    if (is.integer(data[[col]])) {
      data[[col]] <- as.double(data[[col]])
    }
  }
  
  data
}

## FRACTIONAL RESPONSE INFERENCE ===============================================
# --- Logit/Probit check for fractional response
#' @keywords internal
.fractional_response_inference_alert <- function(object, vcov)
{
  
  if (!inherits(object, c("ml_logit", "ml_probit")) || isTRUE(object$model$is_binary)) {
    return(invisible(TRUE))
  }
  
  vcov_type <- attr(vcov, "vcov.type") %||% "user-supplied"
  
  # Checking for fractional response estimation, and variance
  if (!object$model$is_binary) {
    if (vcov_type %in% c("oim", "opg")) {
      cli::cli_warn(
        c("Outcome appears to be fractional (values strictly between 0 and 1).",
          "i" = "Estimation is quasi-maximum likelihood (QMLE).",
          "i" = "Variance type `'{vcov_type}'` is not appropriate in this case.",
          "i" = "It is strongly recommended to use robust standard errors.",
          "i" = "Consider setting `vcov.type = \"robust\"` or `vcov.type = \"boot\"`.")
      )
    } 
    else if (vcov_type == "user-supplied") {
      cli::cli_warn(
        c("Outcome appears to be fractional (values strictly between 0 and 1).",
          "i" = "Estimation is quasi-maximum likelihood (QMLE).",
          "i" = "Could not determine the type of variance matrix used.",
          "i" = "It is strongly recommended to use robust standard errors.",
          "i" = "Consider using `vcov.type = \"robust\"` or `vcov.type = \"boot\"`.")
      )
    }
  }
  
  invisible(TRUE)
}

## PREDICTION HELPERS ==========================================================
# --- Align estimates to original dataset --------------------------------------
#' @keywords internal
.predict_align_estimates <- function(object, x) {
  UseMethod(".predict_align_estimates")
}
#' @keywords internal
.predict_align_estimates.mlmodel <- function(object, x) {
  
  if (!inherits(object, "mlmodel"))
    cli::cli_abort("`object` must be an 'mlmodel' object.", call = NULL)
  
  if (!is.numeric(x))
    cli::cli_abort("`x` must be a numeric vector.", call = NULL)
  
  samp <- object$model$sample
  
  # Already full length (e.g. user passed newdata)
  if (length(x) == length(samp)) {
    return(x)
  }
  
  # Length matches number of observations used in estimation
  n_used <- sum(samp)
  if (length(x) != n_used) {
    cli::cli_abort(paste0(
      "`x` has length {.val {length(x)}}, but the model used ",
      "{.val {n_used}} observations."
    ), call = NULL)
  }
  
  # Align to full original data
  full <- rep(NA_real_, length(samp))
  full[samp] <- x
  
  return(full)
}

# --- Prepare data for predictions ---------------------------------------------

#' @keywords internal
.prepare_prediction_data <- function(object, newdata = NULL)
{
  UseMethod(".prepare_prediction_data")
}

#' @keywords internal
.prepare_prediction_data.mlmodel <- function(object, newdata = NULL) {
  
  if (!inherits(object, "mlmodel"))
    cli::cli_abort("`object` must be an 'mlmodel' object.")
  
  if (is.null(newdata)) {
    # -- In-sample prediction --------------------------------------------------
    X <- as.matrix(object$model$value$predictors)
    
    # Scale matrix
    if (is.null(object$model$scale)) {
      Z <- NULL                          # Poisson or homoskedastic logit/probit
    } else {
      Z <- as.matrix(object$model$scale$predictors)
    }
    
  } else {
    # -- Out-of-sample prediction ----------------------------------------------
    # Value predictors
    val_bp <- object$model$value$blueprint
    forged_val <- hardhat::forge(newdata, blueprint = val_bp, outcomes = FALSE)
    X <- as.matrix(forged_val$predictors)
    
    # Scale predictors
    if (is.null(object$model$scale)) {
      Z <- NULL                          # Poisson or homoskedastic logit/probit
      
    } else if (is.null(object$model$scale$blueprint)) {
      # Homoskedastic models that always have a scale (lm, gamma, beta, negbin)
      Z <- matrix(1, nrow = nrow(X), ncol = 1)
      
    } else {
      # Heteroskedastic case (including logit/probit with scale)
      scale_bp <- object$model$scale$blueprint
      forged_scale <- hardhat::forge(newdata, blueprint = scale_bp, outcomes = FALSE)
      Z <- as.matrix(forged_scale$predictors)
    }
  }
  
  list(X = X, Z = Z)
}

# --- Type parsing for probabilities -------------------------------------------
# Internal parser for probability types.
#
# For models in which we can predict probabilities P(,) or P(), we parse
# these strings to pull the information about the probability that wants to be
# predicted.
#
# Arguments:
#   type    Character string passed by the user (as a type to Predict())
#
# Returns:
#   A list with components:
#     - base_type: "link", "response", or "prob"
#     - prob_type: NULL, "exact", "leq", "geq", or "interval"
#     - lower: lower bound (NULL if not applicable)
#     - upper: upper bound (NULL if not applicable)
#
# prob_type can only be NULL when base_type is not "prob". So in the probability
# block of the prediction function, you don't need to account for NULL.
# 
# If `prob_type` is "exact" both lower and upper hold the value. So you can
# use either.
# 
#' @keywords internal
.predict_types_parsing <- function(type)
{
  type <- tolower(trimws(type))
  
  # Not a probability request -> return as base_type
  if (!grepl("^p\\(.*\\)$", type)) {
    return(list(base_type = type, 
                prob_type = NULL, 
                lower = NULL, 
                upper = NULL))
  }
  
  # --- Probability syntax P(...) ---
  content <- sub("^p\\((.*)\\)$", "\\1", type)
  content <- trimws(content)
  
  if (content == "") {
    cli::cli_abort("Empty P() is not allowed.", call = NULL)
  }
  
  if (!grepl(",", content))
  {
    # P(k) -> exact count
    k <- suppressWarnings(as.numeric(content))
    if (is.na(k) || !is.finite(k) || k < 0 || k != round(k)) {
      cli::cli_abort("P(k) requires a non-negative integer k.", call = NULL)
    }
    return(list(base_type = "prob", prob_type = "exact", lower = k, upper = k))
  }
  else
  {
    parts <- strsplit(content, ",")[[1]]
    parts <- trimws(parts)
    
    if (length(parts) == 1) {
      # GEQ
      k <- suppressWarnings(as.numeric(parts[1]))
      if (is.na(k) || !is.finite(k) || k < 0 || k != round(k)) {
        cli::cli_abort("P(k,) requires a non-negative integer k.", call = NULL)
      }
      return(list(base_type = "prob", prob_type = "geq", lower = k, upper = NULL))
    }
    else if(parts[1] == "")
    {
      # LEQ
      k <- suppressWarnings(as.numeric(parts[2]))
      if (is.na(k) || !is.finite(k) || k < 0 || k != round(k)) {
        cli::cli_abort("P(,k) requires a non-negative integer k.", call = NULL)
      }
      return(list(base_type = "prob", prob_type = "leq", lower = NULL, upper = k))
    }
    else
    {
      # interval
      lower <- suppressWarnings(as.numeric(parts[1]))
      upper <- suppressWarnings(as.numeric(parts[2]))
      if (is.na(lower) || !is.finite(lower) || lower < 0 || lower != round(lower)) {
        cli::cli_abort("In P(a,b), a and b need to be non-negative integers.", call = NULL)
      }
      if (is.na(upper) || !is.finite(upper) || upper < 0 || upper != round(upper)) {
        cli::cli_abort("In P(a,b), a and b need to be non-negative integers.", call = NULL)
      }
      if (upper <= lower)
        cli::cli_abort("In P(a,b), b needs to be greater than a.", call = NULL)
      return(list(base_type = "prob", prob_type = "interval", lower = lower, upper = upper))
    }
  }
  cli::cli_abort("Invalid probability syntax '{type}'. Use P(k), P(,k), P(k,), or P(a,b).",
                 call = NULL)
}

## SUBSET PROCESSING ===========================================================
# Helper function to process the subset argument in estimation.
#
# Arguments:
#   subset_expr   either the expression to evaluate into a logical vector or the
#                 logical vector itself.
#   data          The data.frame used in the estimation.
#
# Returns:
#   A list with two elements
#     - expr: the enquoted expression from subset_expr
#     - idx:  the logical vector with the indices of the observations to be used
#             in estimation.
#
#' @keywords internal
.process_subset <- function(subset_expr, data) {
  n_orig <- nrow(data)
  
  if (rlang::quo_is_missing(subset_expr) || rlang::quo_is_null(subset_expr)) {
    res <- list(
      expr = NULL,
      idx = rep(TRUE, n_orig)
    )
    return(res)
  }
  
  raw_expr <- rlang::quo_get_expr(subset_expr)
  
  # If it's already a logical vector, evaluate it as is
  # If it's a symbolic expression, evaluate in the context of 'data'
  subset_idx <- if (is.logical(raw_expr)) {
    rlang::eval_tidy(subset_expr)
  } else {
    rlang::eval_tidy(subset_expr, data = data)
  }
  
  if (!is.logical(subset_idx) || length(subset_idx) != n_orig) {
    cli::cli_abort("`subset` must result in a logical vector of length {n_orig}.")
  }
  
  subset_idx[is.na(subset_idx)] <- FALSE
  
  res <- list(expr = raw_expr,
              idx = subset_idx)
  
  return(res)
}

## VARIANCE HELPERS ============================================================
# --- 1. Cluster info ----------------------------------------------------------
# Internal helper to extract clustering information
#
# Used by several functions to that accept `cl_var` as an argument, to check the
# validity of the clustering variable, and get the number of clusters, as well
# as the name of the clustering variable if `cl_var` is a string.
#
# Arguments:
#   object      An `mlmodel` object.
#   cl_var      Character string or vector. The clustering variable.
#
# Returns:
#   A list with
#     - var_name:   the name of the clustering variable if the user passed the name
#     - n_cluster:  the number of unique elements in the variable (clusters)
#     - ids:        the values of the unique elements in the variable (the cluster ids)
#
#' @keywords internal
.vcov_cluster_info <- function(object, cl_var)
{
  if(!inherits(object, "mlmodel"))
    cli::cli_abort("`object` needs to be of class `'mlmodel'`",
                   call = NULL)
  if (is.character(cl_var))
  {
    # User passed variable name
    if (!is.null(object$model$data) && is.data.frame(object$model$data)) {
      d <- object$model$data
    } else if (!is.null(object$model$d_name) && object$model$d_name != "<unknown data>") {
      d <- tryCatch(get(object$model$d_name), error = function(e) {
        cli::cli_abort("Cannot retrieve the dataset to get the clustering variable.",
                       call = NULL)
      })
    } else {
      cli::cli_abort("Dataset and its name not stored; cannot retrieve clustering variable.",
                     call = NULL)
    }
    cl_vec <- d[[cl_var]][object$model$sample]
    vcov.cluster <- list(var_name = cl_var,
                         n_cluster = length(unique(cl_vec)),
                         ids = unique(cl_vec))
  }
  else
  {
    # User passed a vector directly
    cl_vec <- if (length(cl_var) == object$model$n_orig)
      cl_var[object$model$sample]
    else
      cl_var
    vcov.cluster <- list(var_name = NULL,
                         n_cluster = length(unique(cl_vec)),
                         ids = unique(cl_vec))
  }
  
  return(vcov.cluster)
}

# --- 2. process_vcov ----------------------------------------------------------
# Internal helper to obtain the variance-covariance matrix
#
# This helper is used by `summary.ml_lm()`, `waldtest.mlmodel()`,
# `predict.ml_lm()` and other functions that take vcov and vcov.type as arguments.
#
# If a matrix is supplied via the `vcov` argument, it is validated and
# returned directly. Otherwise, it calls `vcov.mlmodel()`.
#
# Arguments:
#   object        An `mlmodel` object.
#   vcov          An optional user-supplied variance-covariance matrix. If
#                 provided, it will be used instead of computing a new one.
#   vcov.type     Character string specifying the type of variance-covariance
#                 matrix to compute. See vcov.mlmodel to see the types it accepts.
#   cl_var        Character string or vector. Clustering variable. When clustering
#                 a variance type.
#   repetitions   Integer. Number of bootstrap replications when `vcov.type = 
#                 "boot"`. Default is 999.
#   seed          Integer. Random seed for reproducibility when `vcov.type = "boot"`.
#                 If `NULL`, a random seed is generated.
#   progress      Logical, Should a progress bar be displayed during bootstrapping
#                 or jackknifing? Default is FALSE.
#   ...           Further arguments passed to vcov.mlmodel().
#
# Returns
#   The variance-covariance matrix.
#
#' @keywords internal
.process_vcov <- function(object,
                          vcov = NULL,           # User-supplied variance matrix
                          vcov.type = "oim",
                          cl_var = NULL,
                          repetitions = 999,
                          seed = NULL,
                          progress = FALSE,
                          ...)
{
  # Case 1: User passed a pre-computed variance matrix
  if (!is.null(vcov)) {
    if (!is.matrix(vcov))
      cli::cli_abort("`vcov` must be a matrix when supplied directly.",
                     call = NULL)
    
    n_coef <- length(coef(object))
    
    # Check dimensions
    if (!identical(dim(vcov), c(n_coef, n_coef)))
      cli::cli_abort(c("Supplied `vcov` matrix has incorrect dimensions ({nrow(vcov)} x {ncol(vcov)}).,
                       Expected {n_coef} x {n_coef} (matching number of coefficients)."),
                     call = NULL)
    
    # Optional: check row/column names
    coef_names <- names(coef(object))
    if (!is.null(rownames(vcov)) && !identical(rownames(vcov), coef_names))
      cli::cli_warn("Row names of supplied `vcov` do not match coefficient names.",
                    call = NULL)
    
    if (!is.null(colnames(vcov)) && !identical(colnames(vcov), coef_names))
      cli::cli_warn("Column names of supplied `vcov` do not match coefficient names.",
                    call = NULL)
    
    return(vcov)
  }
  
  # Case 2: Generate variance using vcov.mlmodel()
  vcov(object,
       type   = vcov.type,
       cl_var      = cl_var,
       repetitions = repetitions,
       seed        = seed,
       progress    = progress)
}

# --- 3. vcov_boot -------------------------------------------------------------
# --- 3.1. Generic -------------------------------------------------------------
# Bootstrap Variance-Covariance Matrix (mlmodel method)
#
# Internal function to compute bootstrapped variance-covariance matrix. General
# method that uses the general update() function or the specific ones for the
# models.
#
# Models usually have a specific boostrap helper that uses the data from estimation
# without having to process it again, and calls its fit function directly. This
# method acts as a backup/safeguard for those models that don't.
#
# Arguments:
#   object      An mlmodel object.
#   repetitions Number of bootstrap replications.
#   seed        Random seed for reproducibility.
#   cl_var      Clustering variable (if clustered bootstrap).
#   progress    Logical. Whether to show progress bar.
#   ...         Not currently used.
#
# Called by mlmodels::vcov() when type is set to "boot"
#
#' @keywords internal
.vcov_boot <- function(object, ...) {
  UseMethod(".vcov_boot")
}

# --- 3.2. mlmodel -------------------------------------------------------------
#' @keywords internal
.vcov_boot.mlmodel <- function(object,
                               repetitions = 999,
                               seed = NULL,
                               cl_var = NULL,
                               progress = TRUE,
                               ...)
{
  if(!inherits(object, "mlmodel"))
    cli::cli_abort("`object` needs to be of class 'mlmodel'.")
  if (is.null(seed)) seed <- sample.int(1e6, 1)
  set.seed(seed)
  
  if(!is.null(object$model$data) && is.data.frame(object$model$data))
    original_data <- object$model$data
  else tryCatch({
    if (!is.null(object$call$d_name)) {
      eval(object$call$d_name, envir = parent.frame(2))
    } else if (!is.null(object$model$d_name) && object$model$d_name != "<unknown data>") {
      get(object$model$d_name, envir = .GlobalEnv)
    } else {
      cli::cli_abort("Could not recover original data", call = NULL)
    }
  }, error = function(e) {
    cli::cli_abort("Could not recover the original data for bootstrapping.", call = NULL)
  })
  
  # Use only the observations actually used in estimation
  used_data <- original_data[object$model$sample, , drop = FALSE]
  w <- object$model$weights %||% rep(1, nrow(used_data))
  
  # -- Prepare for clustered bootstrap if requested ----------------------------
  is_clustered <- !is.null(cl_var)
  if (is_clustered) {
    cluster_ids <- unique(cl_var[object$model$sample])
    n_cluster   <- length(cluster_ids)
  }
  
  # -- Bootstrap loop ----------------------------------------------------------
  if (progress) {
    if (is_clustered) {
      cli::cli_alert_info("Clustered bootstrap with {.val {repetitions}} repetitions and {.val {n_cluster}} clusters.")
    } else {
      cli::cli_alert_info("Bootstrap with {.val {repetitions}} repetitions.")
    }
    cat(cli::col_blue(" 0"))
    for (i in seq(10, 50, by = 10)) cat(cli::col_blue(sprintf("%10d", i)))
    cat("\n")
    cat(cli::col_blue(strrep("=", 52), "\n"))
  }
  
  success     <- logical(repetitions)
  coef_matrix <- matrix(NA_real_, nrow = repetitions, ncol = length(coef(object)))
  
  for (i in seq_len(repetitions)) {
    if (progress && i %% 50 == 1 && i > 1) cat("\n ")
    else if(progress && i == 1) cat(" ")
    
    tryCatch({
      if (is_clustered) {
        sampled_clusters <- sample(cluster_ids, size = n_cluster, replace = TRUE)
        boot_idx <- unlist(lapply(sampled_clusters, function(cid) {
          which(cl_var[object$model$sample] == cid)
        }))
      } else {
        boot_idx <- sample(nrow(used_data), nrow(used_data), replace = TRUE)
      }
      
      boot_data <- used_data[boot_idx, , drop = FALSE]
      w_boot    <- w[boot_idx]
      
      # Use update() for the general case
      suppressMessages({
        updated <- update(object, data = boot_data, weights = w_boot, evaluate = TRUE)
      })
      
      if (updated$code %in% c(0L, 1L, 2L, 8L)) {
        if (progress) cat(cli::col_green("."))
        success[i] <- TRUE
        coef_matrix[i, ] <- coef(updated)
      } else {
        if (progress) cat(cli::col_red("x"))
        success[i] <- FALSE
        coef_matrix[i, ] <- NA_real_
      }
    }, error = function(e) {
      if (progress) cat(cli::col_red("x"))
      success[i] <- FALSE
      coef_matrix[i, ] <- NA_real_
    })
  }
  
  if (progress) {
    cat("\n")
    cat(cli::col_blue(strrep("=", 52), "\n"))
  }
  
  # Final summary
  success_rate <- mean(success) * 100
  cli::cli_text("Bootstrapping finished - {round(success_rate, 1)}% of replications converged.")
  
  if (success_rate < 70) {
    cli::cli_warn("Low convergence rate - bootstrap results may be unreliable.")
  }
  
  # Variance from successful replications only
  valid_rows <- complete.cases(coef_matrix)
  vcov_boot  <- var(coef_matrix[valid_rows, , drop = FALSE])
  
  attr(vcov_boot, "repetitions") <- repetitions
  attr(vcov_boot, "n_success") <- sum(success)
  attr(vcov_boot, "n_failure") <- repetitions - sum(success)
  attr(vcov_boot, "success_rate") <- success_rate
  
  dimnames(vcov_boot) <- list(names(coef(object)), names(coef(object)))
  vcov_boot
}

# --- 4. vcov_jack -------------------------------------------------------------
# --- 4.1. Generic -------------------------------------------------------------
#Jackknife Variance-Covariance Matrix (mlmodel method)
#
# Internal function to compute a jackknife variance-covariance matrix. General
# method that uses the general update() function or the specific ones for the
# models.
#
# Models usually have a specific jakknife helper that uses the data from estimation
# without having to process it again, and calls its fit function directly. This
# method acts as a backup/safeguard for those models that don't.
#
# Arguments:
#   object    An mlmodel object.
#   cl_var    Clustering variable (if clustered jackknife).
#   progress  Logical. Whether to show progress bar.
#   ...       Not currently used.
#
# Called by vcov() when type is set to "jack" or "jackknife".
#
#' @keywords internal
.vcov_jack <- function(object, ...) {
  UseMethod(".vcov_jack")
}

# --- 4.2. mlmodel -------------------------------------------------------------
#' @keywords internal
.vcov_jack.mlmodel <- function(object,
                               cl_var = NULL,
                               progress = TRUE,
                               ...)
{
  if(!inherits(object, "mlmodel"))
    cli::cli_abort("`object` needs to be of class 'mlmodel'.")
  
  if(!is.null(object$model$data) && is.data.frame(object$model$data))
    original_data <- object$model$data
  else tryCatch({
    if (!is.null(object$call$d_name)) {
      eval(object$call$d_name, envir = parent.frame(2))
    } else if (!is.null(object$model$d_name) && object$model$d_name != "<unknown data>") {
      get(object$model$d_name, envir = .GlobalEnv)
    } else {
      cli::cli_abort("Could not recover original data", call = NULL)
    }
  }, error = function(e) {
    cli::cli_abort("Could not recover the original data for jackknife", call = NULL)
  })
  
  # Use only the observations actually used in estimation
  used_data <- original_data[object$model$sample, , drop = FALSE]
  w <- object$model$weights %||% rep(1, nrow(used_data))
  n_obs <- nrow(used_data)
  
  # -- Prepare for clustered jackknife if requested ----------------------------
  is_clustered <- !is.null(cl_var)
  if (is_clustered) {
    cl_var <- cl_var[object$model$sample]
    cluster_ids <- unique(cl_var)
    n_cluster   <- length(cluster_ids)
    n_jack <- n_cluster
  }
  else
    n_jack <- n_obs
  
  # -- Bootstrap loop ----------------------------------------------------------
  if (progress) {
    if (is_clustered) {
      cli::cli_alert_info("Clustered jackknife variance with {.val {n_jack}} clusters.")
    } else {
      cli::cli_alert_info("Jackknife variance.")
    }
    cat(cli::col_blue(" 0"))
    for (i in seq(10, 50, by = 10)) cat(cli::col_blue(sprintf("%10d", i)))
    cat("\n")
    cat(cli::col_blue(strrep("=", 52), "\n"))
  }
  
  success     <- logical(n_jack)
  coef_matrix <- matrix(NA_real_, nrow = n_jack, ncol = length(coef(object)))
  
  for (i in seq_len(n_jack)) {
    if (progress && i %% 50 == 1 && i > 1) cat("\n ")
    else if(progress && i == 1) cat(" ")
    
    tryCatch({
      if (is_clustered) {
        # We are looping through cluster ids, so we have to leave out the current
        # cluster from the data.
        keep_idx <- cl_var != cluster_ids[i]
        jack_data <- used_data[keep_idx, , drop = FALSE]
        w_jack <- w[keep_idx]
      } else {
        jack_data <- used_data[-i, , drop = FALSE]
        w_jack <- w[-i]
      }
      
      # Use update() for the general case
      suppressMessages({
        updated <- update(object, data = jack_data, weights = w_jack, evaluate = TRUE)
      })
      
      if (updated$code %in% c(0L, 1L, 2L, 8L)) {
        if (progress) cat(cli::col_green("."))
        success[i] <- TRUE
        coef_matrix[i, ] <- coef(updated)
      } else {
        if (progress) cat(cli::col_red("x"))
        success[i] <- FALSE
        coef_matrix[i, ] <- NA_real_
      }
    }, error = function(e) {
      if (progress) cat(cli::col_red("x"))
      success[i] <- FALSE
      coef_matrix[i, ] <- NA_real_
    })
  }
  
  if (progress) {
    cat("\n")
    cat(cli::col_blue(strrep("=", 52), "\n"))
  }
  
  # Final summary
  success_rate <- mean(success) * 100
  if(progress && success_rate == 100)
  {
    cli::cli_text("Bootstrapping finished - {.val {round(success_rate, 1)}}% of replications converged.")
  }
  
  valid_rows <- complete.cases(coef_matrix)
  valid_coef <- coef_matrix[valid_rows, , drop = FALSE]
  n_valid    <- nrow(valid_coef)
  
  if (n_valid == 0) {
    cli::cli_abort("All jackknife replications failed.")
  }
  
  if (n_valid < n_jack) {
    cli::cli_warn(
      "Jackknife variance computed from only {.val {n_valid}} out of {.val {n_jack}} successful replications ({.val {round(success_rate, 1)}}%)."
    )
  }
  
  theta_bar <- colMeans(valid_coef)
  centered  <- sweep(valid_coef, 2, theta_bar, FUN = "-")
  vcov_jack <- (n_valid - 1) / n_valid * crossprod(centered)
  
  attr(vcov_jack, "n_success") <- n_valid
  attr(vcov_jack, "success_rate") <- n_valid / n_jack * 100
  
  dimnames(vcov_jack) <- list(names(coef(object)), names(coef(object)))
  vcov_jack
}

# -- 5. vcov_description -------------------------------------------------------
# Creates the description string about the variance for print methods where they
# use a variance in getting the estimates (print.summary(), print.waldtest()...)
#' @keywords internal
# Internal helper to create consistent variance description for printing
.vcov_description <- function(vcov_mat) {
  
  if (is.null(vcov_mat) || !is.matrix(vcov_mat) || is.null(attr(vcov_mat, "vcov.type"))) {
    return("User Supplied (Unknown)")
  }
  
  vcov_type <- attr(vcov_mat, "vcov.type")
  
  # Base description
  base <- switch(vcov_type,
                 "oim"    = "Original Information Matrix",
                 "opg"    = "Outer Product of Gradients (BHHH)",
                 "robust" = "Robust",
                 "boot"   = {
                   n_succ <- attr(vcov_mat, "n_success")
                   n_rep  <- attr(vcov_mat, "repetitions")
                   rate   <- attr(vcov_mat, "success_rate") %||% 0
                   sprintf("Bootstrap (%d/%d reps. - %.2f%% rate)", n_succ, n_rep, rate)
                 },
                 "jack"   = {
                   n_succ <- attr(vcov_mat, "n_success")
                   rate   <- attr(vcov_mat, "success_rate") %||% 0
                   sprintf("Jackknife (%d valid reps. - %.2f%% rate)", n_succ, rate)
                 },
                 vcov_type   # fallback
  )
  
  # Add cluster information if applicable
  if (!is.null(attr(vcov_mat, "clustered")) && attr(vcov_mat, "clustered")) {
    cluster_var  <- attr(vcov_mat, "cluster.var")
    cluster_name <- attr(vcov_mat, "cluster.varname")
    
    n_clusters <- length(unique(cluster_var))
    
    if (!is.null(cluster_name) && nzchar(cluster_name)) {
      return(paste0(base, " | Clusters: ", n_clusters, " (", cluster_name, ")"))
    } else {
      return(paste0(base, " | Clusters: ", n_clusters))
    }
  } else {
    return(base)
  }
}