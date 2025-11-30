#' @title Update and Re-fit a GKw Regression Model
#'
#' @description
#' Updates and (by default) re-fits a Generalized Kumaraswamy regression model.
#' This method allows modification of the model formula, data, or other arguments
#' without having to completely re-specify the model call. Supports formulas with
#' up to 5 parts (alpha, beta, gamma, delta, lambda) using the \pkg{Formula} package.
#'
#' @param object An object of class \code{"gkwreg"}, typically obtained from
#'   \code{\link{gkwreg}}.
#' @param formula. Changes to the formula. This is a formula where \code{.} refers
#'   to the corresponding part of the old formula. For multi-part formulas
#'   (e.g., \code{y ~ x1 | x2 | x3}), you can update each part separately using
#'   the \code{|} separator.
#' @param data. Optional. A new data frame in which to evaluate the updated model.
#'   If omitted, the original data is used.
#' @param ... Additional arguments to the call, or arguments with changed values.
#'   Use \code{name = NULL} to remove an argument.
#' @param evaluate Logical. If \code{TRUE} (default), the updated model is fitted.
#'   If \code{FALSE}, the updated call is returned without fitting.
#'
#' @details
#' The \code{update} method allows you to modify a fitted model and re-fit it
#' with the changes. The GKw regression model supports formulas with up to 5 parts:
#' \code{y ~ model_alpha | model_beta | model_gamma | model_delta | model_lambda}
#'
#' Each part can be updated independently using \code{.} to refer to the current
#' specification:
#' \itemize{
#'   \item \code{. ~ . + x | . | . | . | .} - Add \code{x} to alpha only
#'   \item \code{. ~ . | . + x | . | . | .} - Add \code{x} to beta only
#'   \item \code{. ~ . | . | . + x | . | .} - Add \code{x} to gamma only
#'   \item \code{. ~ . + x | . + x | . | . | .} - Add \code{x} to alpha and beta
#'   \item \code{. ~ . - x | . | . | . | .} - Remove \code{x} from alpha
#' }
#'
#' Omitting parts at the end is allowed (they default to \code{.}):
#' \itemize{
#'   \item \code{. ~ . + x | .} is equivalent to \code{. ~ . + x | . | . | . | .}
#'   \item \code{. ~ . | . + x} is equivalent to \code{. ~ . | . + x | . | . | .}
#' }
#'
#' @return If \code{evaluate = TRUE}, a new fitted model object of class
#'   \code{"gkwreg"}. If \code{evaluate = FALSE}, an updated call.
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{gkwreg}}, \code{\link[stats]{update}},
#'   \code{\link[Formula]{Formula}}
#'
#' @examples
#' \donttest{
#' # Load example data
#' require(gkwreg)
#'
#' data(GasolineYield)
#'
#' # EXAMPLE 1: Simple formulas (1 part - alpha only)
#'
#' m1_0 <- gkwreg(yield ~ 1, data = GasolineYield, family = "kw")
#' m1_1 <- update(m1_0, . ~ . + temp)
#' m1_2 <- update(m1_1, . ~ . + batch)
#' m1_3 <- update(m1_2, . ~ . - temp)
#'
#' anova(m1_0, m1_1, m1_2)
#' AIC(m1_0, m1_1, m1_2, m1_3)
#' BIC(m1_0, m1_1, m1_2, m1_3)
#'
#' # EXAMPLE 2: Two-part formulas (alpha | beta)
#'
#' # Start with intercept-only for both
#' m2_0 <- gkwreg(yield ~ 1 | 1, data = GasolineYield, family = "kw")
#'
#' # Add temp to alpha
#' m2_1 <- update(m2_0, . ~ . + temp | .)
#'
#' # Add batch to beta
#' m2_2 <- update(m2_1, . ~ . | . + batch)
#'
#' # Add batch to alpha too
#' m2_3 <- update(m2_2, . ~ . + batch | .)
#'
#' anova(m2_0, m2_1, m2_2, m2_3)
#' AIC(m2_0, m2_1, m2_2, m2_3)
#'
#' # EXAMPLE 3: Three-part formulas (alpha | beta | gamma)
#'
#' m3_0 <- gkwreg(yield ~ 1,
#'   data = GasolineYield,
#'   family = "gkw",
#'   control = gkw_control(method = "BFGS", maxit = 2000)
#' )
#'
#' m3_1 <- update(m3_0, . ~ . + temp | . | .)
#' m3_2 <- update(m3_1, . ~ . | . + batch | .)
#' m3_3 <- update(m3_2, . ~ . | . | . + temp)
#'
#' anova(m3_0, m3_1, m3_2, m3_3)
#'
#' # EXAMPLE 4: Practical nested model comparison
#'
#' # Null model
#' fit0 <- gkwreg(yield ~ 1,
#'   data = GasolineYield,
#'   family = "kw",
#'   control = gkw_control(method = "BFGS", maxit = 2000)
#' )
#'
#' # Add main effects to alpha
#' fit1 <- update(fit0, . ~ . + temp)
#' fit2 <- update(fit1, . ~ . + batch)
#'
#' # Model beta parameter
#' fit3 <- update(fit2, . ~ . | temp)
#' fit4 <- update(fit3, . ~ . | . + batch)
#'
#' # Full comparison
#' anova(fit0, fit1, fit2, fit3, fit4)
#' AIC(fit0, fit1, fit2, fit3, fit4)
#' BIC(fit0, fit1, fit2, fit3, fit4)
#'
#' # EXAMPLE 5: Changing other parameters
#'
#' # Change family
#' fit_gkw <- update(fit2, family = "gkw")
#'
#' # Change link function
#' fit_logit <- update(fit2, link = list(alpha = "logit"))
#'
#' # View call without fitting
#' update(fit2, . ~ . | . + temp, evaluate = FALSE)
#' }
#'
#' @importFrom stats getCall update.formula as.formula
#' @importFrom Formula as.Formula
#' @method update gkwreg
#' @export
update.gkwreg <- function(object, formula., ..., data. = NULL, evaluate = TRUE) {
  # Check that object is gkwreg
  if (!inherits(object, "gkwreg")) {
    stop("'object' must be of class 'gkwreg'", call. = FALSE)
  }

  # Get the original call
  call <- getCall(object)

  if (is.null(call)) {
    stop("model object does not contain a valid call component", call. = FALSE)
  }

  # Update formula if provided
  if (!missing(formula.)) {
    # Get the old formula
    old_formula <- formula(object)

    # Check if update formula contains | (multi-part)
    update_str <- paste(deparse(formula.), collapse = " ")
    is_multipart_update <- grepl("\\|", update_str)

    # Check if old formula is multi-part
    old_str <- paste(deparse(old_formula), collapse = " ")
    is_multipart_old <- grepl("\\|", old_str) || inherits(old_formula, "Formula")

    if (is_multipart_update || is_multipart_old) {
      # Handle multi-part formula update
      new_formula <- .update_multipart_formula(old_formula, formula.)
    } else {
      # Simple formula - use standard update
      new_formula <- update.formula(old_formula, formula.)
    }

    call$formula <- new_formula
  }

  # Update data if provided
  if (!is.null(data.)) {
    call$data <- substitute(data.)
  }

  # Update additional arguments
  extras <- list(...)
  if (length(extras) > 0L) {
    existing <- !is.na(match(names(extras), names(call)))

    # Update existing arguments
    for (a in names(extras)[existing]) {
      call[[a]] <- extras[[a]]
    }

    # Add new arguments
    if (any(!existing)) {
      call <- c(as.list(call), extras[!existing])
      call <- as.call(call)
    }
  }

  # Return call or evaluate
  if (evaluate) {
    eval(call, parent.frame())
  } else {
    call
  }
}


#' @title Update Multi-Part Formula (up to 5 parts)
#'
#' @description
#' Internal function to update multi-part formulas for GKw models.
#' Supports formulas with up to 5 parts: alpha | beta | gamma | delta | lambda.
#'
#' @param old Formula object or formula to update.
#' @param new Formula specification for update.
#'
#' @return Updated formula as a Formula object or regular formula.
#'
#' @keywords internal
#' @noRd
.update_multipart_formula <- function(old, new) {
  # Maximum number of RHS parts (alpha, beta, gamma, delta, lambda)
  MAX_PARTS <- 5L

  # Parse old formula into parts
  old_parts <- .parse_formula_parts(old, max_parts = MAX_PARTS)

  # Parse new formula into parts
  new_parts <- .parse_formula_parts(new, max_parts = MAX_PARTS)

  # Update LHS
  if (new_parts$lhs == "." || new_parts$lhs == "") {
    updated_lhs <- old_parts$lhs
  } else {
    updated_lhs <- new_parts$lhs
  }

  # Update each RHS part
  updated_rhs <- character(MAX_PARTS)

  for (i in seq_len(MAX_PARTS)) {
    old_rhs_i <- old_parts$rhs[i]
    new_rhs_i <- new_parts$rhs[i]

    if (new_rhs_i == "." || new_rhs_i == "") {
      # Keep old
      updated_rhs[i] <- old_rhs_i
    } else {
      # Apply update
      if (old_rhs_i == "" || old_rhs_i == "1") {
        # No old formula, just use new (but still apply update rules)
        old_f <- as.formula(paste("~", "1"))
      } else {
        old_f <- as.formula(paste("~", old_rhs_i))
      }

      new_f <- as.formula(paste("~", new_rhs_i))
      updated_f <- update.formula(old_f, new_f)
      updated_rhs[i] <- deparse(updated_f[[2]])
    }
  }

  # Remove trailing empty/intercept-only parts to simplify
  # But keep at least first part
  last_meaningful <- 1L
  for (i in seq_len(MAX_PARTS)) {
    if (updated_rhs[i] != "" && updated_rhs[i] != "1") {
      last_meaningful <- i
    }
  }

  # Construct formula string
  if (last_meaningful == 1L && updated_rhs[1] != "" &&
    !any(updated_rhs[2:MAX_PARTS] != "" & updated_rhs[2:MAX_PARTS] != "1")) {
    # Simple single-part formula
    formula_str <- paste(updated_lhs, "~", updated_rhs[1])
    result <- as.formula(formula_str)
  } else {
    # Multi-part formula
    rhs_parts <- updated_rhs[1:max(last_meaningful, 2)] # At least 2 parts for Formula
    formula_str <- paste(updated_lhs, "~", paste(rhs_parts, collapse = " | "))
    result <- Formula::as.Formula(formula_str)
  }

  return(result)
}


#' @title Parse Formula into Parts
#'
#' @description
#' Internal function to parse a formula into LHS and RHS parts.
#'
#' @param f Formula to parse.
#' @param max_parts Maximum number of RHS parts to extract.
#'
#' @return List with 'lhs' (character) and 'rhs' (character vector).
#'
#' @keywords internal
#' @noRd
.parse_formula_parts <- function(f, max_parts = 5L) {
  # Convert to character
  f_str <- paste(deparse(f), collapse = " ")

  # Remove extra whitespace
  f_str <- gsub("\\s+", " ", f_str)
  f_str <- trimws(f_str)

  # Split by ~
  parts <- strsplit(f_str, "~", fixed = TRUE)[[1]]

  if (length(parts) == 1) {
    # No LHS (one-sided formula)
    lhs <- ""
    rhs_full <- trimws(parts[1])
  } else if (length(parts) == 2) {
    lhs <- trimws(parts[1])
    rhs_full <- trimws(parts[2])
  } else {
    stop("invalid formula structure", call. = FALSE)
  }

  # Split RHS by |
  rhs_parts <- strsplit(rhs_full, "\\|")[[1]]
  rhs_parts <- trimws(rhs_parts)

  # Pad with empty strings if needed
  if (length(rhs_parts) < max_parts) {
    rhs_parts <- c(rhs_parts, rep("", max_parts - length(rhs_parts)))
  }

  # Trim to max_parts
  rhs_parts <- rhs_parts[1:max_parts]

  # Replace empty with "1" (intercept only) where appropriate
  # but keep track of truly empty (not specified)
  for (i in seq_along(rhs_parts)) {
    if (rhs_parts[i] == "") {
      rhs_parts[i] <- "" # Keep empty to distinguish from explicit "1"
    }
  }

  return(list(lhs = lhs, rhs = rhs_parts))
}


#' @title Extract Formula from GKw Regression Model
#'
#' @description
#' Extracts the model formula from a fitted Generalized Kumaraswamy regression
#' model object. Properly handles formulas with up to 5 parts.
#'
#' @param x An object of class \code{"gkwreg"}.
#' @param ... Currently not used.
#'
#' @return The formula used to fit the model. For multi-part formulas, returns
#'   an object of class \code{"Formula"}.
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{update.gkwreg}}
#'
#' @examples
#' \donttest{
#' data(GasolineYield)
#'
#' # Simple formula
#' fit1 <- gkwreg(yield ~ batch + temp, data = GasolineYield, family = "kw")
#' formula(fit1)
#'
#' # Two-part formula
#' fit2 <- gkwreg(yield ~ temp | batch, data = GasolineYield, family = "kw")
#' formula(fit2)
#'
#' # Five-part formula
#' fit3 <- gkwreg(yield ~ temp | batch | temp | 1 | 1,
#'   data = GasolineYield, family = "gkw"
#' )
#' formula(fit3)
#' }
#'
#' @importFrom stats formula
#' @importFrom Formula as.Formula
#' @method formula gkwreg
#' @export
formula.gkwreg <- function(x, ...) {
  if (!inherits(x, "gkwreg")) {
    stop("'x' must be of class 'gkwreg'", call. = FALSE)
  }

  # Return stored formula
  if (!is.null(x$formula)) {
    f <- x$formula

    # Ensure proper class for multi-part formulas
    if (!inherits(f, "Formula")) {
      formula_str <- paste(deparse(f), collapse = " ")
      if (grepl("\\|", formula_str)) {
        f <- Formula::as.Formula(f)
      }
    }

    return(f)
  }

  # Try to extract from call
  call <- getCall(x)
  if (!is.null(call) && !is.null(call$formula)) {
    f <- eval(call$formula)

    if (!inherits(f, "Formula")) {
      formula_str <- paste(deparse(f), collapse = " ")
      if (grepl("\\|", formula_str)) {
        f <- Formula::as.Formula(f)
      }
    }

    return(f)
  }

  stop("formula not found in model object", call. = FALSE)
}


#' @title Extract Terms from GKw Regression Model
#'
#' @description
#' Extracts the terms object from a fitted Generalized Kumaraswamy regression
#' model.
#'
#' @param x An object of class \code{"gkwreg"}.
#' @param ... Currently not used.
#'
#' @return A terms object.
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{formula.gkwreg}}
#'
#' @examples
#' \donttest{
#' data(GasolineYield)
#' fit <- gkwreg(yield ~ batch + temp, data = GasolineYield, family = "kw")
#' terms(fit)
#' }
#'
#' @importFrom stats terms
#' @method terms gkwreg
#' @export
terms.gkwreg <- function(x, ...) {
  if (!inherits(x, "gkwreg")) {
    stop("'x' must be of class 'gkwreg'", call. = FALSE)
  }

  # Try to get from model component
  if (!is.null(x$model)) {
    tt <- attr(x$model, "terms")
    if (!is.null(tt)) {
      return(tt)
    }
  }

  # Otherwise construct from formula
  formula_obj <- formula(x)
  terms(formula_obj)
}


#' @title Extract Model Frame from GKw Regression Model
#'
#' @description
#' Extracts the model frame from a fitted Generalized Kumaraswamy regression
#' model object.
#'
#' @param formula An object of class \code{"gkwreg"}.
#' @param ... Currently not used.
#'
#' @return A data frame containing the variables used in fitting the model.
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{model.matrix.gkwreg}}
#'
#' @examples
#' \donttest{
#' data(GasolineYield)
#' fit <- gkwreg(yield ~ batch + temp, data = GasolineYield, family = "kw")
#' head(model.frame(fit))
#' }
#'
#' @importFrom stats model.frame
#' @method model.frame gkwreg
#' @export
model.frame.gkwreg <- function(formula, ...) {
  if (!inherits(formula, "gkwreg")) {
    stop("'formula' must be of class 'gkwreg'", call. = FALSE)
  }

  if (!is.null(formula$model)) {
    return(formula$model)
  }

  stop("model frame not available; refit model with 'model = TRUE'", call. = FALSE)
}


#' @title Extract Model Matrix from GKw Regression Model
#'
#' @description
#' Extracts the model matrix (design matrix) from a fitted Generalized Kumaraswamy
#' regression model object.
#'
#' @param object An object of class \code{"gkwreg"}.
#' @param ... Currently not used.
#'
#' @return A design matrix.
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{model.frame.gkwreg}}
#'
#' @examples
#' \donttest{
#' data(GasolineYield)
#' fit <- gkwreg(yield ~ batch + temp, data = GasolineYield, family = "kw")
#' head(model.matrix(fit))
#' }
#'
#' @importFrom stats model.matrix model.frame
#' @method model.matrix gkwreg
#' @export
model.matrix.gkwreg <- function(object, ...) {
  if (!inherits(object, "gkwreg")) {
    stop("'object' must be of class 'gkwreg'", call. = FALSE)
  }

  mf <- model.frame(object)
  tt <- terms(object)
  model.matrix(tt, mf)
}


#' @title Extract Response Variable from GKw Regression Model
#'
#' @description
#' Extracts the response variable from a fitted Generalized Kumaraswamy
#' regression model object.
#'
#' @param object An object of class \code{"gkwreg"}.
#' @param ... Currently not used.
#'
#' @return A numeric vector containing the response variable values.
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{fitted.gkwreg}}, \code{\link{residuals.gkwreg}}
#'
#' @examples
#' \donttest{
#' data(GasolineYield)
#' fit <- gkwreg(yield ~ batch + temp, data = GasolineYield, family = "kw")
#' y <- response(fit)
#' head(y)
#' }
#'
#' @export
response <- function(object, ...) {
  UseMethod("response")
}


#' @rdname response
#' @method response gkwreg
#' @export
response.gkwreg <- function(object, ...) {
  if (!inherits(object, "gkwreg")) {
    stop("'object' must be of class 'gkwreg'", call. = FALSE)
  }

  if (!is.null(object$y)) {
    return(object$y)
  }

  mf <- model.frame(object)
  if (!is.null(mf)) {
    tt <- terms(object)
    response_var <- attr(tt, "response")
    if (response_var > 0) {
      return(mf[[response_var]])
    }
  }

  stop("response variable not found in model object", call. = FALSE)
}


#' @title Get Call from GKw Regression Model
#'
#' @description
#' Extracts the call that was used to fit a Generalized Kumaraswamy regression
#' model.
#'
#' @param x An object of class \code{"gkwreg"}.
#' @param ... Currently not used.
#'
#' @return The matched call.
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{update.gkwreg}}
#'
#' @examples
#' \donttest{
#' data(GasolineYield)
#' fit <- gkwreg(yield ~ batch + temp, data = GasolineYield, family = "kw")
#' getCall(fit)
#' }
#'
#' @importFrom stats getCall
#' @method getCall gkwreg
#' @export
getCall.gkwreg <- function(x, ...) {
  if (!inherits(x, "gkwreg")) {
    stop("'x' must be of class 'gkwreg'", call. = FALSE)
  }

  if (!is.null(x$call)) {
    return(x$call)
  }

  stop("call not found in model object", call. = FALSE)
}


#' @title Extract Family from GKw Regression Model
#'
#' @description
#' Extracts the family specification from a fitted Generalized Kumaraswamy
#' regression model object.
#'
#' @param object An object of class \code{"gkwreg"}.
#' @param ... Currently not used.
#'
#' @return A character string indicating the family used in the model.
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{gkwreg}}
#'
#' @examples
#' \donttest{
#' data(GasolineYield)
#' fit <- gkwreg(yield ~ batch + temp, data = GasolineYield, family = "kw")
#' family(fit)
#' }
#'
#' @importFrom stats family
#' @method family gkwreg
#' @export
family.gkwreg <- function(object, ...) {
  if (!inherits(object, "gkwreg")) {
    stop("'object' must be of class 'gkwreg'", call. = FALSE)
  }

  if (!is.null(object$family)) {
    return(object$family)
  }

  stop("family not found in model object", call. = FALSE)
}
