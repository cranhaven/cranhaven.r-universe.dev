validate_numeric <- function(x, empty = FALSE, missing = FALSE, nan = FALSE,
                             finite = TRUE, pos_int = FALSE, positive = FALSE,
                             len = NULL, min_len = NULL, max_len = NULL,
                             min_val = NULL, max_val = NULL,
                             min_val_strict = NULL, max_val_strict = NULL,
                             func = stop, message = NULL){
  var_name <- deparse1(substitute(x))
  if(!is.numeric(x)) func(message, var_name, " must be numeric; supplied value has class \"", class(x), "\"")
  if(isFALSE(empty) && length(x) == 0) func(message, var_name, " cannot be empty")
  if(isFALSE(missing) && any(is.na(x))) func(message, var_name, " cannot have NA values")
  if(isFALSE(nan) && any(is.nan(x))) func(message, var_name, " cannot have NaN values")
  if(isTRUE(finite) && any(!is.finite(x))) func(message, var_name, " must be finite")
  # Some values are converted to integer via as.integer(), and the result must
  # be positive. Require finite values, so prog = Inf does not cause a problem.
  if(isTRUE(pos_int) && all(is.finite(x)) && any(as.integer(x) <= 0))
    func(message, "as.integer(", var_name, ") must be positive")
  if(isTRUE(positive) && any(x <= 0)) func(message, var_name, " must be positive")
  if(!is.null(len) && length(x) != len) func(message, var_name, " must have length ", len)
  if(!is.null(min_len) && length(x) < min_len)
    func(message, var_name, " must have length at least ", min_len)
  if(!is.null(max_len) && length(x) > max_len)
    func(message, var_name, " must have length at most ", max_len)
  if(!is.null(min_val) && any(x < min_val))
    func(message, var_name, " must have value(s) greater than or equal to ", min_val)
  if(!is.null(max_val) && any(x > max_val))
    func(message, var_name, " must have value(s) less than or equal to ", max_val)
  if(!is.null(min_val_strict) && any(x <= min_val_strict))
    func(message, var_name, " must have value(s) greater than ", min_val_strict)
  if(!is.null(max_val_strict) && any(x >= max_val_strict))
    func(message, var_name, " must have value(s) less than ", max_val_strict)
}
