`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

is_fct_custom_contrast <- function(x) {
  is.factor(x) && !is.null(attr(x, "contrasts"))
}
