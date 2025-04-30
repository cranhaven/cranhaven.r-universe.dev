#' @keywords internal
#' @noRd

# Identifies the values of the first (minimum) k values in a vector x
k_min_vals_vec <- function(x,k) {
  idx <- sort.int(x, index.return = T)$ix[1:k]
  x[idx]
}
