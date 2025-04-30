#' @keywords internal
#' @noRd

# Identifies the indices of the first (minimum) k values in a vector x
k_min_vals_vec_idx <- function(x,k) {
  sort.int(x, index.return = T)$ix[1:k]
}
