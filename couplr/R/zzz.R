# Package initialization and internal helpers
# This file contains package hooks and internal utility functions.

# ==============================================================================
# Internal ggplot2 Theme
# ==============================================================================

#' Minimal ggplot2 theme without grid lines
#'
#' Wraps \code{ggplot2::theme_minimal()} and removes all panel grid lines
#' for cleaner pkgdown rendering.
#'
#' @param base_size Base font size (default 11)
#' @param ... Additional arguments passed to \code{ggplot2::theme_minimal()}
#' @return A ggplot2 theme object
#' @keywords internal
#' @noRd
.theme_couplr <- function(base_size = 11, ...) {
  ggplot2::theme_minimal(base_size = base_size, ...) +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}

# ==============================================================================
# Internal Cost Matrix Preparation
# ==============================================================================

#' Internal prep step for LAP: NA -> forbidden, rectangular OK, optional maximize flip
#'
#' @param cost Numeric cost matrix
#' @param maximize Logical; if TRUE, flip costs for maximization
#' @return List with cost (row-major numeric), mask (integer 0/1), n, m, cmax
#' @keywords internal
#' @noRd
prepare_cost_matrix <- function(cost, maximize = FALSE) {
  cost <- as.matrix(cost)
  if (!is.numeric(cost)) {
    stop("`cost` must be a numeric matrix, got ", typeof(cost))
  }
  if (any(is.nan(cost))) stop("NaN not allowed in `cost`")
  lap_prepare_cost_matrix(cost, maximize)
}

# ==============================================================================
# Package Hooks
# ==============================================================================

# .onLoad <- function(libname, pkgname) {
#   # Package initialization code here if needed
# }

.onUnload <- function(libpath) {
  library.dynam.unload("couplr", libpath)
}
