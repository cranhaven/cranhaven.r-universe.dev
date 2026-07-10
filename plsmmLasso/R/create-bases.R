#' Generate bases function matrix
#'
#' The \code{create_bases} function generates a matrix of bases functions for modeling time. 
#' The bases function matrix include Fourier basis functions and polynomial basis functions.
#'
#' @param t A vector representing the timepoints for which basis functions are generated.
#' @param keep (Optional) A vector specifying the indices of the basis functions to retain. 
#' If not provided, all generated basis functions are retained.
#'
#' @return A list with the following components:
#' \item{bases}{A matrix containing the generated bases function.}
#' \item{selected_bases}{A vector containing the indices of the selected basis functions after applying \code{\link{filter_nonzero_bases}}.
#' If the \code{keep} argument is provided, \code{\link{filter_nonzero_bases}} is applied exclusively to the functions specified in \code{keep}.}
#' @details
#' The function constructs two types of basis functions: Fourier basis functions and polynomial basis functions.
#' Fourier basis functions are constructed based on the maximum timepoint (\code{max_t}) and the input timepoints \code{t}. 
#' Polynomial basis functions are constructed with degrees ranging from 0.1 to 2, incrementing by 0.02. 
#'
#' @seealso \code{\link{filter_nonzero_bases}}
#'
#' @examples
#' t <- seq(0, 10, by = 0.5)
#' bases <- create_bases(t)
#' selected_bases <- create_bases(t)
#' selected_bases[[1]]
#' selected_bases[[2]]
#' @export
create_bases <- function(t, keep = NULL) {
  max_t <- max(t)
  n_timepoints <- length(t)
  
  Fourier_basis <- matrix(NA, n_timepoints, 2 * round(max_t))
  for (i in 1:round(max_t)) {
    angle <- 2 * pi * i * t / max_t
    Fourier_basis[, (2 * i - 1):(2 * i)] <- cbind(sin(angle), cos(angle))
  }

  normalized_t <- t / max_t
  poly_degrees <- seq(0.1, 2, 0.02)
  
  poly_basis <- matrix(NA, n_timepoints, length(poly_degrees))
  for (i in 1:length(poly_degrees)) {
    poly_basis[,i] <- normalized_t^poly_degrees[i]
  }

  bases_functions <- cbind(Fourier_basis, poly_basis)

  if (!is.null(keep)) {
    bases_functions <- bases_functions[, keep]
  }

  filtered_bases_functions <- filter_nonzero_bases(bases = bases_functions)
  
  selected_bases <- filtered_bases_functions$selected_bases
  filtered_bases_bases_functions <- filtered_bases_functions$filtered_bases

  return(list(bases = filtered_bases_bases_functions, selected_bases = selected_bases))
}


#' Filter bases functions
#' 
#' The \code{filter_nonzero_bases} function filters out bases functions that are essentially zero. 
#' Bases functions with a sum of absolute values less than a threshold (\eqn{10^{-10}}) are considered as essentially zero and are filtered out.
#' @param bases A matrix containing the bases functions.
#' 
#' @return A list with the following components:
#' \item{filtered_bases}{A matrix containing the filtered bases functions, removing those that are essentially zero.}
#' \item{selected_bases}{A vector containing the indices of the selected bases functions after filtering.}
#'
#' @examples
#' bases <- matrix(c(0, 0.1, 0.2, 0, 0, 0.3, 0, 0, 0), nrow = 3)
#' filtered_bases <- filter_nonzero_bases(bases)
#'
#' @export
filter_nonzero_bases <- function(bases) {
  # Filter bases functions that are essentially 0 if any
  filtered_bases <- c()
  M <- ncol(bases)
  bases_indices <- c()
  for (j in 1:M) {
    fj <- bases[, j]
    if (sum(abs(fj)) > 10^-10) {
      bases_indices <- c(bases_indices, j)
    }
  }
  filtered_bases <- bases[, bases_indices]
  return(list(filtered_bases = filtered_bases, selected_bases = bases_indices))
}