#' Create matrix of distances between strata
#'
#' Create a distance matrix between strata levels created from the interactions
#' of factors. Used as input to \code{\link{generate_qs}()}.
#'
#' @param ... any number of matrices that contain the distances between levels of
#' a single stratifying factor. These should have both column and row names
#' which correspond to the levels of the stratifying factor.
#' @return Matrix containing the distances between all levels of the factor of all
#' interactions between the inputted factors. The row and column names correspond
#' to the levels of the strata, formed by combining the level name of each stratifying
#' factor separated with `:`.
#' @export
#'
#' @examples
#'
#' data('nh0506')
#'
#' age_cat <- cut(nh0506$age,
#'                breaks = c(19, 39, 50, 85),
#'                labels = c('< 40 years', '40 - 50 years', '> 50 years'))
#'
#' age_dist <- matrix(data = c(0, 1, 2, 1, 0, 1, 2, 1, 0),
#'                    nrow = 3,
#'                    byrow = TRUE,
#'                    dimnames = list(levels(age_cat), levels(age_cat)))
#'
#' sex_dist <- matrix(data = c(0, 1, 1, 0),
#'                    nrow = 2,
#'                    dimnames = list(levels(nh0506$sex), levels(nh0506$sex)))
#'
#' strata_dist <- create_dist_matrix(age_dist, sex_dist)


create_dist_matrix <- function(...) {
  dists <- list(...)
  n_ind <- length(dists)
  n_total <- prod(sapply(dists, function(x) nrow(x)))

  total_dist <- matrix(0, nrow = n_total, ncol = n_total)

  n_rep <- 1
  for (dist in dists) {
    for (i in 1:nrow(dist)) {
      for (j in 1:ncol(dist)) {
        rows <- rep(FALSE, n_total / n_rep)
        cols <- rep(FALSE, n_total / n_rep)
        rows[seq(i, n_total / n_rep, by = nrow(dist))] <- TRUE
        cols[seq(j, n_total / n_rep, by = ncol(dist))] <- TRUE
        rows <- rep(rows, each = n_rep)
        cols <- rep(cols, each = n_rep)
        total_dist[rows, cols] <- total_dist[rows, cols] + dist[i,j]
      }
    }
    n_rep <- n_rep * nrow(dist)
  }

  level_names <- rownames(dists[[1]])
  for (dist in dists[2:n_ind]) {
    level_names <- c(outer(level_names, rownames(dist), FUN = paste, sep=":"))
  }
  dimnames(total_dist) <- list(level_names, level_names)
  return(total_dist)
}

