#' Normalizes a time course using a given mapping from integration sites to
#' clones.
#'
#' Each integration site is replaced by its clone. The size of the clone is
#' adjusted to be the mean size of the integration sites within it. For
#' integration sites that are not mentioned in `rec`, we adjust by the average
#' number of integration sites per clone.
#'
#' @param readouts The integration site readouts to adjust.
#' @param rec A matrix with columns "IS" and "Clone" that assigns each
#'            integration site to a clone.
#' @param rec_first Whether the clones should be put in the first rows of the
#'                  resulting time course.
#' @param reduce_clones Whether to represent the integration sites by their
#'                      respective clone.
#'
#' @return The adjusted time course.
#' @export
normalize_timecourse <- function(readouts,
                                 rec,
                                 rec_first = FALSE,
                                 reduce_clones = TRUE) {
  norm_bcr <- readouts
  clones <- unique(rec[, "Clone"])
  barcodes <- rec[, "IS"]

  if (reduce_clones) {
    norm.clones <- matrix(
      NA,
      nrow = length(clones),
      ncol = ncol(norm_bcr),
      dimnames = list(clones, colnames(norm_bcr)))

    for (c in clones) {
      rel_is <- rec[rec[, "Clone"] == c, "IS"]
      rel_clone <- norm_bcr[rel_is, , drop = FALSE]
      sum.clone <- colSums(rel_clone, na.rm = TRUE)
      mean.clone <- sum.clone / nrow(rel_clone)
      norm.clones[c, ] <- mean.clone
    }
  } else {
    norm.clones <- matrix(
      NA,
      nrow = length(barcodes),
      ncol = ncol(norm_bcr),
      dimnames = list(barcodes, colnames(norm_bcr)))

    for (c in clones) {
      rel_is <- rec[rec[, "Clone"] == c, "IS"]
      rel_clone <- norm_bcr[rel_is, , drop = FALSE]
      # total clone should have the mean size
      mean.clone <- colMeans(rel_clone, na.rm = TRUE)
      norm.clones[rel_is, ] <- t(
        (t(rel_clone) / colSums(rel_clone, na.rm = TRUE)) * mean.clone)
    }
  }

  mean_is_per_clone <- nrow(rec) / length(clones)

  rest <- rownames(readouts)[!rownames(readouts) %in% rec[, "IS"]]
  norm_bcr <- norm_bcr[rest, , drop = FALSE]
  norm_bcr <- norm_bcr / mean_is_per_clone
  norm_bcr <- rbind(norm.clones, norm_bcr)

  if (!rec_first && !reduce_clones) {
    norm_bcr <- norm_bcr[rownames(readouts), ]
  }

  return(norm_bcr)
}
