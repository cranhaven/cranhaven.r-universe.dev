.cimehr_numeric_warning <- function(message, code = 'numerical_precision') {
  quiet <- exists('.cimehr_identification_signal', inherits = TRUE) &&
    isTRUE(.cimehr_identification_signal$quiet)
  if (!quiet) warning(structure(list(message = message, call = NULL, code = code),
    class = c('cimehr_numerical_warning', 'warning', 'condition')))
  invisible(NULL)
}

.cimehr_checked_se <- function(V) {
  v <- if (is.matrix(V)) diag(V) else V
  out <- rep(NA_real_, length(v)); names(out) <- names(v)
  good <- is.finite(v) & v >= 0
  out[good] <- sqrt(v[good])
  bad <- is.nan(v) | (!is.na(v) & (!is.finite(v) | v < 0))
  if (any(bad)) .cimehr_numeric_warning(paste0(
    'A reported variance is negative or nonfinite. Its SE and CI are unavailable; ',
    'the variance has not been truncated to zero. Other computed results are retained.'),
    'invalid_reported_variance')
  out
}

.cimehr_stable_matvec <- function(M, x) {
  M <- as.matrix(M)
  if (ncol(M) != length(x)) stop('Numerical matrix/vector dimensions do not match.', call. = FALSE)
  out <- cimehr_stable_matvec_cpp(M, as.double(x))
  names(out) <- rownames(M)
  out
}

.cimehr_stable_right_product <- function(M, B) {
  M <- as.matrix(M); B <- as.matrix(B)
  if (ncol(M) != nrow(B)) stop('Numerical matrix-product dimensions do not match.', call. = FALSE)
  out <- cimehr_stable_right_product_cpp(M, B)
  dimnames(out) <- list(rownames(M), colnames(B))
  out
}

.cimehr_covariance_factor <- function(influence) {
  influence <- as.matrix(influence)
  if (any(!is.finite(influence)))
    stop('A covariance factor requires finite participant influences.', call. = FALSE)
  p <- ncol(influence)
  if (!p || !nrow(influence)) return(matrix(0, 0L, p,
    dimnames = list(NULL, colnames(influence))))
  qr_if <- qr(influence, LAPACK = TRUE)
  factor <- qr.R(qr_if)[, order(qr_if$pivot), drop = FALSE]
  dimnames(factor) <- list(NULL, colnames(influence))
  attr(factor, 'scope') <- 'untruncated QR factor of the supplied participant influences'
  factor
}

.cimehr_factor_variance <- function(D, factor) {
  D <- as.matrix(D); factor <- as.matrix(factor)
  if (ncol(D) != ncol(factor) || any(!is.finite(D)) || any(!is.finite(factor)))
    stop('A finite aligned influence factor is required for contrast uncertainty.', call. = FALSE)
  if (!is.null(colnames(D)) && !is.null(colnames(factor))) {
    if (!setequal(colnames(D), colnames(factor)))
      stop('Contrast columns do not match the covariance factor.', call. = FALSE)
    factor <- factor[, colnames(D), drop = FALSE]
  }
  projected <- .cimehr_stable_right_product(D, t(factor))
  rowSums(projected * projected)
}
