#' Normalize a numeric vector
#'
#' Utility used across HealthMarkers to normalize numeric vectors with several
#' common schemes while handling edge cases (constant vectors, all-NA, non-finite
#' values) robustly. NA positions are preserved.
#'
#' Methods:
#' - "none": return input as-is (no coercion; fully backward compatible).
#' - "z": z-score (mean 0, sd 1). Constant vectors return zeros (non-NA entries).
#' - "range": min-max to a target interval (default \eqn{[0,1]}). Constant vectors return
#'   the lower bound (mapped from zeros).
#' - "robust": median/MAD scaling. Constant vectors (MAD=0) return zeros.
#' - "inverse": rank-based inverse normal transform (normal scores).
#'
#' @param x A numeric (or numeric-coercible) vector.
#' @param method One of c("none","z","inverse","range","robust"). Default "none".
#' @param na_rm Logical; remove NAs when estimating statistics (mean, sd, etc.). Default TRUE.
#' @param feature_range Numeric length-2 vector giving the target range for method = "range".
#'   Default c(0, 1).
#' @param invnorm_denominator One of c("n","n+1","blom") controlling the denominator
#'   of the inverse-normal transform:
#'   - "n": p = (r - 0.5) / n  (Rankit; default)
#'   - "n+1": p = (r - 0.5) / (n + 1)
#'   - "blom": p = (r - 3/8) / (n + 1/4)  (Blom, 1958)
#' @param ties Ties method passed to base::rank for method = "inverse".
#'   One of c("average","first","last","random","max","min"). Default "average".
#' @param warn_constant Logical; if TRUE, warn when input is constant and a zero vector
#'   (or lower bound for range) is returned. Default TRUE.
#'
#' @return A numeric vector of the same length as x.
#' @export
#' @importFrom stats sd median qnorm mad
#' @importFrom stats na.omit setNames
#' @importFrom utils head modifyList
#'
#' @examples
#' x <- c(1, 2, 3, NA, 5)
#' normalize_vec(x, "none")
#' normalize_vec(x, "z")
#' normalize_vec(x, "range", feature_range = c(-1, 1))
#' normalize_vec(x, "robust")
#' normalize_vec(x, "inverse")               # Rankit (default)
#' normalize_vec(x, "inverse", invnorm_denominator = "blom")
#'
#' @references
#' \insertRef{beasley2009invnorm}{HealthMarkers}
#' \insertRef{leys2013mad}{HealthMarkers}
#' \insertRef{bland1996sd}{HealthMarkers}
normalize_vec <- function(
  x,
  method = c("none", "z", "inverse", "range", "robust"),
  na_rm = TRUE,
  feature_range = c(0, 1),
  invnorm_denominator = c("n", "n+1", "blom"),
  ties = c("average", "first", "last", "random", "max", "min"),
  warn_constant = TRUE
) {
  method <- match.arg(method)

  # Full backward compatibility: "none" returns input as-is (no coercion/cleanup)
  if (method == "none") {
    return(x)
  }

  invnorm_denominator <- match.arg(invnorm_denominator)
  ties <- match.arg(ties)

  # Coerce to numeric, preserve NA positions; treat non-finite as NA
  if (!is.numeric(x)) {
    old <- x
    suppressWarnings(x <- as.numeric(old))
    if (any(is.na(x) & !is.na(old))) {
      rlang::warn("normalize_vec(): input coerced to numeric; NAs introduced.",
                  class = "healthmarkers_normalize_warn_coercion")
    }
  }
  x[!is.finite(x)] <- NA_real_

  n <- length(x)
  out <- rep(NA_real_, n)

  if (all(is.na(x))) return(out)

  if (method == "z") {
    m <- mean(x, na.rm = na_rm)
    s <- stats::sd(x, na.rm = na_rm)
    if (!is.finite(s) || s == 0) {
      if (isTRUE(warn_constant)) {
        rlang::warn("normalize_vec(z): constant or degenerate input; returning zeros.",
                    class = "healthmarkers_normalize_warn_constant")
      }
      out <- ifelse(is.na(x), NA_real_, 0)
    } else {
      out <- (x - m) / s
    }
    return(out)
  }

  if (method == "range") {
    if (!(is.numeric(feature_range) && length(feature_range) == 2L && all(is.finite(feature_range)))) {
      rlang::abort("normalize_vec(range): `feature_range` must be a finite numeric vector of length 2.",
                   class = "healthmarkers_normalize_error_feature_range")
    }
    lo <- min(feature_range); hi <- max(feature_range)
    xmin <- min(x, na.rm = na_rm); xmax <- max(x, na.rm = na_rm)
    rng <- xmax - xmin
    if (!is.finite(rng) || rng == 0) {
      if (isTRUE(warn_constant)) {
        rlang::warn("normalize_vec(range): constant input; mapping to lower bound.",
                    class = "healthmarkers_normalize_warn_constant")
      }
      base <- ifelse(is.na(x), NA_real_, 0)  # maps to lo
    } else {
      base <- (x - xmin) / rng
    }
    out <- lo + (hi - lo) * base
    return(out)
  }

  if (method == "robust") {
    med <- stats::median(x, na.rm = na_rm)
    md  <- stats::mad(x, na.rm = na_rm)
    if (!is.finite(md) || md == 0) {
      if (isTRUE(warn_constant)) {
        rlang::warn("normalize_vec(robust): MAD is zero; returning zeros.",
                    class = "healthmarkers_normalize_warn_constant")
      }
      out <- ifelse(is.na(x), NA_real_, 0)
    } else {
      out <- (x - med) / md
    }
    return(out)
  }

  if (method == "inverse") {
    ok <- !is.na(x)
    n_ok <- sum(ok)
    if (n_ok == 0L) return(out)  # all NA
    r <- rank(x[ok], na.last = "keep", ties.method = ties)
    # Denominator/offset
    if (invnorm_denominator == "n") {
      p <- (r - 0.5) / n_ok
    } else if (invnorm_denominator == "n+1") {
      p <- (r - 0.5) / (n_ok + 1)
    } else { # "blom"
      p <- (r - 3/8) / (n_ok + 1/4)
    }
    # Clamp probabilities away from 0/1 to avoid +/-Inf
    p <- pmin(pmax(p, 1e-12), 1 - 1e-12)
    out[ok] <- stats::qnorm(p)
    return(out)
  }

  # Fallback (should not reach)
  return(x)
}

#' Normalise marker columns in a data frame
#'
#' A convenience wrapper around [normalize_vec()] that applies a normalisation
#' method to every selected numeric column in a data frame and returns the
#' modified data frame. The most common use-case is to normalise the output of
#' any HealthMarkers function — especially domain functions such as
#' `glycemic_markers()`, `lipid_markers()`, or `renal_markers()` whose internal
#' `normalize` argument currently has no effect.
#'
#' @param data A data frame (or tibble) containing marker columns to normalise.
#' @param cols Character vector of column names to normalise.  If `NULL`
#'   (default), **all numeric columns** in `data` are normalised.
#' @param method One of `c("z","inverse","range","robust")`.  Passed directly to
#'   [normalize_vec()]:
#'   \describe{
#'     \item{`"z"`}{z-score (mean 0, sd 1).}
#'     \item{`"inverse"`}{Rank-based inverse-normal transform (Rankit; default).}
#'     \item{`"range"`}{Min-max scaling to `feature_range` (default `[0, 1]`).}
#'     \item{`"robust"`}{Median/MAD scaling.}
#'   }
#' @param skip_cols Character vector of column names to leave untouched even if
#'   they are numeric (e.g. `c("age", "BMI")`).  Ignored when `cols` is
#'   explicitly supplied.
#' @param ... Additional arguments forwarded to [normalize_vec()] (e.g.
#'   `feature_range`, `invnorm_denominator`, `ties`).
#'
#' @return The input `data` with the selected columns replaced by their
#'   normalised values.  Class (tibble, data.frame, etc.) is preserved.
#'
#' @seealso [normalize_vec()] for single-vector normalisation.
#'
#' @export
#'
#' @examples
#' # Build a tiny data frame with pre-computed marker columns
#' df <- data.frame(
#'   age    = c(45, 52, 61),
#'   HOMA_IR = c(1.2, 3.4, 2.1),
#'   TyG     = c(8.1, 9.0, 8.6),
#'   NLR     = c(2.1, 3.5, 1.8)
#' )
#'
#' marker_cols <- c("HOMA_IR", "TyG", "NLR")
#'
#' # z-score normalise marker columns only
#' hm_normalize(df, cols = marker_cols, method = "z")
#'
#' # Inverse-normal transform, leaving age untouched
#' hm_normalize(df, method = "inverse", skip_cols = "age")
hm_normalize <- function(
  data,
  cols       = NULL,
  method     = c("z", "inverse", "range", "robust"),
  skip_cols  = NULL,
  ...
) {
  method <- match.arg(method)

  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame.", class = "healthmarkers_hm_normalize_error")
  }

  # Determine target columns
  if (!is.null(cols)) {
    bad <- setdiff(cols, names(data))
    if (length(bad)) {
      rlang::abort(
        paste0("Column(s) not found in `data`: ", paste(bad, collapse = ", ")),
        class = "healthmarkers_hm_normalize_error"
      )
    }
    target <- cols
  } else {
    target <- names(data)[vapply(data, is.numeric, logical(1))]
    if (!is.null(skip_cols)) {
      target <- setdiff(target, skip_cols)
    }
  }

  if (length(target) == 0L) {
    rlang::warn("hm_normalize(): no numeric columns to normalise; returning data unchanged.",
                class = "healthmarkers_hm_normalize_warn_no_cols")
    return(data)
  }

  data[target] <- lapply(data[target], normalize_vec, method = method, ...)
  data
}
