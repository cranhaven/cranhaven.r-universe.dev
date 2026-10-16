#' Moran's I for trafficCAR residuals
#'
#' Computes Moran's I statistic for model residuals using the model adjacency.
#'
#' @param fit A `traffic_fit` object.
#' @param type Residual type: "raw" or "unstructured".
#' @param nsim Number of permutations for permutation test.
#' @param method "analytic" or "permutation".
#' @importFrom stats residuals
#'
#' @return An object of class `traffic_moran`.
#' @export
moran_residuals <- function(fit,
                            type = c("raw", "structured", "unstructured"),
                            nsim = 199,
                            method = c("analytic", "permutation")) {
  type <- match.arg(type, c("raw", "structured", "unstructured"))
  method <- match.arg(method)

  if (is.null(fit$A)) stop("`fit` must contain adjacency matrix `A`.")

  r <- residuals(fit, type)
  r <- as.numeric(r)

  A <- fit$A
  if (!inherits(A, "Matrix")) {
    stop("`A` must be a sparse Matrix.")
  }

  n <- length(r)
  if (nrow(A) != n || ncol(A) != n) {
    stop("Dimensions of `A` must match residual length.")
  }

  # Binary weights
  W <- A
  W@x <- rep(1, length(W@x))

  r_cent <- r - mean(r)
  denom <- sum(r_cent^2)
  if (denom == 0) {
    out <- list(
      I = NA_real_,
      p_value = NA_real_,
      type = type,
      method = method,
      n = length(r)
    )
    class(out) <- "traffic_moran"
    return(out)
  }


  num <- as.numeric(crossprod(r_cent, W %*% r_cent))
  S0 <- sum(W)

  I_obs <- (n / S0) * (num / denom)

  if (method == "analytic") {
    E_I <- -1 / (n - 1)

    out <- list(
      I = I_obs,
      expected = E_I,
      type = type,
      method = "analytic",
      n = n
    )
    class(out) <- "traffic_moran"
    return(out)
  }

  # permutation test
  I_perm <- numeric(nsim)
  for (b in seq_len(nsim)) {
    rp <- sample(r_cent, replace = FALSE)
    I_perm[b] <- (n / S0) *
      as.numeric(crossprod(rp, W %*% rp)) / sum(rp^2)
  }

  p_val <- (1 + sum(abs(I_perm) >= abs(I_obs))) / (nsim + 1)

  out <- list(
    I = I_obs,
    p_value = p_val,
    permuted = I_perm,
    type = type,
    method = "permutation",
    n = n,
    nsim = nsim
  )
  class(out) <- "traffic_moran"
  out
}




#' @method print traffic_moran
#' @export
print.traffic_moran <- function(x, ...) {
  cat("Moran's I\n")
  cat("Type:", x$type, "\n")
  cat("I =", formatC(x$I, digits = 4), "\n")

  if (x$method == "analytic") {
    cat("E[I] =", formatC(x$expected, digits = 4), "\n")
  } else {
    cat("p-value =", formatC(x$p_value, digits = 4), "\n")
    cat("Permutations:", x$nsim, "\n")
  }

  invisible(x)
}


