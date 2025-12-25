validate_ci_bounds <- function(prob) {
  if (prob < 0 || prob > 1) {
    stop("'prob' must be a single numeric value in [0, 1].")
  }
  probs <- c((1 - prob) / 2, 1 - (1 - prob) / 2)
  probs
}

print_format <- function(x, digits = 2, no_digits = c("Bulk_ESS", "Tail_ESS")) {
  x <- as.matrix(x)
  digits <- as.numeric(digits)
  if (length(digits) != 1L) {
    stop("'digits' should be a single numeric value.")
  }
  out <- x
  fmt <- paste0("%.", digits, "f")
  for (i in seq_len(NCOL(x))) {
    if (isTRUE(colnames(x)[i] %in% no_digits)) {
      out[, i] <- sprintf("%.0f", x[, i])
    } else {
      out[, i] <- sprintf(fmt, x[, i])
    }
  }
  print(out, quote = FALSE, right = TRUE)
  invisible(x)
}

reverse_scaling <- function(y_scaled, scaling) {
  y_scaled * scaling$scale + scaling$center
}

ess <- function(x) {
  # nu is returned as a matrix with n_obs rows and N cols. delta and eta have 1 col with N rows.
  if (ncol(x) > 1) {
    ess_values <- c()
    for (i in 1:nrow(x)) {
      ess_values <- append(ess_values, mcmcse::ess(x[i, ]))
    }
    stats::median(ess_values)
  } else {
    mcmcse::ess(x)
  }
}
