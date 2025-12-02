#' Compute the Log Wilks' Lambda Statistic
#'
#' Calculates the log of Wilks' lambda statistic
#' for a given \code{super_sample} object.
#' This function ensures that the within-group
#' and total covariance matrices are computed,
#' then computes the difference of their log determinants.
#'
#' @param super_sample An object of class CSuperSample
#'
#' @return A numeric value representing the log Wilks' lambda statistic.
#'
#' @details
#' Wilks' lambda is a test statistic for the ANOVA test decribed in (to appear)
#' @export
log_wilks_lambda <- function(super_sample) {
  if (!inherits(super_sample, "CSuperSample")) {
    stop("Argument 'super_sample' must be an object of class 'CSuperSample'.")
  }

  # Check for minimum number of groups
  if (length(super_sample$list_of_samples) < 2) {
    stop("CSuperSample must contain at least 2 groups for ANOVA analysis")
  }

  # Compute within-group covariance matrix with error handling
  if (super_sample$Within |> is.null()) {
    tryCatch({
      super_sample$compute_W()
    }, error = function(e) {
      stop("Failed to compute within-group covariance matrix: ", e$message)
    })
  }

  # Compute total covariance matrix with error handling
  if (super_sample$Total |> is.null()) {
    tryCatch({
      super_sample$compute_T()
    }, error = function(e) {
      if (grepl("In index:", e$message)) {
        stop("Failed to compute total covariance matrix due to CSuperSample structure issues. ",
             "This may be caused by incompatible sample data or a bug in the riemtan package. ",
             "Try creating CSample objects first before combining into CSuperSample. ",
             "Original error: ", e$message)
      } else {
        stop("Failed to compute total covariance matrix: ", e$message)
      }
    })
  }

  # Check that matrices are valid
  if (is.null(super_sample$Within)) {
    stop("Within-group covariance matrix is NULL after computation")
  }
  
  if (is.null(super_sample$Total)) {
    stop("Total covariance matrix is NULL after computation")
  }

  # Compute determinants with error handling
  tryCatch({
    det_within <- Matrix::determinant(super_sample$Within)$modulus
    det_total <- Matrix::determinant(super_sample$Total)$modulus
    
    result <- det_within - det_total
    result
  }, error = function(e) {
    stop("Failed to compute log Wilks' lambda determinants: ", e$message)
  })
}

#' Compute Pillai's Trace Statistic
#'
#' Calculates Pillai's trace statistic for a given \code{super_sample} object.
#' This function ensures that the within-group
#' and total covariance matrices are computed,
#' then computes the sum of the eigenvalues of
#' the matrix (Total - Within) %*% solve(Total).
#'
#' @param super_sample An object of class CSuperSample
#'
#' @return A numeric value representing Pillai's trace statistic.
#'
#' @details
#' Pillai's trace is a test statistic for
#' the ANOVA test decribed in (to appear).
#' @export
pillais_trace <- function(super_sample) {
  if (!inherits(super_sample, "CSuperSample")) {
    stop("Argument 'super_sample' must be an object of class 'CSuperSample'.")
  }

  # Check for minimum number of groups
  if (length(super_sample$list_of_samples) < 2) {
    stop("CSuperSample must contain at least 2 groups for ANOVA analysis")
  }

  # Compute within-group covariance matrix with error handling
  if (super_sample$Within |> is.null()) {
    tryCatch({
      super_sample$compute_W()
    }, error = function(e) {
      stop("Failed to compute within-group covariance matrix: ", e$message)
    })
  }

  # Compute total covariance matrix with error handling
  if (super_sample$Total |> is.null()) {
    tryCatch({
      super_sample$compute_T()
    }, error = function(e) {
      if (grepl("In index:", e$message)) {
        stop("Failed to compute total covariance matrix due to CSuperSample structure issues. ",
             "This may be caused by incompatible sample data or a bug in the riemtan package. ",
             "Try creating CSample objects first before combining into CSuperSample. ",
             "Original error: ", e$message)
      } else {
        stop("Failed to compute total covariance matrix: ", e$message)
      }
    })
  }

  # Check that matrices are valid
  if (is.null(super_sample$Within)) {
    stop("Within-group covariance matrix is NULL after computation")
  }
  
  if (is.null(super_sample$Total)) {
    stop("Total covariance matrix is NULL after computation")
  }

  # Compute Pillai's trace with error handling
  tryCatch({
    result <- (
      (super_sample$Total - super_sample$Within) %*%
        solve(super_sample$Total)
    ) |>
      as.matrix() |>
      diag() |>
      sum()

    result
  }, error = function(e) {
    stop("Failed to compute Pillai's trace: ", e$message)
  })
}

#' Permutation Statistic for a Super Sample
#'
#' Randomly shuffles all observations across groups while preserving
#' group sample sizes, creating a new super sample under the null hypothesis
#' of no group differences, and computes a specified statistic on the
#' resulting permuted super sample.
#'
#' @param x An object of class \code{CSuperSample},
#' representing the original super sample.
#' @param stat_fun A function to compute a statistic
#' on the resulting \code{CSuperSample} object.
#'
#' @return The value returned by \code{stat_fun} when applied
#' to the permuted super sample.
#'
#' @details
#' This function performs a permutation test by:
#' 1. Extracting all data points from all groups
#' 2. Randomly shuffling the data
#' 3. Reassigning data to groups with the same sample sizes as the original
#' 4. Computing the test statistic on the permuted data
#'
#' This approach tests the null hypothesis that group labels are exchangeable,
#' which is natural for testing whether sub-populations differ.
#' @export
one_permutation <- function(x, stat_fun) {
  # Extract all connectomes (raw data) from all groups
  # Connectomes are the primary data format and don't require centering
  all_data <- x$list_of_samples |>
    purrr::map(\(s) s$connectomes) |>
    purrr::reduce(c)

  # Get original sample sizes
  sample_sizes <- x$list_of_samples |>
    purrr::map_int(\(s) s$sample_size)

  # Randomly permute all data
  shuffled_data <- sample(all_data)

  # Split shuffled data back into groups (preserving sample sizes)
  start_idx <- 1
  permuted_samples <- sample_sizes |>
    purrr::map(\(n) {
      end_idx <- start_idx + n - 1
      group_data <- shuffled_data[start_idx:end_idx]
      start_idx <<- end_idx + 1

      # Create new CSample with permuted connectomes
      riemtan::CSample$new(conns = group_data, metric_obj = x$riem_metric)
    })

  # Create new CSuperSample and compute statistic
  permuted_samples |>
    riemtan::CSuperSample$new() |>
    stat_fun()
}

#' Compute p-values using permutation test
#'
#' Computes a permutation-based p-value for a given super sample.
#' The statistic used for the permutation test can be specified
#' via the `stat_fun` argument.
#'
#' @param ss An object of class `CSuperSample`.
#' @param stat_fun A function to compute a statistic
#' on the `CSuperSample` object (default: `log_wilks_lambda`).
#' @param nperm The number of permutations to generate
#' for estimating the p-value (default: 1000).
#'
#' @return numeric A permutation-based p-value.
#'
#' @details
#' The function computes the statistic on the observed data
#' and compares it to the distribution of statistics computed
#' on permuted samples. Under the null hypothesis that group labels
#' are exchangeable, this provides an exact test (subject to Monte Carlo error).
#'
#' The permutation test:
#' 1. Computes the test statistic on the observed data
#' 2. Randomly shuffles group assignments while preserving sample sizes
#' 3. Recomputes the test statistic on each permuted dataset
#' 4. Calculates the p-value as the proportion of permuted statistics
#'    that exceed the observed statistic
#'
#' This approach is computationally efficient and does not require
#' parameter estimation or synthetic data generation.
#' @export
riem_anova <- function(ss, stat_fun = log_wilks_lambda, nperm = 1000) {
  if (!inherits(ss, "CSuperSample")) {
    stop("Argument 'ss' must be an object of class 'CSuperSample'.")
  }

  # Check for minimum number of groups
  if (length(ss$list_of_samples) < 2) {
    stop("CSuperSample must contain at least 2 groups for ANOVA analysis")
  }

  # Compute observed test statistic
  stat_val <- stat_fun(ss)

  # Generate permutation distribution
  1:nperm |>
    purrr::map_dbl(
      \(m) one_permutation(ss, stat_fun)
    ) |>
    (\(v) stat_val > v)() |>
    mean()
}
