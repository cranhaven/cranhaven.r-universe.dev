#' Format a Matrix as a Packed dpoMatrix
#'
#' Converts a matrix to a packed symmetric positive definite matrix
#' (`dpoMatrix`) using the Matrix package.
#'
#' @param x A numeric matrix.
#' @return A packed `dpoMatrix` object.
#' @importFrom Matrix pack
#' @export
format_matr <- function(x) {
  if (!is.matrix(x)) {
    stop("cannot coerce to matrix")
  }
  if (!is.numeric(x)) {
    stop("invalid 'type' (not numeric)")
  }
  x |>
    methods::as("dpoMatrix") |>
    Matrix::pack()
}

#' Normalize Rows of a Matrix
#'
#' Centers and scales each row of a matrix to have zero mean and unit norm.
#'
#' @param si A numeric matrix.
#' @return A matrix with each row centered and scaled.
#' @export
normalization <- function(si) {
  if (is.null(si)) {
    stop("subscript out of bounds")
  }
  if (is.list(si) && !is.matrix(si)) {
    stop("non-numeric argument")
  }
  if (!is.matrix(si)) {
    stop("argument is not a matrix")
  }
  if (!is.numeric(si)) {
    stop("non-numeric argument")
  }

  # Check for empty matrix
  if (length(si) == 0) {
    stop("cannot normalize empty matrix")
  }

  # Check for Inf values
  if (any(is.infinite(si))) {
    stop("cannot normalize matrix with infinite values")
  }

  eps <- 1e-9
  row_means <- rowMeans(si)
  # Center each row by subtracting its mean
  si <- si - row_means
  row_norms <- sqrt(rowSums(si * si))
  # Scale each row by its norm
  si / pmax(row_norms, eps)
}

#' Compute OAS-Shrunk Correlation Matrix from Time Series
#'
#' Normalizes a time series matrix and computes
#' its OAS-shrunk correlation matrix.
#'
#' @param ts A numeric matrix representing time series data.
#' @return A shrunk correlation matrix.
#' @importFrom CovTools CovEst.2010OAS
#' @export
ts2corr <- function(ts) {
  ts <- normalization(ts)
  c <- ts %*% t(ts)
  c |>
    CovTools::CovEst.2010OAS() |>
    _$S
}

#' Harmonize Vector Images Using ComBat
#'
#' Applies the ComBat batch effect correction to vector images
#' in a `CSuperSample` object and reconstructs harmonized samples.
#'
#' @param super_sample A `CSuperSample` object containing samples to harmonize.
#' @return A new `CSuperSample` object with harmonized vector images.
#' @importFrom sva ComBat
#' @export
combat_harmonization <- function(super_sample) {
  # Input validation
  if (!inherits(super_sample, "CSuperSample")) {
    stop("super_sample must be a CSuperSample object")
  }

  # Check for minimum number of groups
  if (length(super_sample$list_of_samples) < 2) {
    stop("CSuperSample must contain at least 2 groups for harmonization")
  }

  # applying ComBat
  # Prepare data for ComBat
  combined_data <- super_sample$list_of_samples |>
    purrr::imap(
      \(sample, idx) {
        sample$compute_tangents()
        sample$compute_vecs()
        data <- sample$vector_images
        batch <- rep(idx, data |> nrow())
        cbind(data, batch)
      }
    ) |>
    purrr::reduce(rbind)
  
  # Extract data matrix and batch vector
  data_matrix <- combined_data[, -ncol(combined_data)]
  batch_vector <- combined_data[, ncol(combined_data)]
  
  # ComBat expects features x samples, so transpose the data
  harmonized_vector_images <- sva::ComBat(dat = t(data_matrix), batch = batch_vector) |>
    t()  # Transpose back to samples x features

  # Reconstructing
  batches <- batch_vector
  vec_imgs <- harmonized_vector_images

  vec_imgs |>
    nrow() |>
    seq_len() |>
    split(batches) |>
    purrr::map(
      \(idx) {
        riemtan::CSample$new(
          vec_imgs = vec_imgs[idx, ],
          centered = FALSE,
          ref_pt = super_sample$matrix_size |> diag() |> format_matr(),
          metric_obj = super_sample$riem_metric
        )
      }
    ) |>
    riemtan::CSuperSample$new()
}


#' Harmonize Tangent Images Across Batches Using Rigid Correction
#'
#' Applies a rigid harmonization procedure to tangent images
#' in a `CSuperSample` object.
#' First, batch means are subtracted from each sample's
#' tangent images (batch correction), then the overall mean is added back
#' (global correction). The harmonized tangent images are used
#' to reconstruct new `CSample` objects, which are collected
#' into a new `CSuperSample`.
#'
#' @param super_sample A `CSuperSample` object containing samples to harmonize.
#' @return A new `CSuperSample` object with harmonized tangent images.
#' @export
#'
#' @details
#' This function performs harmonization in two steps:
#' \enumerate{
#'   \item \strong{Batch Correction:} For each batch, the mean
#' of its tangent images is subtracted from each tangent image in the batch.
#'   \item \strong{Global Correction:} The overall mean (across all batches)
#' of tangent images is added back to each tangent image.
#' }
#' The harmonized tangent images are then used to reconstruct samples
#' using the reference point and metric from the original `CSuperSample`.
rigid_harmonization <- function(super_sample) {
  # Input validation
  if (!inherits(super_sample, "CSuperSample")) {
    stop("super_sample must be a CSuperSample object")
  }

  # Check for minimum number of groups
  if (length(super_sample$list_of_samples) < 2) {
    stop("CSuperSample must contain at least 2 groups for harmonization")
  }

  tangent_collection <- super_sample$list_of_samples |>
    purrr::map(
      \(sample) {
        sample$compute_tangents()
        sample$tangent_images
      }
    )

  batch_means <- tangent_collection |>
    purrr::map(
      \(l) {
        l |>
          purrr::reduce(`+`) |>
          (\(x) {
            result <- as.matrix(x) / length(l)
            Matrix::pack(methods::as(result, "dsyMatrix"))
          })()
      }
    )

  overall_mean <- tangent_collection |>
    purrr::reduce(c) |>
    purrr::reduce(`+`) |>
    (\(x) {
      result <- as.matrix(x) / super_sample$sample_size
      Matrix::pack(methods::as(result, "dsyMatrix"))
    })()

  list(tangent_collection, batch_means) |>
    # Batch correction
    purrr::pmap(
      \(list_of_tangents, batch_mean){
        list_of_tangents |>
          purrr::map(
            \(tgt) {
              result <- tgt - batch_mean
              Matrix::pack(methods::as(result, "dsyMatrix"))
            }
          )
      }
    ) |>
    purrr::map(
      \(list_of_tangents) {
        aux_id <- super_sample$matrix_size |>
          diag() |>
          format_matr()

        # Global correction
        list_of_tangents |>
          purrr::map(
            \(tgt) {
              result <- tgt + overall_mean
              Matrix::pack(methods::as(result, "dsyMatrix"))
            }
          ) |>
          riemtan::CSample$new(
            tan_imgs = _,
            centered = FALSE,
            ref_pt = aux_id,
            metric_obj = super_sample$riem_metric
          )
      }
    ) |>
    riemtan::CSuperSample$new()
}
