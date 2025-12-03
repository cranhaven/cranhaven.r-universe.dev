#' Making a new gimap dataset
#' @description This function allows people to have their data ready to be
#'  processed by gimap
#' @param counts a matrix of data that contains the counts where rows are each
#' paired_guide target and columns are each sample
#' @param pg_ids the pgRNA IDs: metadata associated with the pgRNA constructs
#' that correspond to the rows of the counts data
#' @param sample_metadata metadata associated with the samples of the dataset
#' that correspond to the columns of the counts data.
#' Should include a column that has replicate information as well as a column
#' that contains timepoint information respectively (this will be used for log
#'  fold calculations). These columns should be factors.
#' @return A special gimap_dataset to be used with the other functions in this
#' package.
#' @export
#' @examples
#'
#' counts <- get_example_data("count", data_dir = tempdir()) %>%
#'   dplyr::select(c(
#'     "Day00_RepA", "Day05_RepA", "Day22_RepA", "Day22_RepB",
#'     "Day22_RepC"
#'   )) %>%
#'   as.matrix()
#'
#' pg_ids <- get_example_data("count", data_dir = tempdir()) %>%
#'   dplyr::select("id")
#'
#' sample_metadata <- data.frame(
#'   col_names = c("Day00_RepA", "Day05_RepA", "Day22_RepA", "Day22_RepB", "Day22_RepC"),
#'   day = as.numeric(c("0", "5", "22", "22", "22")),
#'   rep = as.factor(c("RepA", "RepA", "RepA", "RepB", "RepC"))
#' )
#'
#' gimap_dataset <- setup_data(
#'   counts = counts,
#'   pg_ids = pg_ids,
#'   sample_metadata = sample_metadata
#' )
#'
setup_data <- function(counts = NULL,
                       pg_ids = NULL,
                       sample_metadata = NULL) {
  new_data <- gimap_object()
  class(new_data) <- c("list", "gimap_dataset")
  if (is.null(counts)) stop("counts cannot be NULL")
  if (!is.matrix(counts)) stop("counts can only be in the form of a matrix")
  # If they don't give sample metadata, then we will make up a row id
  if (is.null(sample_metadata)) {
    stop("sample metadata will be required in later steps. Please provide it")
  }
  if (!is.data.frame(sample_metadata)) {
    stop("metadata can only be in the form of a data.frame")
  }
  if (nrow(sample_metadata) != ncol(counts)) {
    stop(
      "the number of rows in the sample metadata",
      "is not equal to the number of columns in the counts"
    )
  }
  if (!(length(unique(sample_metadata[, 1])) ==
    length(sample_metadata[, 1]))) {
    stop("The first column in sample metadata must be a unique ID")
  }
  new_data$metadata$sample_metadata <- sample_metadata
  # If they don't give paired guide ids, then we will make up a row id
  if (is.null(pg_ids)) pg_ids <- data.frame(id = 1:nrow(counts))
  if (!is.data.frame(pg_ids)) {
    stop("pgRNA IDs can only be in the form of a data.frame")
  }
  if (nrow(pg_ids) != nrow(counts)) {
    stop(
      "the number of rows in the pg_info is not equal",
      "to the number of rows in the counts"
    )
  }
  # we need the first column to be a unique id
  if (!(length(unique(pg_ids[, 1])) == length(pg_ids[, 1]))) {
    stop("The paired guide IDs must be a unique ID")
  }
  new_data$metadata$pg_ids <- pg_ids
  # Store the raw counts
  new_data$raw_counts <- counts
  # Calculate the counts per sample
  new_data$counts_per_sample <- apply(counts, 2, sum)
  # Transform the data
  new_data$transformed_data$cpm <-
    apply(counts, 2, function(x) (x / sum(x)) * 1e6)
  new_data$transformed_data$log2_cpm <-
    log2(new_data$transformed_data$cpm + 1)
  return(new_data)
}

#' Make an empty gimap dataset object
#' @description This function makes an empty gimap data object
#' @export
#' @return an empty `gimap_dataset` which is a named list which will
#' be filled by various `gimap` functions.
gimap_object <- function() {
  gimap_obj <- list(
    raw_counts = NULL,
    counts_per_sample = NULL,
    transformed_data = list(
      cpm = NULL,
      log2_cpm = NULL
    ),
    metadata = list(
      pg_ids = NULL,
      sample_metadata = NULL
    ),
    filtered_data = list(
      filter_step_run = FALSE,
      metadata_pg_ids = NULL,
      transformed_log2_cpm = NULL,
      removed_pg_ids = NULL,
      all_reps_zerocount_ids = NULL
    ),
    comparisons = NULL,
    annotation = NULL,
    normalized_log_fc = NULL,
    gi_scores = NULL,
    linear_model = NULL
  )

  return(gimap_obj)
}
