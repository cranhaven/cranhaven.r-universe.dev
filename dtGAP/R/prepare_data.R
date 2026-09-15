#' Prepare Features for Modeling
#'
#' @description
#' Converts target variable for classification tasks and coerces logical/character columns to factors.
#'
#' @param data       Data frame or tibble. Input dataset (train or test).
#' @param target_lab Character. Name of the target column. Required for classification.
#' @param task       Character. Type of task: "classification" or "regression".
#'
#' @return A tibble with processed feature types.
#' @export

prepare_features <- function(data,
                             target_lab = NULL,
                             task = c("classification", "regression")) {
  task <- match.arg(task)

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame or tibble.")
  }

  # Validate target column
  if (task == "classification") {
    if (is.null(target_lab) || !target_lab %in% names(data)) {
      stop("Provide a valid 'target_lab' column name for classification.")
    }
    data[[target_lab]] <- as.factor(data[[target_lab]])
  }

  # Convert logical and character columns to factors
  data[] <- lapply(data, function(col) {
    if (is.logical(col)) {
      factor(ifelse(col, "Yes", "No"))
    } else if (is.character(col)) {
      factor(col)
    } else {
      col
    }
  })

  return(dplyr::as_tibble(data))
}

#' Assigns a train/test indicator to a combined dataset
#'
#' @param data_train A data frame of training observations (or `NULL`).
#' @param data_test A data frame of testing observations (or `NULL`).
#' @param data_all A data frame of all observations (or `NULL`).
#' @param test_size Numeric in (0,1). Proportion for testing (default 0.3).
#' @param seed Integer. Random seed for splitting (default 42).
#' @return A data frame with a `data_type` factor column.
#' @export
add_data_type <- function(data_train = NULL,
                          data_test = NULL,
                          data_all = NULL,
                          test_size = 0.3,
                          seed = 42) {
  if (is.null(data_all)) {
    if (is.null(data_train) || is.null(data_test)) {
      stop("When data_all is NULL, you must supply both data_train and data_test.")
    }

    data_train <- data_train %>%
      mutate(data_type = "train")
    data_test <- data_test %>%
      mutate(data_type = "test")

    data_all <- bind_rows(data_train, data_test) %>%
      mutate(data_type = factor(data_type, levels = c("train", "test")))
  } else {
    # Save and restore RNG state to avoid side effects on the global environment
    if (exists(".Random.seed", envir = globalenv())) {
      old_seed <- get(".Random.seed", envir = globalenv())
      on.exit(assign(".Random.seed", old_seed, envir = globalenv()),
              add = TRUE)
    } else {
      on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
    }
    set.seed(seed)

    n <- nrow(data_all)
    idx <- sample(n, size = floor(test_size * n))
    data_all$data_type <- "train"
    data_all$data_type[idx] <- "test"
    data_all$data_type <- factor(data_all$data_type, levels = c("train", "test"))
  }
  return(data_all)
}
