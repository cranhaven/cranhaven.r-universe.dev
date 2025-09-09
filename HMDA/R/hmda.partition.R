#' @title Partition Data for HMDA Analysis
#' @description Partition a data frame into training, testing, and
#'   optionally validation sets, and upload these sets to a local
#'   H2O server. If an outcome column \code{y} is provided and is a
#'   factor or character, stratified splitting is used; otherwise, a
#'   random split is performed. The proportions must sum to 1.
#'
#' @param df         A data frame to partition.
#' @param y          A string with the name of the outcome column.
#'                 Must match a column in \code{df}.
#' @param train      A numeric value for the proportion of the
#'                 training set.
#' @param test       A numeric value for the proportion of the
#'                 testing set.
#' @param validation Optional numeric value for the proportion of
#'                 the validation set. Default is \code{NULL}. If
#'                 specified, train + test + validation must equal 1.
#' @param seed       A numeric seed for reproducibility.
#'                 Default is 2025.
#'
#' @return A named list containing the partitioned data frames
#'         and their corresponding H2O frames:
#'         \describe{
#'           \item{hmda.train}{Training set (data frame).}
#'           \item{hmda.test}{Testing set (data frame).}
#'           \item{hmda.validation}{Validation set (data frame), if any.}
#'           \item{hmda.train.hex}{Training set as an H2O frame.}
#'           \item{hmda.test.hex}{Testing set as an H2O frame.}
#'           \item{hmda.validation.hex}{Validation set as an H2O frame, if
#'           applicable.}
#'         }
#'
#' @details This function uses the \code{splitTools} package to perform
#'   the partition. When \code{y} is provided and is a factor or character,
#'   a stratified split is performed to preserve class proportions. Otherwise,
#'   a basic random split is used. The partitions are then converted to H2O
#'   frames using \code{h2o::as.h2o()}.
#'
#' @examples
#' \dontrun{
#'   # Example: Random split (80% train, 20% test) using iris data
#'   data(iris)
#'   splits <- hmda.partition(
#'               df = iris,
#'               train = 0.8,
#'               test = 0.2,
#'               seed = 2025
#'             )
#'   train_data <- splits$hmda.train
#'   test_data  <- splits$hmda.test
#'
#'   # Example: Stratified split (70% train, 15% test, 15% validation)
#'   # using iris data, stratified by Species
#'   splits_strat <- hmda.partition(
#'                      df = iris,
#'                      y = "Species",
#'                      train = 0.7,
#'                      test = 0.15,
#'                      validation = 0.15,
#'                      seed = 2025
#'                    )
#'   train_strat <- splits_strat$hmda.train
#'   test_strat  <- splits_strat$hmda.test
#'   valid_strat <- splits_strat$hmda.validation
#' }
#'
#' @export
#' @author E. F. Haghish

hmda.partition <- function(df,
                           y = NULL,
                           train = 0.80,
                           test = 0.20,
                           validation = NULL,
                           # global = FALSE, #not needed atm
                           seed = 2025) {

  if (!requireNamespace("splitTools", quietly = TRUE)) {
    stop("Package 'splitTools' is required. Please install it.")
  }

  # Basic checks on df
  if (missing(df) || !is.data.frame(df)) {
    stop("'df' must be a data frame.")
  }

  # If y is provided, check if it exists in df
  if (!is.null(y) && !y %in% names(df)) {
    stop("The provided 'y' does not match any column in 'df'.")
  }

  # Validate that train/test/(validation) sum to 1
  if (is.null(validation)) {
    if (abs(train + test - 1) > .Machine$double.eps^0.5) {
      stop("train + test must sum to 1 if 'validation' is NULL.")
    }
  } else {
    if (abs(train + test + validation - 1) > .Machine$double.eps^0.5) {
      stop("train + test + validation must sum to 1 if 'validation' is specified.")
    }
  }

  set.seed(seed)

  # Decide how to split: stratified (for categorical) or random
  if (!is.null(y) && (is.factor(df[[y]]) || is.character(df[[y]]))) {
    # Stratified partition
    split_args <- list(
      y = df[[y]],
      type = "stratified"
    )
  } else {
    # Basic random partition
    split_args <- list(
      y = seq_len(nrow(df)),
      type = "basic"
    )
  }

  # Create proportions vector
  if (is.null(validation)) {
    split_args$p <- c(train, test)
  } else {
    split_args$p <- c(train, test, validation)
  }

  # Perform partition using splitTools
  splits <- do.call(splitTools::partition, split_args)
  hmda.train <- df[splits[[1]], , drop = FALSE]
  hmda.test  <- df[splits[[2]], , drop = FALSE]
  hmda.train.hex <- h2o::as.h2o(hmda.train, destination_frame = "hmda.train.hex")
  hmda.test.hex <- h2o::as.h2o(hmda.train, destination_frame = "hmda.test.hex")

  # Create the outputs
  if (!is.null(validation)) {
    hmda.validation <- df[splits[[3]], , drop = FALSE]
    hmda.validation.hex <- h2o::as.h2o(df[splits[[3]], , drop = FALSE], destination_frame = "hmda.validation.hex")
    out <- list(
      hmda.train = hmda.train,
      hmda.test = hmda.test,
      hmda.validation = hmda.validation,
      hmda.train.hex = hmda.train.hex,
      hmda.test.hex = hmda.test.hex,
      hmda.validation.hex = hmda.validation.hex
    )
  }
  else {
    out <- list(
      hmda.train = hmda.train,
      hmda.test = hmda.test,
      hmda.train.hex = hmda.train.hex,
      hmda.test.hex = hmda.test.hex
    )
  }

  # # If global = TRUE, assign data frames and h2o frames to global env
  # # ============================================================
  # if (isTRUE(global)) {
  #   for (nm in names(out)) {
  #     # Assign data frame globally
  #     message(paste0("uploading ",nm, ".hex data to the local h2o server on your machine"))
  #     assign(nm, out[[nm]], envir = .GlobalEnv)
  #     # Upload to h2o with the same name
  #     assign(
  #       paste0(nm, ".hex"),
  #       h2o::as.h2o(out[[nm]], destination_frame = paste0(nm,".hex")),
  #       envir = .GlobalEnv
  #     )
  #   }
  # }

  return(out)
}
