#' Prepare and clean IAT data.
#'
#' Select IAT blocks for the \emph{D-score} computation and eventually save demographic data.
#'
#' @param data Dataframe containing IAT data.
#' @param sbj_id Column identifying participants' IDs. This variable can be a \code{character},
#'                  \code{numeric}, or \code{factor}.
#' @param block_id String. Column identifying
#'                  IAT blocks. The \code{block_id} variable should be a
#'                  \code{factor} with each level identifying an IAT block.
#' @param mapA_practice String. Label for the practice blocks of Mapping A (as
#'                       it appears in the \code{block_id} variable).
#' @param mapA_test String. Label for the test blocks of Mapping A (as
#'                       it appears in the \code{block_id} variable).
#' @param mapB_practice String. Label for the practice blocks of Mapping B (as
#'                       it appears in the \code{block_id} variable).
#' @param mapB_test String. Label for the test blocks of Mapping B (as
#'                       it appears in the \code{block_id} variable).
#' @param accuracy_id String. Column identifying the
#'                   IAT accuracy responses. The \code{accuracy_id} variable
#'                   should be a numeric variable identifying the correct
#'                   responses (with 1) and the incorrect responses (with 0).
#' @param latency_id String. Column identifying
#'                    response times (in millisecond). If the IAT had a
#'                    built-in correction, latencies of the incorrect responses
#'                    should be those inflated with the built-in correction.
#' @param trial_id Character. Column identifying the trials. Specify this only
#'                    if you want to delete some specific trials.
#' @param trial_eliminate Character or character vector. Label(s) identifying the trials
#'                    in \code{trial_id} to eliminate.
#' @param demo_id Character. Column identifying demographic blocks. It can be the same as \code{block_id}.
#' @param trial_demo Character or character vector identifying the name of the
#'  blocks in \code{demo_id} containing the demographic information.
#'
#' @return List of dataframe.
#'  \describe{
#'     \item{\code{data_keep}}{Dataframe with class \code{iat_clean}. The
#'     dataframe contains the data of the blocks specified in
#'     \code{mapA_practice}, \code{mapA_test}, \code{mapB_practice},
#'     \code{mapB_test}. If you have specified the trials to eliminate through
#'     \code{trial_eliminate}, \code{data_keep} will contain the already
#'      cleaned dataset. This dataset should be passed to the \code{computeD} function.}
#'     \item{\code{data_eliminate}}{Dataframe containing all the discarded
#'      blocks and trials.}
#'     \item{\code{data_demo}}{Dataframe containing demographic variables.
#'      It will be present only if you specified the \code{demo_id} and
#'      \code{trial_demo} arguments.}
#'     }
#'
#' @export
#'
#' @importFrom stringr str_trim
#' @import stats
#'
#' @examples
#' data("raw_data") # load data
#' iat_cleandata <- clean_iat(raw_data, sbj_id = "Participant",
#'                           block_id = "blockcode",
#'                           mapA_practice = "practice.iat.Milkbad",
#'                           mapA_test = "test.iat.Milkbad",
#'                           mapB_practice = "practice.iat.Milkgood",
#'                           mapB_test = "test.iat.Milkgood",
#'                           latency_id = "latency",
#'                           accuracy_id = "correct",
#'                           trial_id = "trialcode",
#'                           trial_eliminate = c("reminder", "reminder1"),
#'                           demo_id = "blockcode",
#'                           trial_demo = "demo")
#' iat_data <- iat_cleandata[[1]] # select the first element of the list (IAT data)
#' head(iat_data)
#' demo_data <- iat_cleandata[[3]] # select the third element of the list
#'                             # (demographic data)
#' head(demo_data)
clean_iat <- function(data, sbj_id = "participant",
                      block_id = "blockcode",
                      mapA_practice = "practice_MappingA",
                      mapA_test = "test_MappingA",
                      mapB_practice = "practice_MappingB",
                      mapB_test = "test_MappingB",
                      latency_id = "latency",
                      accuracy_id = "correct",
                      trial_id = NULL,
                      trial_eliminate = NULL,
                      demo_id = NULL,
                      trial_demo = NULL){
  # store the original data
  original <- data
  # create the labels
  options_label <- c(mapA_practice, mapA_test, mapB_practice, mapB_test)
  names(options_label) <- c("mapA_practice", "mapA_test", "mapB_practice",
                            "mapB_test")
  # check if the data are coming from spss ----
  if (class(data)[1] == "tbl_df"){
    data <- as.data.frame(data)
  } else if (class(data)[1] == "list") {
    data <- as.data.frame(data)
    data[, block_id] <- stringr::str_trim(data[, block_id])
  } else {
    data <- data
  }
  # save the colum names of the orginal dataset
  original_colnames <- c(colnames(data))
  # labels and columns check --------------------------
  if (is.null(trial_id) & is.null(demo_id)) {
    name_data <- c(sbj_id, block_id, latency_id, accuracy_id)
  } else if (!is.null(trial_id) & is.null(demo_id)) {
    name_data <- c(sbj_id, block_id, latency_id, accuracy_id, trial_id)
  } else if (is.null(trial_id) & !is.null(demo_id)) {
    name_data <- c(sbj_id, block_id, latency_id, accuracy_id, demo_id)
  } else if (!is.null(trial_id) & !is.null(demo_id)) {
    name_data <- c(sbj_id, block_id, latency_id, accuracy_id, trial_id, demo_id)
  }

  # colnames check
  test_colnames <- NULL
  for(i in 1:length(name_data)){
    test_colnames[i] <- any(original_colnames == name_data[i])
  }
  if (any(test_colnames == FALSE)) {stop("Specify valid column names")}

  # blockcode labels check
  check_lab <- list()
  for(i in 1:length(options_label)){
    check_lab[[i]] <- any(data[ , block_id] == options_label[i])
  }
  test_lab <- unlist(check_lab)
  if(any(test_lab == FALSE)) {
    stop("blocks labels not found in the dataset")
  } else if (length(unique(options_label)) < 4) {
    stop("Two equals blocks labels were found")
  } else {
  # create the list with keeping and deleting dataframes
  data <- list(data_keep = data[data[ , block_id] %in% options_label, ],
               data_eliminate = data[!(data[ , block_id]) %in% options_label, ])
    data$data_keep[, block_id] <- as.character(data$data_keep[, block_id])
  }

  # check consistency between columns and labels --------------------------
  if (!is.null(trial_eliminate) & is.null(trial_id)) {
    stop("You must specify the trial_id variable to eliminate trials")
  }

  if (is.null(trial_eliminate) & !is.null(trial_id)) {
    stop("You must specify the trial_elimate variable to eliminate trials")
  }

  if (!is.null(trial_demo) & is.null(demo_id)) {
    stop("You must specify the demo_id variable to select demographic trials")
  }

  if (is.null(trial_demo) & !is.null(demo_id)) {
    stop("You must specify the trial_demo variable to select demographic trials")
  }

  # select only useful columns --------------------------
  if (!is.null(trial_id)) {
    data$data_keep <- data$data_keep[ , c(sbj_id, block_id, latency_id,
                                         accuracy_id, trial_id)]
  } else {
    data$data_keep <- data$data_keep[ , c(sbj_id, block_id, latency_id,
                                         accuracy_id)]
  }
  # save original block code
  data$data_keep$block_original <- data$data_keep[, block_id]
  data$data_keep[, block_id] <- NULL

  # prepare dataset --------------------------
  # create conditions
  data$data_keep$condition <- ifelse(
    data$data_keep[ , "block_original"] == options_label[1] |
      data$data_keep[ , "block_original"] == options_label[2],
    "MappingA", "MappingB")

  # create pooled blocks (practice vs test)
  data$data_keep$block_pool <- ifelse(
    data$data_keep[ , "block_original"] == options_label[1] |
      data$data_keep[ ,"block_original"] == options_label[3],
    "practice", "test")

  # create blocks
  data$data_keep$block <- paste(data$data_keep$block_pool,
                                data$data_keep$condition, sep = "_")

  # name the class of the clean IAT dataframe
  class(data$data_keep) <- append(class(data$data_keep), "iat_clean")

  if (!(is.null(trial_id))) {
    data <- list(data_keep = data$data_keep[!(data$data_keep[ , trial_id])
                                            %in% trial_eliminate, ],
                 data_eliminate = data$data_eliminate)
    data$data_keep <- data$data_keep[, c(1:3, 5:8, 4)]
    colnames(data$data_keep) <- c("participant", "latency", "correct",
                                  "block_original", "condition", "block_pool",
                                  "block", "trial_id")
  } else {
    data <- data
    colnames(data$data_keep) <- c("participant", "latency", "correct",
                                  "block_original", "condition", "block_pool",
                                  "block")
  }

  if (!is.null(demo_id)) {
    data$demo <- original[original[, demo_id] %in% trial_demo, ]
    names(data$demo)[names(data$demo) == sbj_id] <- "participant"
  }
  # results --------------------------
  return(data)
}
