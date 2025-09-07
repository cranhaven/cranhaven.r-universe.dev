#' Prepare and clean SC-IAT data
#'
#' Select the SC-IAT blocks, for either one or two SC-IATs. Eventually
#' save demographic data.
#'
#' @param data Dataframe containing SC-IAT data.
#' @param sbj_id Column identifying participants' IDs. This variable can be a \code{character},
#'                  \code{numeric}, or \code{factor}.
#' @param block_id String. Column identifying
#'                  SC-IAT blocks. The \code{block_id} variable should be a
#'                  \code{factor} with each level identifying a SC-IAT block.
#' @param accuracy_id String. Column identifying the
#'                   IAT accuracy responses. The \code{accuracy_id} variable
#'                   should be a numeric variable identifying the correct
#'                   responses (with 1) and the incorrect responses (with 0).
#' @param latency_id String. Column identifying
#'                    response times (in millisecond).
#' @param block_sciat_1 Character or character vector. Labels identifying the first SC-IAT
#' blocks as they are named in the \code{block_id}.
#' @param block_sciat_2 Character or character vector. Labels identifying the second (if present) SC-IAT
#' blocks as they are named in the \code{block_id}.
#' @param trial_id Character. Column identifying the trials. Specify this only
#'      if you want to delete some specific trials. If a response window was used
#'      for the SC-IAT administration the label of the non-response must be included in
#'      this variable.
#' @param trial_eliminate Character or character vector. Labels of the trials to
#'       eliminate in the \code{trial_id} to eliminate (NOTE: don't use this command
#'       to delete the responses exceeding the response time window).
#' @param demo_id Character. Character. Column identifying demographic blocks.
#'        It can be the same as \code{block_id}.
#' @param trial_demo Character or character vector identifying the name of the
#'  blocks in \code{demo_id} containing the demographic information.
#'
#' @return List of dataframe.
#'
#'  \describe{
#'     \item{\code{sciat1}}{Data frame with class \code{sciat_clean} containing
#'     the data of the first SC-IAT as specified  \code{block_sciat_1}.
#'     If any labels was specified in \code{trial_eliminate}, \code{data_keep}
#'     will contain the already cleaned dataset.}
#'     \item{\code{sciat2}}{Data frame with class \code{sciat_clean} containing
#'     the data of the second (if any) SC-IAT as specified through \code{block_sciat_2}.
#'     If any labels was specified in \code{trial_eliminate}, \code{data_keep}
#'     will contain the already cleaned dataset.}
#'     \item{\code{data_demo}}{Data frame. Present only when
#'     \code{variable_demo} and \code{trial_demo} arguments are specified.}
#'     }
#'
#' @export
#'
#' @examples
#' data("raw_data")
#' sciat_data <- clean_sciat(raw_data, sbj_id = "Participant",
#'                          block_id = "blockcode",
#'                          latency_id = "latency",
#'                          accuracy_id = "correct",
#'                          block_sciat_1 = c("test.sc_dark.Darkbad",
#'                                            "test.sc_dark.Darkgood"),
#'                          block_sciat_2 = c("test.sc_milk.Milkbad",
#'                                            "test.sc_milk.Milkgood"),
#'                          trial_id  = "trialcode",
#'                          trial_eliminate = c("reminder",
#'                                              "reminder1"))
#' sciat1 <- sciat_data[[1]]
#' sciat2 <- sciat_data[[2]]
clean_sciat <- function(data, sbj_id = "participant",
                        block_id = "blockcode",
                        accuracy_id = "correct",
                        latency_id = "latency",
                        block_sciat_1 = NULL,
                        block_sciat_2 = NULL,
                        trial_id = NULL,
                        trial_eliminate = NULL,
                        demo_id = NULL,
                        trial_demo = NULL){
  original <- data
  # check if the data are coming from spss ----
  if (class(data)[1] == "tbl_df"){
    data <- as.data.frame(data)
  } else if (class(data)[1] == "list") {
    data <- as.data.frame(data)
    data[, block_id] <- stringr::str_trim(data[, block_id])
  } else {
    data <- data
  }
  # check consistency between columns and labels --------------------------
  if (!is.null(trial_eliminate) & is.null(trial_id)){
    stop("You must specify the trial_eliminate variable to eliminate trials")
  }

  if (is.null(trial_eliminate) & !is.null(trial_id)){
    stop("You must specify the trial_id variable to eliminate trials")
  }

  if (!is.null(trial_demo) & is.null(demo_id)){
    stop("You must specify the trial_demo variable to select demographic trials")
  }

  if (is.null(trial_demo) & !is.null(demo_id)){
    stop("You must specify the demo_id variable to select demographic trials")
  }

  # save the colum names of the orginal dataset
  original_colnames <- c(colnames(data))
  # labels and column check --------------------------
  if (is.null(trial_id) & is.null(demo_id)) {
    name_data <- c(sbj_id, block_id, latency_id, accuracy_id)
  } else if (!is.null(trial_id) & is.null(demo_id)) {
    name_data <- c(sbj_id, block_id, latency_id, accuracy_id, trial_id)
  } else if (is.null(trial_id) & !is.null(demo_id)){
    name_data <- c(sbj_id, block_id, latency_id, accuracy_id, demo_id)
  } else if (!is.null(trial_id) & !is.null(demo_id)){
    name_data <- c(sbj_id, block_id, latency_id, accuracy_id, trial_id, demo_id)
  }

  # check whether the colum names are entered correctly
  test_colnames <- NULL
  for(i in 1:length(name_data)) {
    test_colnames[i] <- any(original_colnames == name_data[i])
  }
  if (any(test_colnames == FALSE)) {stop("Specify valid column names")}

  # trials deletion (if any) --------------------------
  if(!is.null(trial_id)){
    data <- data[!(data[, trial_id]) %in% trial_eliminate, ]
    # select meaningful column
    data <- data[, c(sbj_id, block_id, trial_id, accuracy_id, latency_id)]
    names(data)[names(data) == trial_id] <- "trial"
  } else if (is.null(trial_id)) {
    data <- data
    # select meanaingful column
    data <- data[, c(sbj_id, block_id, accuracy_id, latency_id)]
  }
  # prepare an empty list (sciat) for storing the results
  sciat <- list()

  data[ , block_id] <- as.character(data[, block_id])
  # rename block column
  names(data)[names(data) == block_id] <- "block"
  # rename subject column
  names(data)[names(data) == sbj_id] <- "participant"
  # rename latency column
  names(data)[names(data) == latency_id] <- "latency"
  # rename correct column
  names(data)[names(data) == accuracy_id] <- "correct"
  # sciat 1 labels check --------------------------
  check_b1 <- list()
  for (i in 1:length(block_sciat_1)) {
    check_b1[[i]] <- any(data[ , "block"] == block_sciat_1[i])
  }
  b1_test <- unlist(check_b1)
  if (any(b1_test == FALSE)) {
    stop("blocks labels not found in the dataset")
  } else {
    # save data from sciat 1
    sciat1 <- data[data[, "block"] %in% block_sciat_1,  ]
    sciat$sciat1 <- sciat1
  }

  # store data for sciat 2 (if present) --------------------------
  if (!is.null(block_sciat_2)){
    # check whether the labels exist
    check_b2 <- list()
    for (i in 1:length(block_sciat_2)) {
      check_b2[[i]] <- any(data[ , "block"] == block_sciat_2[i])
    }
    b2_test <- unlist(check_b2)
    if (any(b2_test == FALSE)) {
      stop("blocks labels not found in the dataset")
    } else {
      sciat2 <- data[data[ , "block"] %in% block_sciat_2, ]
      sciat$sciat2 <- sciat2
      # assign a class to each sciat
      class(sciat$sciat1) <- append(class(sciat$sciat1), "sciat_clean")
      class(sciat$sciat2) <- append(class(sciat$sciat2), "sciat_clean")
    }
  } else {
    # otherwise, keep only sciat 1
    class(sciat1) <- append(class(sciat1), "sciat_clean")
    sciat$sciat1 <- sciat1
  }

  # check/store demographic variables --------------------------
  if (!is.null(demo_id)) {
    demo <- original[original[, demo_id] %in% trial_demo, ]
    names(demo)[names(demo) == sbj_id] <- "participant"
    sciat$demo <- demo
  } else {
    sciat <- sciat
  }
  # results --------------------------
  return(sciat)
}
