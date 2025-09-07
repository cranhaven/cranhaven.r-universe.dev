#' Compute IAT D-score
#'
#' Compute \emph{D-score} for the IAT according to different algorithms.
#'
#' @param data Dataframe with class \code{iat_clean}.
#' @param Dscore Character. Indicates which \emph{D-score} to compute. For
#'                       details on the algorithms, please refer to Greenwald
#'                       et al. (2003).
#'
#' @return Dataframe with class \code{"dscore"}. The number of rows of the
#'          dataframe corresponds to  the total number of participants.
#'          Variables are defined as follows (the values are specific for each
#'          participant):
#'  \describe{
#'  \item{\code{participant}}{Respondents' IDs.}
#'  \item{\code{n_trial}}{Number of trails before data cleaning.}
#'  \item{\code{nslow10000}}{Number of slow trials (> 10,000 ms).}
#'  \item{\code{nfast400}}{Number of fast trials (< 400 ms).}
#'  \item{\code{nfast300}}{Number of fast trials (< 300 ms).}
#'  \item{\code{accuracy.practice_MappingA}}{Proportion of correct responses
#'      in practice block of Mapping A.}
#'  \item{\code{accuracy.practice_MappingB}}{Proportion of correct responses
#'      in practice block of Mapping B.}
#'  \item{\code{accuracy.test_MappingA}}{Proportion of correct responses in test
#'      block of Mapping A.}
#'  \item{\code{accuracy.test_MappingB}}{Proportion of correct responses in test
#'      block of Mapping B.}
#'  \item{\code{accuracy.MappingA}}{Proportion of correct responses in Mapping A.}
#'  \item{\code{accuracy.MappingB}}{Proportion of correct responses in Mapping B.}
#'  \item{\code{RT_mean.MappingA}}{Mean response time in Mapping A.}
#'  \item{\code{RT_mean.MappingB}}{Mean response time in Mapping B.}
#'  \item{\code{mean_practice_MappingA}}{Mean response time in practice block of
#'       Mapping A.}
#'  \item{\code{mean_practice_MappingB}}{Mean response time in practice block of
#'       Mapping B.}
#'  \item{\code{mean_test_MappingA}}{Mean response time in test block of
#'       Mapping A.}
#'  \item{\code{mean_test_MappingB}}{Mean response time in test block of
#'       Mapping B.}
#'  \item{\code{d_practice_dX}}{\emph{D-score}s compute_iat on the practice blocks.
#'      The X stands for the selected \emph{D-score} procedure.}
#'  \item{\code{d_test_dX}}{\emph{D-score}s compute_iat on the test blocks. The X
#'      stands for the selected \emph{D-score} procedure.}
#'  \item{\code{dscore_dX}}{The average \emph{D-score} for the practice and test
#'     \emph{D-score}s. The X stands for the selected \emph{D-score} procedure.}
#'  \item{\code{cond_ord}}{Indicates the order with which the associative
#'      conditions have been presented, either \code{"MappingA_First"} or
#'     \code{"MappingB_First"}.}
#'  \item{\code{legendMappingA}}{Indicates the corresponding value of Mapping A
#'     in the original dataset.}
#'  \item{\code{legendMappingB}}{Indicates the corresponding value of Mapping B
#'     in the original dataset.}
#'  }
#' @export
#'
#'
#' @examples
#' # compute D-score 2 for the IAT data ###
#'   data("raw_data") # import data
#'   iat_cleandata <- clean_iat(raw_data, sbj_id = "Participant",
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
#'   iat_data <- iat_cleandata[[1]]
#' # calculate D-score
#'   iat_dscore <- compute_iat(iat_data,
#'                          Dscore =  "d2")
compute_iat <- function(data,
                     Dscore = c("d1","d2","d3","d4", "d5", "d6")){
  # check the class of the dataset (must be "iat_clean) -----------------------
  if (is.na(class(data)[2]) | class(data)[2] != "iat_clean"){
    stop('use the clean_iat function to prepare the dataset for the compute_iat
         function')
  }
  # define the dscore options
  dscore <- match.arg(Dscore)

  # gives an error if no Dscore has been chosen
  if (length(Dscore) > 1 ) {stop("Select a D-score")}

  # order of block presentation --------------------------
  condition_order <- aggregate(condition ~ participant,
                               data = data,
                               unique)
  colnames(condition_order)[2] <-  "order"
  condition_order$cond_ord <- paste(condition_order$order[ , 1],
                                    condition_order$order[ , 2],
                                    sep = "_")
  condition_order <- condition_order[ , c("participant", "cond_ord")]
  condition_order$cond_ord <- with(condition_order,
                                   ifelse(
                                     cond_ord == "MappingA_MappingB",
                                     "MappingA_First",
                                     "MappingB_First"))
  # store original labels
  start_labels <- aggregate(block_original ~  block, data = data,
                            unique)
  starting_blocks <- factor(start_labels$block_original)
  # create legend
  condition_order$legendMappingA <- paste(as.character(starting_blocks[1]),
                                          "and",
                                          as.character(starting_blocks[3]),
                                          sep = "_")
  condition_order$legendMappingB <- paste(as.character(starting_blocks[2]),
                                          "and",
                                          as.character(starting_blocks[4]),
                                          sep = "_")
  # create filters   --------------------------
  # filter for slow responses (10000ms)
  data$slow10000 <- ifelse(data$latency > 10000,
                           "out", "keep")
  # create table for slow participants
  table_slow <- table(data$slow1000, data$participant)

  # filter for fast responses (400ms)
  data$fast400 <- ifelse(data$latency < 400,
                         "out", "keep")
  # create table for fast participants (400)
  table_400 <- table(data$fast400, data$participant)

  # filter for fast responses (300ms)
  data$fast300 <- ifelse(data$latency < 300,
                         "out", "keep")
  # create table for fast participants (300)
  table_300 <- table(data$fast300, data$participant)

  # filter for fast participants (more than 10% of trials < 300 ms)
  sbj_300 <- data.frame(with(data,
                             table(latency < 300,
                                   participant)))
  if (dim(sbj_300)[1] == length(unique(data$participant))) {
    sbj_300$out_fast <- "keep"
  } else {
    sbj_300 <- sbj_300[sbj_300$Var1 %in% "TRUE", ]
    sbj_300$participant <- as.character(sbj_300$participant)
    for(i in 1:length(unique(data[, "participant"]))) {
      sbj_300$out_fast <- ifelse(sbj_300$Freq >
                                   (table(data[, "participant"])[i])*0.10,
                                 "out", "keep")
    }
  }
  sbj_300 <- sbj_300[, c(2:4)]
  colnames(sbj_300)[2] <- "n.trial300"
  data <- merge(data, sbj_300, by = "participant")

  # filter on accuracy responses
  accuracy_clean <- aggregate(correct ~ participant + condition,
                              data = data,
                              mean)
  acc_clean_wide <- reshape(accuracy_clean,
                            idvar = "participant",
                            timevar = "condition",
                            direction = "wide")
  data <- merge(data,
                acc_clean_wide,
                by = "participant")
  # save number of trials per participant before the deletion of anything
  n_trial <- data.frame(table(data$participant))
  colnames(n_trial) <- c("participant", "n_trial")
  # select only the trials with latency < 100000
  data <- data[data$slow10000 %in% "keep", ]

  # initialize D-score  --------------------------

  if (Dscore == "d1") {
    # built in, no lower treatment
    data1 <- data
    data1$latency_cor <- data1$latency
    d_data <- data1
    d <- "d1"
  } else if (Dscore == "d2") {
    # built in, lower treatment < 400ms
    data2 <- data[data$fast400 %in% "keep", ]
    data2$latency_cor <- data2$latency
    d_data <- data2
    d <- "d2"
  } else if (Dscore == "d3") {
    # no built in, no lower treatment, error = mean + 2sd
    data3 <- data
    # correct response: mean
    correct_time_d3 <- data3[which(data3$correct == 1), ]
    mean_correct_d3 <- aggregate(latency ~ block + participant,
                                 data = correct_time_d3,
                                 mean)
    colnames(mean_correct_d3)[3] <-  "mean"
    # merge original data with mean on correct responses
    data3 <- merge(data3,
                   mean_correct_d3,
                   by = c("participant", "block"))
    # correct response: sd
    sd_correct_d3 <- aggregate(latency ~ block + participant,
                               data = correct_time_d3,
                               sd)
    colnames(sd_correct_d3)[3] <- "sd_block"
    # merge original data with correct sd
    data3 <- merge(data3,
                   sd_correct_d3,
                   by = c("participant", "block"))
    # if the response is incorrect --> penalty, otherwise latency
    data3$latency_cor <- with(data3,
                              ifelse(correct == 0,
                                     mean + (2 * sd_block),
                                     latency))
    d_data <- data3
    d <- "d3"
  } else if (Dscore == "d4") {
    # no built in, no lower treatment, error = mean + 600 ms
    data4 <- data
    # correct response: mean
    correct_time_d4 <- data4[which(data4$correct == 1), ]
    mean_correct_d4 <- aggregate(latency ~ block + participant,
                                 data = correct_time_d4, mean)
    colnames(mean_correct_d4)[3] <-  "mean"
    # merge original data with mean on correct responses
    data4 <- merge(data4, mean_correct_d4,
                   by = c("participant", "block"))

    # if the respone is correct --> penalty, otherwise latency
    data4$latency_cor <- with(data4,
                              ifelse(correct == 0,
                                     (mean + 600),
                                     latency))
    d_data <- data4
    d <- "d4"
  } else if (Dscore == "d5") {
    # no built in, lower treatment < 400ms, error = mean + 2sd
    data5 <- data[data$fast400 %in% "keep", ]
    # correct response: mean
    correct_time_d5 <- data5[which(data5$correct == 1), ]
    mean_correct_d5 <- aggregate(latency ~ block + participant,
                                 data = correct_time_d5,
                                 mean)
    colnames(mean_correct_d5)[3] <-  "mean"
    # merge original data with mean on correct responses
    data5 <- merge(data5, mean_correct_d5,
                   by = c("participant", "block"))
    # correct response: sd
    sd_correct_d5 <- aggregate(latency ~ block + participant,
                               data = correct_time_d5,
                               sd)
    colnames(sd_correct_d5)[3] <- "sd_block"
    # merge original data with correct sd
    data5 <- merge(data5, sd_correct_d5,
                   by = c("participant", "block"))
    # if the respone is mapBorrect --> penalty, otherwise latency
    data5$latency_cor <- with(data5,
                              ifelse(correct == 0,
                                     mean + (2 * sd_block),
                                     latency))
    d_data <- data5
    d <- "d5"
  } else if (Dscore == "d6"){
    # no built in, lower treatment < 400ms, error = mean + 600ms
    data6 <- data[data$fast400 %in% "keep", ]
    # correct response: mean
    correct_time_d6 <- data6[which(data6$correct == 1), ]
    mean_correct_d6 <- aggregate(latency ~ block + participant,
                                 data = correct_time_d6,
                                 mean)
    colnames(mean_correct_d6)[3] <-  "mean"
    # merge original data with mean on correct responses
    data6 <- merge(data6, mean_correct_d6,
                   by = c("participant", "block"))

    # if the respone is correct --> penalty, otherwise latency
    data6$latency_cor <- with(data6,
                              ifelse(correct == 0, mean + 600,
                                     latency))
    d_data <- data6
    d <- "d6"
  }

  # dataset with participants and number of trials
  descript_data <- data.frame(participant = unique(d_data$participant))
  descript_data <- merge(descript_data, n_trial)

  # populate dataset with info on sbj --------------------------
  # number of slow trials > 10000ms
  if (dim(table_slow)[1] == 1) {
    # if there are no trials > 10000 it just writes 0
    descript_data$nslow10000 <- 0
  } else {
    # otherwise it reports the number of trials slower than 10000ms
    # per participant
    nslow10000 <- data.frame(participant = names(table_slow[1, ]),
                             nslow10000 = (table_slow[2, ]))
    # merge with the descript data
    descript_data <- merge(descript_data,
                           nslow10000,
                           by ="participant")
    # compute the proprortion
    descript_data$nslow10000 <- round(descript_data$nslow10000 /
                                        descript_data$n_trial, 2)
  }

  # number of slow trials < 400ms
  if (dim(table_400)[1] == 1) {
    # if there are no trials < 400 it just writes 0
    descript_data$nfast400 <- 0
  } else {
    # otherwise it reports the number of trials faster than 400ms
    # per participant
    nfast400 <- data.frame(participant = names(table_400[1, ]),
                           nfast400 = (table_400[2, ]))
    # merge with the descript data
    descript_data <- merge(descript_data, nfast400,
                           by = "participant")
    # compute the proportion
    descript_data$nfast400 <- round(descript_data$nfast400 /
                                      descript_data$n_trial, 2)
  }

  # number of slow trials < 300ms
  if (dim(table_300)[1] == 1){
    # if there are no trials < 300 it just writes 0
    descript_data$nfast300 <- 0
  } else {
    # otherwise it reports the number of trials faster than 300ms
    # per participant
    nfast300 <- data.frame(participant = names(table_300[1, ]),
                           nfast300 = (table_300[2, ]))
    # merge with the descript data
    descript_data <- merge(descript_data,
                           nfast300,
                           by ="participant")
    # compute the proportion
    descript_data$nfast300 <- round(descript_data$nfast300 /
                                      descript_data$n_trial, 2)
  }

  # create dataset on accuracy performance
  accuracy_block <- aggregate(correct ~ participant + block,
                              data = d_data,
                              mean)
  accuracy_block <- reshape(accuracy_block, idvar = "participant",
                            timevar = "block", direction = "wide")
  colnames(accuracy_block) <- gsub("correct", "accuracy",
                                   colnames(accuracy_block))
  accuracy_condition <- aggregate(correct ~ participant + condition,
                                  data = d_data,
                                  mean)
  accuracy_condition <- reshape(accuracy_condition, idvar = "participant",
                                timevar = "condition", direction = "wide")
  colnames(accuracy_condition) <- gsub("correct", "accuracy",
                                       colnames(accuracy_condition))
  # create dataset on time performance
  mean_condition <- aggregate(latency_cor ~ participant + condition,
                              data = d_data,
                              mean)
  mean_condition <- reshape(mean_condition, idvar = "participant",
                            timevar = "condition", direction = "wide")
  colnames(mean_condition) <- gsub("latency_cor", "RT_mean",
                                   colnames(mean_condition))
  descript_data <- merge(descript_data, accuracy_block,
                         by = "participant")
  descript_data <- merge(descript_data, accuracy_condition,
                         by = "participant")
  descript_data <- merge(descript_data, mean_condition,
                         by = "participant")
  # Compute D score --------------------------
  variance <- with(d_data,
                   aggregate(latency_cor,
                             by = list(participant, block_pool),
                             FUN = var))
  colnames(variance) <- c("participant", "block_pool", "variance")
  sbj_mean <- aggregate(latency_cor ~ participant + block,
                        data = d_data,
                        mean)
  colnames(sbj_mean)[3] <- "mean"
  sbj_mean$block_pool <- sbj_mean$block
  sbj_mean$block_pool  <- gsub("_MappingA|_MappingB", '',
                               sbj_mean$block_pool)
  sbj_data <- merge(variance,
                    sbj_mean,
                    by = c("participant","block_pool"))
  sbj_data_wide <- reshape(sbj_data,
                           idvar = "participant",
                           timevar = "block",
                           direction = "wide")
  sbj_data_wide <- sbj_data_wide[,
                                 c("participant",
                                   "block_pool.practice_MappingA",
                                   "variance.practice_MappingA",
                                   "mean.practice_MappingA",
                                   "mean.test_MappingA",
                                   "block_pool.test_MappingB",
                                   "variance.test_MappingB",
                                   "mean.test_MappingB",
                                   "mean.practice_MappingB")]
  colnames(sbj_data_wide) <- c("participant",
                               "block_pool_practice_MappingA",
                               "variance_practice",
                               "mean_practice_MappingA",
                               "mean_test_MappingA",
                               "block_pool_test_MappingB",
                               "variance_test",
                               "mean_test_MappingB",
                               "mean_practice_MappingB")
  sbj_data_wide$diff_practice <- with(sbj_data_wide,
                                      mean_practice_MappingB -
                                        mean_practice_MappingA)
  sbj_data_wide$diff_test <- with(sbj_data_wide,
                                  mean_test_MappingB -
                                    mean_test_MappingA)
  sbj_data_wide$d_practice <- with(sbj_data_wide,
                                   diff_practice / sqrt(variance_practice))
  sbj_data_wide$d_test <- with(sbj_data_wide,
                               diff_test/sqrt(variance_test))
  sbj_data_wide$dscore <- with(sbj_data_wide,
                               (rowSums(
                                 sbj_data_wide[ , c("d_practice","d_test")])) / 2)
  sbj_data_wide <- sbj_data_wide[ , c("participant", "mean_practice_MappingA",
                                      "mean_test_MappingA",
                                      "mean_practice_MappingB",
                                      "mean_test_MappingB",
                                      "d_practice", "d_test",
                                      "dscore")]
  colnames(sbj_data_wide)[6:8] <- paste(colnames(sbj_data_wide)[6:8],
                                        d, sep = "_")
  Dscore_data <- merge(descript_data,
                       sbj_data_wide,
                       by = "participant")
  Dscore_data <- merge(Dscore_data, condition_order,
                       by = "participant")
  class(Dscore_data) <- append(class(Dscore_data), "dscore")
  # results --------------------------
  return(Dscore_data)
}
