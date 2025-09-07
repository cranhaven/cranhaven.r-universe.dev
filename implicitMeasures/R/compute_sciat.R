#' Compute the D-score for the SC-IAT
#'
#' Compute the D-score for the SC-IAT.
#'
#' @param data Data frame with class \code{clean_sciat}.
#' @param mappingA String. Label identifying the mapping A of the SC-IAT in the
#'                    \code{block_id} variable.
#' @param mappingB String. Label identifying the mapping B of the SC-IAT in the
#'                    \code{block_id} variable.
#' @param non_response String. Labels of the trials identifying the
#'                      non-responses, a.k.a responses beyond the response time
#'                      window, as it was specified in \code{trial_id} (if included).
#'
#' @return A dataframe with class \code{compute_sciat}. The number of rows of the
#'         dataframe corresponds to the total number of participants. Variables
#'         are defined as follows (the values are specific for each
#'          participant):
#'
#'  \describe{
#'  \item{\code{participant}}{Respondents' IDs.}
#'  \item{\code{n_trial}}{Number of trial before data cleaning.}
#'  \item{\code{no_response}}{If there were any trials identifying the non
#'                 response, it indicates the number of non responses per each
#'                 participant. Otherwise, it is equal for all participants
#'                 (\code{"none"}).}
#'  \item{\code{nslow10000}}{Number of slow trials (> 10,000 ms).}
#'  \item{\code{out_accuracy}}{Indicates whether the participants had more
#'                 than 25 \% of incorrect responses in at least one of the
#'                 critical blocks and hence should be eliminated (\code{"out"})
#'                 or not (\code{"keep"}).}
#'  \item{\code{nfast400}}{Number of fast trials (< 400 ms).}
#'  \item{\code{nfast300}}{Number of fast trials (< 350 ms -- deleted).}
#'  \item{\code{accuracy.mappingA}}{Proportion of correct responses in Mapping A.}
#'  \item{\code{accuracy.mappingB}}{Proportion of correct responses in mapping B.}
#'  \item{\code{RT_mean.MappingA}}{Mean response time in Mapping A.}
#'  \item{\code{RT_mean.MappingB}}{Mean response time in Mapping B.}
#'  \item{\code{cond_ord}}{Indicates the order with which the associative
#'                  conditions have been presented, either \code{"MappingA_First"} or
#'                  \code{"MappingB_First"}.}
#'  \item{\code{legendMappingA}}{Indicates the corresponding value of Mapping A
#'     in the original dataset.}
#'  \item{\code{legendMappingB}}{Indicates the corresponding value of Mapping B
#'     in the original dataset.}
#'  \item{\code{d_sciat}}{SC-IAT \emph{D}.}
#'  }
#'
#'
#'
#' @export
#'
#'
#' @examples
#' # calculate D for the SCIAT
#'   data("raw_data") # load data
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
#'  sciat1 <- sciat_data[[1]] # compute D for the first SC-IAT
#'  d_sciat1 <- compute_sciat(sciat1,
#'                     mappingA = "test.sc_dark.Darkbad",
#'                     mappingB = "test.sc_dark.Darkgood",
#'                     non_response = "alert")
#'  head(d_sciat1) # dataframe containing the SC-IAT D of the of the
#'                 # first SC-IAT
#'
#'  sciat2 <- sciat_data[[2]] # Compute D for the second SC-IAT
#'  d_sciat2 <- compute_sciat(sciat2,
#'                     mappingA = "test.sc_milk.Milkbad",
#'                     mappingB = "test.sc_milk.Milkgood",
#'                     non_response = "alert")
#'  head(d_sciat2)

compute_sciat <- function(data,
                   mappingA = "mappingA",
                   mappingB = "mappingB",
                   non_response = NULL){
  # check dataset class --------------------------
  if (is.na(class(data)[2]) | class(data)[2] != "sciat_clean"){
    stop('Object is not of class sciat_clean')
  }
  options_label = c(mappingA, mappingB)
  names(options_label) <- c("mappingA", "mappingB")

  # labels check --------------------------
  if (sum(any(data[, "block"] == mappingA)) == 0 |
      sum(any(data[, "block"] == mappingB)) == 0) {
    stop("mappingA/mappingB labels not found in the dataset")
  } else if (mappingA == mappingB) {
    stop("the labels of the two conditions have the same name")
  }

  # preparare dataset --------------------------
  data$condition <- ifelse(data[ , "block"] == options_label[1],
                           "mappingA", "mappingB")
  data[,"participant"] <- as.character(data[ , "participant"])
  # create order of presentation variable
  condition_order <- aggregate(condition ~ participant,
                               data = data,
                               unique)
  colnames(condition_order)[2] <-  "order"
  condition_order$cond_ord <- paste(condition_order$order[ , 1],
                                    condition_order$order[ , 2],
                                    sep = "_")
  condition_order <- condition_order[, c("participant", "cond_ord")]
  condition_order$cond_ord <- with(condition_order,
                                   ifelse(cond_ord == "MappingA_MappingB",
                                          "MappingA_First",
                                          "MappingB_First"))
  condition_order$legendMappingA <- mappingA
  condition_order$legendMappingB <- mappingB

  # take out non responses, if any:
  # count the non responses before removing
  if (is.null(non_response)) {
    data <- data
    n_resp <- data.frame(participant = unique(data[, "participant"]),
                         no_response = "none")
  } else {
    if (dim(table(data[, "trial"] == non_response,
                  data[, "participant"]))[1] == 2){
      n_resp <- data.frame(table(data[, "trial"] == non_response,
                                 data[, "participant"]))
      n_resp <- n_resp[n_resp$Var1 %in% TRUE, c(2,3) ]
      colnames(n_resp) <- c("participant", "no_response")

    } else {
      n_resp <- data.frame(participant = unique(data[, "participant"]),
                           no_response = 0)
    }
    data <- data[!(data[, "trial"]) %in% non_response, ]
  }

  #store number of trials per participant
  n_trial <- data.frame(table(data$participant))
  colnames(n_trial) <- c("participant", "n_trial")
  # filter for slow responses (10000ms)
  data$slow10000 <- ifelse(
    data$latency > 10000,
    "out", "keep")
  # create table for slow participants
  table_slow <- table(data$slow1000, data$participant)
  # filter for fast responses (400ms)
  data$fast400 <- ifelse(data$latency < 400,
                         "out", "keep")
  # create table for fast participants
  table_400 <- table(data$fast400, data$participant)
  # filter for fast responses (300ms)
  data$fast350 <- ifelse(data$latency < 350,
                         "out", "keep")
  # create table for fast participants
  table_350 <- table(data$fast350, data$participant)
  # filter on accuracy
  accuracy_clean <- aggregate(correct ~ participant + condition,
                              data = data,
                              mean)
  acc_clean_wide <- reshape(accuracy_clean,
                            idvar = "participant",
                            timevar = "condition",
                            direction = "wide")
  acc_clean_wide$out_accuracy <- ifelse(acc_clean_wide$correct.mappingA < .75 |
                                          acc_clean_wide$correct.mappingB < .75,
                                        "out", "keep")
  data <- merge(data,
                acc_clean_wide,
                by = "participant")
  descript_data <- data.frame(participant = acc_clean_wide$participant)
  descript_data <- merge(descript_data,
                         n_trial,
                         by = "participant")
  descript_data <- merge(descript_data, n_resp)
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
    descript_data <- merge(descript_data, nslow10000,
                           by ="participant")
    # compute the proprortion
    descript_data$nslow10000 <- round(descript_data$nslow10000 /
                                        descript_data$n_trial, 2)
  }
  # number of slow trials (< 400ms)
  if (dim(table_400)[1] == 1){
    # if there are no trials < 400 it just writes 0
    descript_data$nfast400 <- 0
  } else {
    # otherwise it reports the number of trials faster than 400ms
    # per participant
    nfast400 <- data.frame(participant = names(table_400[1, ]),
                           nfast400 = (table_400[2, ]))
    # merge with the descript data
    descript_data <- merge(descript_data, nfast400,
                           by ="participant")
    # compute the proportion
    descript_data$nfast400 <- round(descript_data$nfast400 /
                                      descript_data$n_trial, 2)
  }
  # number of slow trials (< 300ms)
  if (dim(table_350)[1] == 1){
    # if there are no trials < 300 it just writes 0
    descript_data$nfast350 <- 0
  } else {
    # otherwise it reports the number of trials faster than 300ms
    # per participant
    nfast350 <- data.frame(participant = names(table_350[1, ]),
                           nfast350 = (table_350[2, ]))
    # merge with the descript data
    descript_data <- merge(descript_data, nfast350,
                           by ="participant")
    # compute the proportion
    descript_data$nfast350 <- round(descript_data$nfast350 /
                                      descript_data$n_trial, 2)
  }
  # take out fast responses
  data <- data[data$fast350 %in% "keep", ]
  #take out participant based on the accuracy
  sbj_out_acc <- aggregate(out_accuracy ~ participant,
                           data = data,
                           unique)
  descript_data <- merge(descript_data,
                         sbj_out_acc,
                         by = "participant")
  accuracy_condition <- aggregate(correct ~ participant + condition,
                                  data = data,
                                  mean)
  accuracy_condition <- reshape(accuracy_condition,
                                idvar = "participant",
                                timevar = "condition",
                                direction = "wide")
  colnames(accuracy_condition) <- gsub("correct", "accuracy",
                                       colnames(accuracy_condition))
  # Compute D sciat --------------------------
  sciat_correct <- data[data$correct == 1, ]
  sciat_mean <- with(data,
                     aggregate(latency,
                               by = list(condition, participant),
                               FUN = mean))
  colnames(sciat_mean) <- c("condition", "participant", "mean_sc")
  sciat_sd <- with(sciat_correct,
                   aggregate(latency,
                             by = list(participant),
                             FUN = sd))
  colnames(sciat_sd) <- c("participant", "sd_sciat")

  ## merge mean with original dataset (correct responses)
  data <- merge(data,
                sciat_mean,
                by = c("participant", "condition"))
  # merge dataframe with the pooled sd
  data <- merge(data,
                sciat_sd,
                by = "participant" )
  # replace errror response with mean + 400ms
  data$latency_cor <- with(data,
                           ifelse(correct == 0,
                                  mean_sc + 400,
                                  latency))
  sc_mean_correct <- with(data,
                          aggregate(latency_cor,
                                    by = list(condition, participant),
                                    FUN = mean))
  colnames(sc_mean_correct) <- c("condition", "participant", "mean_correct")
  calc_d_sciat <- reshape(sc_mean_correct,
                          idvar = "participant",
                          timevar = "condition",
                          direction = "wide")
  calc_d_sciat <- merge(calc_d_sciat,
                        sciat_sd, by = "participant")
  calc_d_sciat$diff_mean <- with(calc_d_sciat,
                                 mean_correct.mappingA - mean_correct.mappingB)
  calc_d_sciat$d_sciat <- with(calc_d_sciat,
                               diff_mean / sd_sciat)
  d_sciat <- calc_d_sciat[, c("participant", "d_sciat")]
  mean_condition <- aggregate(latency_cor ~ participant + condition,
                              data = data,
                              mean)
  mean_condition <- reshape(mean_condition,
                            idvar = "participant",
                            timevar = "condition",
                            direction = "wide")
  colnames(mean_condition) <- gsub("latency_cor", "RT_mean",
                                   colnames(mean_condition))
  descript_data <- merge(descript_data,
                         accuracy_condition,
                         by = "participant")
  descript_data <- merge(descript_data,
                         mean_condition,
                         by = "participant")
  dsciat_data <- merge(descript_data,
                       condition_order,
                       by = "participant")
  dsciat_data <- merge(dsciat_data, d_sciat, by = "participant")
  dsciat_data <- dsciat_data[, c("participant", "n_trial", "no_response",
                                 "nslow10000","nfast400", "nfast350",
                                 "out_accuracy", "accuracy.mappingA",
                                 "accuracy.mappingB", "RT_mean.mappingA",
                                 "RT_mean.mappingB", "cond_ord",
                                 "legendMappingA", "legendMappingB",
                                 "d_sciat")]
  class(dsciat_data) <- append(class(dsciat_data), "dsciat")
  # results --------------------------
  return(dsciat_data)
}
