user_agent <- httr::user_agent("https://gitlab.com/ntrlshrp/metaculr")

#' Retrieve questions from Metaculus API
#'
#' @param api_domain Use "www" unless you have a custom Metaculus domain
#' @param order_by Choose "last_prediction_time", "-activity", "-votes", "-publish_time", "close_time", "resolve_time", "last_prediction_time"
#' @param status Choose "all", "upcoming", "open", "closed", "resolved"
#' @param search Search term(s)
#' @param guessed_by Generally your Metaculus_user_id
#' @param offset Question offset
#' @param pages Number of pages to request
#'
#' @return A list of questions, ordered by last prediction time.
#' @export
#' @family Question Retrieval functions
#'
#' @examples
#' \dontrun{
#' questions_recent_open <-
#'   MetaculR_questions(
#'     order_by = "close_time",
#'     status = "open",
#'     guessed_by = "")
#' }

MetaculR_questions <- function(api_domain = "www", order_by = "last_prediction_time", status = "all", search = "", guessed_by = "", offset = 0, pages = 10) {
  pb <- progress::progress_bar$new(
    format = "  downloading [:bar] :percent eta: :eta",
    total = pages - 1, clear = FALSE, width= 60)
  pb$tick(0)
  #for (i in 1:100) {

  #}

  endpoint <- paste0("https://", api_domain, ".metaculus.com/api2/questions/?")
  extra <- paste0("order_by=", order_by, "&status=", status, "&search=", search, "&guessed_by=", guessed_by, "&limit=20&offset=", offset)

  endpoint <- paste0(endpoint, extra)
  message(endpoint)

  get <- httr::GET(url = endpoint, user_agent)
  data = jsonlite::fromJSON(rawToChar(get$content))
  data_all <- list(data)

  page <- 1
  offset_base <- 20

  while(length(data$results$id) == 20) {
    pb$tick()
    Sys.sleep(0.5)
    #print(paste0(page, " "))

    endpoint <- paste0("https://", api_domain, ".metaculus.com/api2/questions/?")
    extra <- paste0("order_by=", order_by, "&status=", status, "&search=", search, "&guessed_by=", guessed_by, "&limit=20&offset=", offset + offset_base)

    endpoint <- paste0(endpoint, extra)

    get <- httr::GET(url = endpoint, user_agent)
    data <- jsonlite::fromJSON(rawToChar(get$content))
    if(length(data$results) == 0) break
    data_all <- append(data_all, list(data))

    page <- page + 1
    offset_base <- page * 20

    if(page == pages) break
  }

  dupes <- which(
    duplicated(
      unlist(
        lapply(data_all, function(x)
          x$results$id))))
  if(length(dupes) > 0) {
    for(d in dupes) {
      data_all[[d %/% 20 + 1]]$results$possibilities$type[d %% 20] <- "duplicate"
      data_all[[d %/% 20 + 1]]$results$id[d %% 20] <- NA
    }
  }

  return(data_all)
}





#' Retrieve questions from Metaculus API (A wrapper for MetaculR_questions())
#'
#' @param api_domain Use "www" unless you have a custom Metaculus domain
#' @param order_by Default is "last_prediction_time"
#' @param status Choose "all", "upcoming", "open", "closed", "resolved"
#' @param search Search term(s)
#' @param guessed_by Generally your Metaculus_user_id
#' @param offset Question offset
#' @param pages Number of pages to request
#'
#' @return A list of questions that I've predicted, ordered by last prediction time.
#' @export
#' @family Question Retrieval functions
#'
#' @examples
#' \dontrun{
#' questions_myPredictions <-
#'   MetaculR_myPredictions(
#'     guessed_by = Metaculus_user_id)
#' }

MetaculR_myPredictions <- function(api_domain = "www", order_by = "last_prediction_time", status = "all", search = "", guessed_by = "", offset = 0, pages = 10) {
  data_all <- MetaculR_questions(api_domain = api_domain, order_by = order_by, status = status, search = search, guessed_by = guessed_by, offset = offset, pages = pages)

  return(data_all)
}





#' Retrieve questions from Metaculus API (A wrapper for MetaculR_questions())
#'
#' @param api_domain Use "www" unless you have a custom Metaculus domain
#' @param order_by Default is "-resolve_time"
#' @param status Default is "resolved"
#' @param search Search term(s)
#' @param guessed_by Generally your Metaculus_user_id
#' @param offset Question offset
#' @param pages Number of pages to request
#'
#' @return A list of questions that I've predicted, ordered by last prediction time, and resolved.
#' @export
#' @family Question Retrieval functions
#'
#' @examples
#' \dontrun{
#' questions_myPredictions_resolved <-
#'   MetaculR_myPredictions_Resolved(
#'     guessed_by = Metaculus_user_id)
#' }

MetaculR_myPredictions_Resolved <- function(api_domain = "www", order_by = "-resolve_time", status = "resolved", search = "", guessed_by = "", offset = 0, pages = 10) {
  data_all <- MetaculR_questions(api_domain = "www", order_by = order_by, status = status, search = "", guessed_by = guessed_by, offset = offset, pages = pages)

  return(data_all)
}





#' Login to Metaculus
#'
#' @param api_domain Use "www" unless you have a custom Metaculus domain
#'
#' @return Your Metaculus_user_ID.
#' @export
#'
#' @examples
#' \dontrun{
#' Metaculus_user_id <-
#'   MetaculR_login()
#' }

MetaculR_login <- function(api_domain = "www") {
  if(Sys.getenv("Metaculus_username") == "") {
    stop("No username in .Renviron!")
  }

  endpoint <- paste0("https://", api_domain, ".metaculus.com/api2/accounts/login/")

  response <- httr::POST(url = endpoint,
                         user_agent,
                         httr::accept_json(),
                         httr::content_type_json(),
                         body = jsonlite::toJSON(list(username = Sys.getenv("Metaculus_username"), password = Sys.getenv("Metaculus_password")),
                                                 auto_unbox = TRUE),
                         encode = "json")

  return(list(Metaculus_user_id = jsonlite::fromJSON(rawToChar(response$content)),
              csrftoken = response$cookies$value[which(response$cookies$name == "csrftoken")]))
}





#' Calculate Brier statistics on MetaculR_questions object
#'
#' @param MetaculR_questions A MetaculR_questions object
#' @param me Show my scores alongside Metaculus scores
#' @param thresholds Thresholds to bin questions
#'
#' @return A list of Brier statistics for you and Metaculus.
#' \item{brier_me, brier_Metaculus, brier_community}{}
#' \item{baseline.tf}{Logical indicator of whether climatology was provided.}
#' \item{bs}{Brier score}
#' \item{bs.baseline}{Brier Score for climatology}
#' \item{ss}{Skill score}
#' \item{bs.reliability}{Reliability portion of Brier score.}
#' \item{bs.resolution}{Resolution component of Brier score.}
#' \item{bs.uncert}{Uncertainty component of Brier score.}
#' \item{y.i}{Forecast bins -- described as the center value of the bins.}
#' \item{obar.i}{Observation bins -- described as the center value of the bins.}
#' \item{prob.y}{Proportion of time using each forecast.}
#' \item{obar}{Forecast based on climatology or average sample observations.}
#' \item{thresholds}{The thresholds for the forecast bins.}
#' \item{check}{ Reliability - resolution + uncertainty should equal brier score.}
#' \item{Other}{}
#' \item{ss_me_Metaculus, ss_me_community, ss_Metaculus_community}{Skill score, me vs. Metaculus, etc.}
#' \item{count_questions}{Number of total questions included.}
#' \item{brier_df: Used for plotting Brier score statistics}{}
#' \item{ID}{Predictor.}
#' \item{name}{Name of value, see above.}
#' \item{value}{Value.}
#' \item{brier_bins_df: Used for plotting histogram and calibration plots.}{}
#' \item{ID}{Predictor.}
#' \item{centers}{y.i, see above.}
#' \item{freqs}{prob.y, see above.}
#' \item{obars}{obar.i, see above.}
#' \item{ideal}{Ideal calibration where centers equals obars.}
#' \item{ci_low}{Low end of 95% confidence interval for obar.i.}
#' \item{ci_high}{High end of 95% confidence interval for obar.i.}
#' @export
#'
#' @examples
#' \dontrun{
#' brier_me <-
#'   MetaculR_brier(
#'     questions_myPredictions_resolved)
#' }

MetaculR_brier <- function(MetaculR_questions, me = TRUE, thresholds = seq(0,1,0.1)) {
  ## no visible binding for global variable solution
  ID <- NULL

  assertthat::assert_that(
    is.list(MetaculR_questions) &
      !is.data.frame(MetaculR_questions),
    msg = "Are you sure that is a MetaculR_questions object returned by the Metaculus API?"
  )
  assertthat::assert_that(
      length(MetaculR_questions[[2]]) == 3,
    msg = "MetaculR currently only works on MetaculR_questions objects that are greater than one page of results (20 questions) from the Metaculus API. Please make more predictions and/or relax your search criteria (e.g., use `MetaculR_myPredictions()` instead of `MetaculR_myPredictions_Resolved()`) and try again."
  )

  my_predictions <- unlist(lapply(MetaculR_questions, function(x) if(is.data.frame(x$results$my_predictions)) {lapply(x$results$my_predictions$predictions[which(x$results$possibilities$type == "binary" & !is.na(x$results$resolution) & x$results$resolution != -1 & unlist(lapply(x$results$metaculus_prediction$full, function(z) !is.null(z))))], function(f) f$x[length(f$x)])}))

  if(is.null(my_predictions) |
     me == FALSE) {
    binary_questions <- data.frame(
      id = MetaculR__questions_id(MetaculR_questions,
                                  scope = "resolved_community"),
      observed = unlist(lapply(MetaculR_questions, function(x) x$results$resolution[which(x$results$possibilities$type == "binary" & !is.na(x$results$resolution) & x$results$resolution != -1 & unlist(lapply(x$results$metaculus_prediction$full, function(z) !is.null(z))))])),
      metaculus_prediction = unlist(lapply(MetaculR_questions, function(x) x$results$metaculus_prediction$full[which(x$results$possibilities$type == "binary" & !is.na(x$results$resolution) & x$results$resolution != -1 & unlist(lapply(x$results$metaculus_prediction$full, function(z) !is.null(z))))])),
      community_prediction = unlist(lapply(MetaculR_questions, function(x) x$results$community_prediction$full$q2[which(x$results$possibilities$type == "binary" & !is.na(x$results$resolution) & x$results$resolution != -1 & unlist(lapply(x$results$community_prediction$full, function(z) !is.null(z))))]))
    )

    brier_me <- NULL
    brier_Metaculus <-
      verification::brier(
        obs = binary_questions$observed,
        pred = binary_questions$metaculus_prediction,
        thresholds = thresholds) ### c(0, exp(seq(from = -4.59512, to = 4.59512, by = 0.919024)) / (exp(seq(from = -4.59512, to = 4.59512, by = 0.919024)) + 1), 1)
    brier_community <-
      verification::brier(
        obs = binary_questions$observed,
        pred = binary_questions$community_prediction,
        thresholds = thresholds) ### c(0, exp(seq(from = -4.59512, to = 4.59512, by = 0.919024)) / (exp(seq(from = -4.59512, to = 4.59512, by = 0.919024)) + 1), 1)
    ss_me_Metaculus <- NULL
    ss_me_community <- NULL

    brier_df <-
      data.frame(ID = c("Me", "Metaculus", "Community", "NA"),
                 bs = c(NA, brier_Metaculus$bs, brier_community$bs, NA),
                 bs.baseline = c(NA, NA, NA, brier_Metaculus$bs.baseline),
                 bs.reliability = c(NA, brier_Metaculus$bs.reliability, brier_community$bs.reliability, NA),
                 bs.resolution = c(NA, brier_Metaculus$bs.resol, brier_community$bs.resol, NA),
                 bs.uncertainty = c(NA, NA, NA, brier_Metaculus$bs.uncert),
                 skill_baseline = c(NA, brier_Metaculus$ss, brier_community$ss, NA),
                 obar = c(NA, NA, NA, brier_Metaculus$obar),
                 skill_Metaculus = c(NA, NA, 1 - brier_community$bs / brier_Metaculus$bs, NA),
                 skill_community = c(NA, 1 - brier_Metaculus$bs / brier_community$bs, NA, NA)) %>%
      tidyr::pivot_longer(cols = -ID)

    bins_df <- data.frame(x = round(brier_Metaculus$obar.i * brier_Metaculus$prob.y * nrow(binary_questions), 0),
                          n = round(brier_Metaculus$prob.y * nrow(binary_questions), 0))

    df_binom.test <- apply(bins_df[which(!is.na(bins_df$x)), ],
                           MARGIN = 1,
                           FUN = function(z) stats::binom.test(z[1], z[2]))

    brier_Metaculus_bins_df <-
      data.frame(ID = "Metaculus",
                 centers = brier_Metaculus$y.i[which(!is.na(bins_df$x))],
                 freqs = brier_Metaculus$prob.y[which(!is.na(bins_df$x))],
                 obars = brier_Metaculus$obar.i[which(!is.na(bins_df$x))],
                 ideal = brier_Metaculus$y.i[which(!is.na(bins_df$x))],
                 ci_low = unlist(lapply(df_binom.test, function(x) x$conf.int[1])),
                 ci_high = unlist(lapply(df_binom.test, function(x) x$conf.int[2])))

    bins_df <- data.frame(x = round(brier_community$obar.i * brier_community$prob.y * nrow(binary_questions), 0),
                          n = round(brier_community$prob.y * nrow(binary_questions), 0))

    df_binom.test <- apply(bins_df[which(!is.na(bins_df$x)), ],
                           MARGIN = 1,
                           FUN = function(z) stats::binom.test(z[1], z[2]))

    brier_community_bins_df <-
      data.frame(ID = "Community",
                 centers = brier_community$y.i[which(!is.na(bins_df$x))],
                 freqs = brier_community$prob.y[which(!is.na(bins_df$x))],
                 obars = brier_community$obar.i[which(!is.na(bins_df$x))],
                 ideal = brier_community$y.i[which(!is.na(bins_df$x))],
                 ci_low = unlist(lapply(df_binom.test, function(x) x$conf.int[1])),
                 ci_high = unlist(lapply(df_binom.test, function(x) x$conf.int[2])))

    brier_bins_df <- rbind(brier_Metaculus_bins_df,
                           brier_community_bins_df)
  } else {
    binary_questions <- data.frame(
      id = MetaculR__questions_id(MetaculR_questions,
                                  scope = "resolved_community"),
      observed = unlist(lapply(MetaculR_questions, function(x) x$results$resolution[which(x$results$possibilities$type == "binary" & !is.na(x$results$resolution) & x$results$resolution != -1 & unlist(lapply(x$results$metaculus_prediction$full, function(z) !is.null(z))))])),
      metaculus_prediction = unlist(lapply(MetaculR_questions, function(x) x$results$metaculus_prediction$full[which(x$results$possibilities$type == "binary" & !is.na(x$results$resolution) & x$results$resolution != -1 & unlist(lapply(x$results$metaculus_prediction$full, function(z) !is.null(z))))])),
      community_prediction = unlist(lapply(MetaculR_questions, function(x) x$results$community_prediction$full$q2[which(x$results$possibilities$type == "binary" & !is.na(x$results$resolution) & x$results$resolution != -1 & unlist(lapply(x$results$community_prediction$full, function(z) !is.null(z))))]))
      )

    binary_questions_me <- data.frame(
      id = MetaculR__questions_id(MetaculR_questions,
                                  scope = "resolved_me"),
      my_prediction = my_predictions
      )

    binary_questions <- merge(binary_questions,
                              binary_questions_me)

    brier_me <-
      verification::brier(
        obs = binary_questions$observed,
        pred = binary_questions$my_prediction,
        thresholds = thresholds) ### c(0, exp(seq(from = -4.59512, to = 4.59512, by = 0.919024)) / (exp(seq(from = -4.59512, to = 4.59512, by = 0.919024)) + 1), 1)
    brier_Metaculus <-
      verification::brier(
        obs = binary_questions$observed,
        pred = binary_questions$metaculus_prediction,
        thresholds = thresholds) ### c(0, exp(seq(from = -4.59512, to = 4.59512, by = 0.919024)) / (exp(seq(from = -4.59512, to = 4.59512, by = 0.919024)) + 1), 1)
    brier_community <-
      verification::brier(
        obs = binary_questions$observed,
        pred = binary_questions$community_prediction,
        thresholds = thresholds) ### c(0, exp(seq(from = -4.59512, to = 4.59512, by = 0.919024)) / (exp(seq(from = -4.59512, to = 4.59512, by = 0.919024)) + 1), 1)

    brier_df <-
      data.frame(ID = c("Me", "Metaculus", "Community", "NA"),
                 bs = c(brier_me$bs, brier_Metaculus$bs, brier_community$bs, NA),
                 bs.baseline = c(NA, NA, NA, brier_Metaculus$bs.baseline),
                 bs.reliability = c(brier_me$bs.reliability, brier_Metaculus$bs.reliability, brier_community$bs.reliability,NA),
                 bs.resolution = c(brier_me$bs.resol, brier_Metaculus$bs.resol, brier_community$bs.resol, NA),
                 bs.uncertainty = c(NA, NA, NA, brier_Metaculus$bs.uncert),
                 skill_baseline = c(brier_me$ss, brier_Metaculus$ss, brier_community$ss, NA),
                 obar = c(NA, NA, NA, brier_me$obar),
                 skill_Metaculus = c(1 - brier_me$bs / brier_Metaculus$bs, NA, 1 - brier_community$bs / brier_Metaculus$bs, NA),
                 skill_community = c(1 - brier_me$bs / brier_community$bs, 1 - brier_Metaculus$bs / brier_community$bs, NA, NA)) %>%
      tidyr::pivot_longer(cols = -ID)

    bins_df <- data.frame(x = round(brier_me$obar.i * brier_me$prob.y * nrow(binary_questions), 0),
                          n = round(brier_me$prob.y * nrow(binary_questions), 0))

    df_binom.test <- apply(bins_df[which(!is.na(bins_df$x)), ],
                           MARGIN = 1,
                           FUN = function(z) stats::binom.test(z[1], z[2]))

    brier_me_bins_df <-
      data.frame(ID = "Me",
                 centers = brier_me$y.i[which(!is.na(bins_df$x))],
                 freqs = brier_me$prob.y[which(!is.na(bins_df$x))],
                 obars = brier_me$obar.i[which(!is.na(bins_df$x))],
                 ideal = brier_me$y.i[which(!is.na(bins_df$x))],
                 ci_low = unlist(lapply(df_binom.test, function(x) x$conf.int[1])),
                 ci_high = unlist(lapply(df_binom.test, function(x) x$conf.int[2])))

    bins_df <- data.frame(x = round(brier_Metaculus$obar.i * brier_Metaculus$prob.y * nrow(binary_questions), 0),
                          n = round(brier_Metaculus$prob.y * nrow(binary_questions), 0))

    df_binom.test <- apply(bins_df[which(!is.na(bins_df$x)), ],
                           MARGIN = 1,
                           FUN = function(z) stats::binom.test(z[1], z[2]))

    brier_Metaculus_bins_df <-
      data.frame(ID = "Metaculus",
                 centers = brier_Metaculus$y.i[which(!is.na(bins_df$x))],
                 freqs = brier_Metaculus$prob.y[which(!is.na(bins_df$x))],
                 obars = brier_Metaculus$obar.i[which(!is.na(bins_df$x))],
                 ideal = brier_Metaculus$y.i[which(!is.na(bins_df$x))],
                 ci_low = unlist(lapply(df_binom.test, function(x) x$conf.int[1])),
                 ci_high = unlist(lapply(df_binom.test, function(x) x$conf.int[2])))

    bins_df <- data.frame(x = round(brier_community$obar.i * brier_community$prob.y * nrow(binary_questions), 0),
                          n = round(brier_community$prob.y * nrow(binary_questions), 0))

    df_binom.test <- apply(bins_df[which(!is.na(bins_df$x)), ],
                           MARGIN = 1,
                           FUN = function(z) stats::binom.test(z[1], z[2]))

    brier_community_bins_df <-
      data.frame(ID = "Community",
                 centers = brier_community$y.i[which(!is.na(bins_df$x))],
                 freqs = brier_community$prob.y[which(!is.na(bins_df$x))],
                 obars = brier_community$obar.i[which(!is.na(bins_df$x))],
                 ideal = brier_community$y.i[which(!is.na(bins_df$x))],
                 ci_low = unlist(lapply(df_binom.test, function(x) x$conf.int[1])),
                 ci_high = unlist(lapply(df_binom.test, function(x) x$conf.int[2])))

    brier_bins_df <- rbind(brier_me_bins_df,
                           brier_Metaculus_bins_df,
                           brier_community_bins_df)
  }

  results <- list(brier_me = brier_me,
                  brier_Metaculus = brier_Metaculus,
                  brier_community = brier_community,
                  ss_me_Metaculus = 1 - brier_me$bs / brier_Metaculus$bs,
                  ss_me_community = 1 - brier_me$bs / brier_community$bs,
                  ss_Metaculus_community = 1 - brier_Metaculus$bs / brier_community$bs,
                  count_questions = nrow(binary_questions),
                  brier_df = brier_df,
                  brier_bins_df = brier_bins_df)

  return(results)
}





#' Find important changes within MetaculR_questions object
#'
#' @param MetaculR_questions A MetaculR_questions object
#'
#' @return A dataframe of questions with difference measures (your most recent prediction vs. community's most recent prediction, etc.).
#' \item{id}{Question ID.}
#' \item{title}{Question title.}
#' \item{my_prediction}{My most recent prediction.}
#' \item{community_q2}{Community median.}
#' \item{community_ave}{Community average.}
#' \item{community_q2_pre_me}{Community median immediately prior to my_prediction.}
#' \item{community_ave_pre_me}{Community average immediately prior to my_prediction.}
#' \item{diff_me_q2}{Difference between me and the community median, by logodds.}
#' \item{diff_me_ave}{Difference between me and the community average, by logodds.}
#' \item{diff_comm_q2_pre_me}{Difference between community_q2_pre_me and the community average, by logodds.}
#' \item{diff_comm_ave_pre_me}{Difference between community_ave_pre_me and the community average, by logodds.}
#' \item{diff_me_q2_abs}{Absolute difference between me and the community median, by logodds.}
#' \item{diff_me_ave_abs}{Absolute difference between me and the community average, by logodds.}
#' \item{diff_comm_q2_pre_me_abs}{Absolute difference between community_q2_pre_me and the community average, by logodds.}
#' \item{diff_comm_ave_pre_me_abs}{Absolute difference between community_ave_pre_me and the community average, by logodds.}
#' \item{diff_me_q2_abs_odds}{Absolute difference between me and the community median, by odds.}
#' \item{diff_me_ave_abs_odds}{Absolute difference between me and the community average, by odds.}
#' \item{diff_comm_q2_pre_me_abs_odds}{Absolute difference between community_q2_pre_me and the community average, by odds.}
#' \item{diff_comm_ave_pre_me_abs_odds}{Absolute difference between community_ave_pre_me and the community average, by odds.}
#' @export
#'
#' @examples
#' \dontrun{
#' questions_myPredictions_byDiff <-
#'   MetaculR_myDiff(
#'     questions_myPredictions)
#' }

MetaculR_myDiff <- function(MetaculR_questions) {
  ## no visible binding for global variable solution
  my_prediction <- community_q2 <- community_ave <- community_q2_pre_me <- community_ave_pre_me <- diff_me_q2 <- diff_me_ave <- diff_comm_q2_pre_me <- diff_comm_ave_pre_me <- diff_me_q2_abs <- NULL

  assertthat::assert_that(
    is.list(MetaculR_questions) &
      !is.data.frame(MetaculR_questions),
    msg = "Are you sure that is a MetaculR_questions object returned by the Metaculus API?"
  )
  assertthat::assert_that(
    length(MetaculR_questions[[2]]) == 3,
    msg = "MetaculR currently only works on MetaculR_questions objects that are greater than one page of results (20 questions) from the Metaculus API. Please make more predictions and/or relax your search criteria (e.g., use `MetaculR_myPredictions()` instead of `MetaculR_myPredictions_Resolved()`) and try again."
  )

  loop_results <- data.frame()
  for(l in 1:length(MetaculR_questions)) {
    if(is.data.frame(MetaculR_questions[[l]]$results$my_predictions)) {
      for(el in which(MetaculR_questions[[l]]$results$possibilities$type == "binary" & is.na(MetaculR_questions[[l]]$results$resolution) & !is.na(MetaculR_questions[[l]]$results$community_prediction$full$q2) & !is.na(MetaculR_questions[[l]]$results$my_predictions$active))) {
    loop_results <-
      rbind(
        loop_results,
        data.frame(
          community_q2_pre_me = MetaculR_questions[[l]]$results$community_prediction$history[[el]]$x1$q2[max(which(MetaculR_questions[[l]]$results$community_prediction$history[[el]]$t <= MetaculR_questions[[l]]$results$my_predictions$predictions[[el]]$t[length(MetaculR_questions[[l]]$results$my_predictions$predictions[[el]]$t)]))],
          community_ave_pre_me = MetaculR_questions[[l]]$results$community_prediction$history[[el]]$x2$avg[max(which(MetaculR_questions[[l]]$results$community_prediction$history[[el]]$t <= MetaculR_questions[[l]]$results$my_predictions$predictions[[el]]$t[length(MetaculR_questions[[l]]$results$my_predictions$predictions[[el]]$t)]))] ###,
          # community_t_pre_me = MetaculR_questions[[l]]$results$community_prediction$history[[el]]$t[max(which(MetaculR_questions[[l]]$results$community_prediction$history[[el]]$t <= MetaculR_questions[[l]]$results$my_predictions$predictions[[el]]$t))]
        ))
  }}}

  binary_questions <- data.frame(
    id = MetaculR__questions_id(MetaculR_questions,
                                scope = "unresolved_me"),
    title = unlist(lapply(MetaculR_questions, function(x) if(is.data.frame(x$results$my_predictions)) {x$results$title[which(x$results$possibilities$type == "binary" & is.na(x$results$resolution) & !is.na(x$results$community_prediction$full$q2) & !is.na(x$results$my_predictions$active))]})),
    my_prediction = unlist(lapply(MetaculR_questions, function(x) if(is.data.frame(x$results$my_predictions)) {lapply(x$results$my_predictions$predictions[which(x$results$possibilities$type == "binary" & is.na(x$results$resolution) & !is.na(x$results$community_prediction$full$q2) & !is.na(x$results$my_predictions$active))], function(f) f$x[length(f$x)])})),
    community_q2 = unlist(lapply(MetaculR_questions, function(x) if(is.data.frame(x$results$my_predictions)) {x$results$community_prediction$full$q2[which(x$results$possibilities$type == "binary" & is.na(x$results$resolution) & !is.na(x$results$community_prediction$full$q2) & !is.na(x$results$my_predictions$active))]})),
    community_ave = unlist(lapply(MetaculR_questions, function(x) if(is.data.frame(x$results$my_predictions)) {lapply(x$results$community_prediction$history[which(x$results$possibilities$type == "binary" & is.na(x$results$resolution) & !is.na(x$results$community_prediction$full$q2) & !is.na(x$results$my_predictions$active))], function(f) f$x2$avg[length(f$x2$avg)])}))
  ) %>%
    cbind(loop_results) %>%
    dplyr::mutate(diff_me_q2 = log(my_prediction / (1 - my_prediction)) - log(community_q2 / (1 - community_q2)),
                  diff_me_ave = log(my_prediction / (1 - my_prediction)) - log(community_ave / (1 - community_ave)),
                  diff_comm_q2_pre_me = log(community_q2 / (1 - community_q2)) - log(community_q2_pre_me / (1 - community_q2_pre_me)),
                  diff_comm_ave_pre_me = log(community_ave / (1 - community_ave)) - log(community_ave_pre_me / (1 - community_ave_pre_me))) %>%
    dplyr::mutate(diff_me_q2_abs = abs(diff_me_q2),
                  diff_me_ave_abs = abs(diff_me_ave),
                  diff_comm_q2_pre_me_abs = abs(diff_comm_q2_pre_me),
                  diff_comm_ave_pre_me_abs = abs(diff_comm_ave_pre_me),
                  diff_me_q2_abs_odds = exp(abs(diff_me_q2)),
                  diff_me_ave_abs_odds = exp(abs(diff_me_ave)),
                  diff_comm_q2_pre_me_abs_odds = exp(abs(diff_comm_q2_pre_me)),
                  diff_comm_ave_pre_me_abs_odds = exp(abs(diff_comm_ave_pre_me))) %>%
    dplyr::arrange(dplyr::desc(diff_me_q2_abs))
}





#' Plot the history of a single question
#'
#' @param MetaculR_questions A MetaculR_questions object
#' @param Metaculus_id The ID of the question to plot
#' @param scale_binary Choose "prob", "odds", or "logodds"
#' @param tournament Plot relative log score below main plot
#'
#' @return A ggplot.
#' @export
#'
#' @examples
#' \dontrun{
#' MetaculR_plot(
#'   MetaculR_questions = questions_myPredictions,
#'   Metaculus_id = 10004)
#' }

MetaculR_plot <- function(MetaculR_questions, Metaculus_id, scale_binary = "prob", tournament = FALSE) {
  ## no visible binding for global variable solution
  Date <- q1 <- q2 <- q3 <- x <- community_q2_pre_me <- community_ave_pre_me <- y <- name <- value <- label <- Cum_RelLogScore_No <- Cum_RelLogScore_Yes <- RelLogScore_No <- RelLogScore_Yes <- group <- NULL

  assertthat::assert_that(
    is.list(MetaculR_questions) &
      !is.data.frame(MetaculR_questions),
    msg = "Are you sure that is a MetaculR_questions object returned by the Metaculus API?"
  )
  assertthat::assert_that(
    length(MetaculR_questions) > 1,
    msg = "MetaculR currently only works on MetaculR_questions objects that are greater than one page of results (20 questions) from the Metaculus API. Please make more predictions and/or relax your search criteria (e.g., use `MetaculR_myPredictions()` instead of `MetaculR_myPredictions_Resolved()`) and try again."
  )
  assertthat::assert_that(unlist(lapply(MetaculR_questions, function(x) x$results$possibilities$type[which(x$results$id == Metaculus_id)])) == "binary",
                          msg = "MetaculR_plot() currently only works on binary questions.")

  community <- data.frame(
    cbind(Date = as.POSIXct(unlist(lapply(MetaculR_questions, function(x) if(TRUE) {lapply(x$results$community_prediction$history[which(x$results$id == Metaculus_id)], function(f) f$t)})), origin = "1970-01-01 00:00.00 UTC"),
    q1 = unlist(lapply(MetaculR_questions, function(x) if(TRUE) {lapply(x$results$community_prediction$history[which(x$results$id == Metaculus_id)], function(f) f$x1$q1)})),
    q2 = unlist(lapply(MetaculR_questions, function(x) if(TRUE) {lapply(x$results$community_prediction$history[which(x$results$id == Metaculus_id)], function(f) f$x1$q2)})),
    q3 = unlist(lapply(MetaculR_questions, function(x) if(TRUE) {lapply(x$results$community_prediction$history[which(x$results$id == Metaculus_id)], function(f) f$x1$q3)})))
  ) %>%
    dplyr::mutate(Date = as.POSIXct(Date, origin = "1970-01-01 00:00.00 UTC"),
                  group = "Community")

  if(!is.null(unlist(lapply(MetaculR_questions, function(x) if(TRUE) {lapply(x$results$metaculus_prediction$history[which(x$results$id == Metaculus_id)], function(f) f$x)})))) {
    metaculus_predict <- TRUE

    metaculus <- data.frame(
      Date = as.POSIXct(unlist(lapply(MetaculR_questions, function(x) if(TRUE) {lapply(x$results$metaculus_prediction$history[which(x$results$id == Metaculus_id)], function(f) f$t)})), origin = "1970-01-01 00:00.00 UTC"),
            q1 = NA,
            q2 = unlist(lapply(MetaculR_questions, function(x) if(TRUE) {lapply(x$results$metaculus_prediction$history[which(x$results$id == Metaculus_id)], function(f) f$x)})),
            q3 = NA,
            group = "Metaculus"
    )
    community <- rbind(community,
                       metaculus) %>%
      dplyr::mutate(group = as.factor(group))
  } else {
    metaculus_predict <- FALSE
  }

  if(is.null(unlist(lapply(MetaculR_questions, function(x) if(is.data.frame(x$results$my_predictions)) {lapply(x$results$my_predictions$predictions[which(x$results$id == Metaculus_id)], function(f) f$t)})))) {
    me_predict <- FALSE
  } else {
    me_predict <- TRUE
  }

  if(me_predict == TRUE) {
    me <- data.frame(
      cbind(Date = as.POSIXct(unlist(lapply(MetaculR_questions, function(x) if(is.data.frame(x$results$my_predictions)) {lapply(x$results$my_predictions$predictions[which(x$results$id == Metaculus_id)], function(f) f$t)})), origin = "1970-01-01 00:00.00 UTC"),
            x = unlist(lapply(MetaculR_questions, function(x) if(is.data.frame(x$results$my_predictions)) {lapply(x$results$my_predictions$predictions[which(x$results$id == Metaculus_id)], function(f) f$x)})))
    ) %>%
      dplyr::mutate(Date = as.POSIXct(Date, origin = "1970-01-01 00:00.00 UTC"))

    loop_results <- data.frame()
    for(l in 1:length(MetaculR_questions)) {
      if(is.data.frame(MetaculR_questions[[l]]$results$my_predictions)) {
        for(el in which(MetaculR_questions[[l]]$results$id == Metaculus_id)) {
          loop_results <-
            rbind(
              loop_results,
              data.frame(
                community_q2_pre_me = MetaculR_questions[[l]]$results$community_prediction$history[[el]]$x1$q2[max(which(MetaculR_questions[[l]]$results$community_prediction$history[[el]]$t <= MetaculR_questions[[l]]$results$my_predictions$predictions[[el]]$t[length(MetaculR_questions[[l]]$results$my_predictions$predictions[[el]]$t)]))],
                community_ave_pre_me = MetaculR_questions[[l]]$results$community_prediction$history[[el]]$x2$avg[max(which(MetaculR_questions[[l]]$results$community_prediction$history[[el]]$t <= MetaculR_questions[[l]]$results$my_predictions$predictions[[el]]$t[length(MetaculR_questions[[l]]$results$my_predictions$predictions[[el]]$t)]))],
                Date = MetaculR_questions[[l]]$results$community_prediction$history[[el]]$t[max(which(MetaculR_questions[[l]]$results$community_prediction$history[[el]]$t <= MetaculR_questions[[l]]$results$my_predictions$predictions[[el]]$t[length(MetaculR_questions[[l]]$results$my_predictions$predictions[[el]]$t)]))]
              ))  %>%
            dplyr::mutate(Date = as.POSIXct(Date, origin = "1970-01-01 00:00.00 UTC"))
        }}}

    diff_comm_q2_pre_me_abs_odds <-
      round(exp(abs(log(community$q2[length(community$q2)] / (1 - community$q2[length(community$q2)])) - log(loop_results$community_q2_pre_me / (1 - loop_results$community_q2_pre_me)))), 2)
    diff_me_q2_abs_odds <-
      round(exp(abs(log(community$q2[length(community$q2)] / (1 - community$q2[length(community$q2)])) - log(me$x[length(me$x)] / (1 - me$x[length(me$x)])))), 2)

    tournament_df <- rbind(community %>%
                          dplyr::mutate(x = NA),
                        me %>%
                          dplyr::mutate(q1 = NA, q2 = NA, q3 = NA, group = "Me")) %>%
      dplyr::arrange(Date) %>%
      tidyr::fill(c(q2, x), .direction = "down") %>%
      dplyr::mutate(RelLogScore_Yes = ifelse(is.na(x), log(1), log((x) / (q2))),
                    RelLogScore_No = ifelse(is.na(x), log(1), log((1 - x) / (1 - q2)))) %>%
      dplyr::mutate(Cum_RelLogScore_Yes = dplyr::cummean(RelLogScore_Yes),
                    Cum_RelLogScore_No = dplyr::cummean(RelLogScore_No))
  }

  ylim <- c(-0.1, 1 * 1.1)

  # current_distribution <- data.frame(
  #   x = seq(0.01, 0.99, 0.01),
  #   y = unlist(lapply(MetaculR_questions, function(x) if(TRUE) {lapply(x$results$community_prediction$full$y[which(x$results$id == Metaculus_id)], function(f) f)}))
  #   )
  weighted <- data.frame(x = seq(from = 0.01, to = 0.99, by = 0.01), w = unlist(lapply(MetaculR_questions, function(x) if(TRUE) {x$results$community_prediction$full$y[which(x$results$id == Metaculus_id)]})))
  weighted_v <- c()
  for(r in 1:nrow(weighted)) {
    weighted_v <- c(weighted_v,
                    rep(weighted[r, "x"],
                        times = weighted[r, "w"] * 100))
  }
  current_distribution <- data.frame(
    x = as.POSIXct(max(community$Date)),
    y = weighted_v
  )

  if(scale_binary == "odds") {
    community <- community %>%
      dplyr::mutate(q1 = ifelse(q1 >= 0.5,
                                q1 / (1 - q1),
                                -(1 - q1) / q1),
                    q2 = ifelse(q2 >= 0.5,
                                q2 / (1 - q2),
                                -(1 - q2) / q2),
                    q3 = ifelse(q3 >= 0.5,
                                q3 / (1 - q3),
                                -(1 - q3) / q3))

    current_distribution <- current_distribution %>%
      dplyr::mutate(y = ifelse(y >= 0.5,
                               y / (1 - y),
                               -(1 - y) / y))

    if(me_predict == TRUE) {
      me <- me %>%
        dplyr::mutate(x = ifelse(x >= 0.5,
                                 x / (1 - x),
                                 -(1 - x) / x))

      loop_results <- loop_results %>%
        dplyr::mutate(community_q2_pre_me = ifelse(community_q2_pre_me >= 0.5,
                                                   community_q2_pre_me / (1 - community_q2_pre_me),
                                                   -(1 - community_q2_pre_me) / community_q2_pre_me),
                      community_ave_pre_me = ifelse(community_ave_pre_me >= 0.5,
                                                    community_ave_pre_me / (1 - community_ave_pre_me),
                                                    -(1 - community_ave_pre_me) / community_ave_pre_me))
    }

    ylim <- c(-99 * 1.1, 99 * 1.1)
  }
  if(scale_binary == "logodds") {
    community <- community %>%
      dplyr::mutate(q1 = log(q1 / (1 - q1)),
                    q2 = log(q2 / (1 - q2)),
                    q3 = log(q3 / (1 - q3)))

    current_distribution <- current_distribution %>%
      dplyr::mutate(y = log(y / (1 - y)))

    if(me_predict == TRUE) {
      me <- me %>%
        dplyr::mutate(x = log(x / (1 - x)))

      loop_results <- loop_results %>%
        dplyr::mutate(community_q2_pre_me = log(community_q2_pre_me / (1 - community_q2_pre_me)),
                      community_ave_pre_me = log(community_ave_pre_me / (1 - community_ave_pre_me)))
    }


    ylim <- c(log(1 / 99) * 1.1, log(99) * 1.1)
  }

  coverage_period_length <- community$Date[length(community$Date)] - community$Date[1]
  xlim <- c(community$Date[1] - coverage_period_length * 0.05,
            community$Date[length(community$Date)] + coverage_period_length * 0.05)

  if(metaculus_predict == TRUE) {
    metaculus <- community %>%
      dplyr::filter(group == "Metaculus") %>%
      dplyr::select(-group)
    community <- community %>%
      dplyr::filter(group == "Community") %>%
      dplyr::select(-group)
  } else {
    community <- community %>%
      dplyr::filter(group == "Community") %>%
      dplyr::select(-group)
  }

  gg <- community %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(x = Date,
                   y = q2)
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(x = Date,
                   ymin = q1,
                   ymax = q3),
      alpha = 0.1) +
    ggplot2::geom_rug(data = current_distribution,
      ggplot2::aes(x = x,
                   y = y),
      sides="r",
      alpha = 0.01,
      position = "jitter"
    )

  if(me_predict == TRUE) {
    gg <- gg +
      ggplot2::geom_step(
        data = me %>%
          rbind(data.frame(Date = as.POSIXct(max(community$Date)),
                           x = me$x[length(me$x)])),
        ggplot2::aes(x = Date,
                     y = x),
        linetype = "dotted"
      ) +
    ggplot2::geom_point(
      data = me %>%
        dplyr::mutate(label = ifelse(
          Date == me$Date[length(me$Date)],
          paste0("Odds diff: ", diff_me_q2_abs_odds),
          "")),
      ggplot2::aes(x = Date,
                   y = x),
      shape = 21
      ) +
    ggplot2::geom_point(
      data = loop_results,
      ggplot2::aes(x = Date,
                   y = community_q2_pre_me),
      shape = 21,
      fill = "black"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = loop_results$Date,
                   y = loop_results$community_q2_pre_me,
                   xend = loop_results$Date,
                   yend = q2[length(q2)],
                   color = "color1")) +
      ggplot2::geom_segment(
        ggplot2::aes(x = Date[length(Date)],
                     y = q2[length(q2)],
                     xend = Date[length(Date)],
                     yend = me$x[length(me$x)],
                     color = "color1")) +
      ggrepel::geom_label_repel(
        data = community %>%
          tidyr::pivot_longer(cols = c(q1, q2, q3)) %>%
          dplyr::mutate(label = ifelse(Date == loop_results$Date & name == "q2",
                                       paste0("Odds diff: ", diff_comm_q2_pre_me_abs_odds),
                                       "")) %>%
          rbind(me %>%
                  tidyr::pivot_longer(cols = x) %>%
                  dplyr::mutate(label = ifelse(Date == me$Date[length(me$Date)],
                                               paste0("Odds diff: ", diff_me_q2_abs_odds),
                                               ""))),
        ggplot2::aes(x = Date,
                     y = value,
                     label = label),
        size = 3,
        min.segment.length = 0,
        box.padding = 1,
        max.overlaps = Inf)

    if(tournament == TRUE) {
      if(is.na(unlist(lapply(MetaculR_questions, function(x) x$results$resolution[which(x$results$id == Metaculus_id)]))) |
         unlist(lapply(MetaculR_questions, function(x) x$results$resolution[which(x$results$id == Metaculus_id)])) == -1) {
        tt <- tournament_df %>%
          tidyr::pivot_longer(cols = c(Cum_RelLogScore_Yes,
                                       Cum_RelLogScore_No)) %>%
          ggplot2::ggplot() +
          ggplot2::geom_line(
            ggplot2::aes(x = Date,
                         y = value,
                         color = name)) +
          ggplot2::scale_color_manual(values = c("red", "blue")) +
          ggplot2::theme_classic() +
          ggplot2::coord_cartesian(
            xlim = xlim
          ) +
          ggplot2::labs(x = "Tournament Score (Relative Log Score)",
                        y = "") +
          ggplot2::guides(color = FALSE)
      } else {
        if(unlist(lapply(MetaculR_questions, function(x) x$results$resolution[which(x$results$id == Metaculus_id)])) == 1) {
          tt <- tournament_df %>%
            ggplot2::ggplot() +
            ggplot2::geom_area(
              ggplot2::aes(x = Date,
                           y = Cum_RelLogScore_Yes),
              color = "blue") +
            ggplot2::theme_classic() +
            ggplot2::coord_cartesian(
              xlim = xlim
            ) +
            ggplot2::labs(x = "Tournament Score (Relative Log Score)",
                          y = "")
        } else {
          tt <- tournament_df %>%
            ggplot2::ggplot() +
            ggplot2::geom_area(
              ggplot2::aes(x = Date,
                           y = Cum_RelLogScore_No),
              color = "red") +
            ggplot2::theme_classic() +
            ggplot2::coord_cartesian(
              xlim = xlim
            ) +
            ggplot2::labs(x = "Tournament Score (Relative Log Score)",
                          y = "")
        }
      }
    }
  }

  if(metaculus_predict == TRUE) {
    gg <- gg +
      ggplot2::geom_line(
        data = metaculus,
        ggplot2::aes(
          x = Date,
          y = q2,
          color = "color2"
        )
      ) +
      ggplot2::scale_color_manual(values = c("red", "lightblue"))
  }

  gg <- gg +
    ggplot2::theme_classic() +
    ggplot2::coord_cartesian(
      xlim = xlim,
      ylim = ylim
    ) +
    ggplot2::ggtitle(label = paste0(Metaculus_id, ": ", unlist(lapply(MetaculR_questions, function(x) if(TRUE) {x$results$title[which(x$results$id == Metaculus_id)]})))) +
    ggplot2::labs(y = "Community prediction") +
    ggplot2::guides(color = FALSE)

  if(me_predict == TRUE & tournament == TRUE) {
    cowplot::plot_grid(gg, tt, align = "v", nrow = 2, rel_heights = c(4, 1))
  } else {
    gg
  }
}





#' Find exciting questions
#'
#' @param MetaculR_questions A MetaculR_questions object
#' @param days The time period used for the excitement calculations starts this number of days ago, prior to today. E.g., if your clock says it is day 12 and your `days` argument is 10, the time period is day 2 until the present.
#'
#' @return A dataframe of questions with excitement measures.
#' \item{id}{Question ID.}
#' \item{title}{Question title.}
#' \item{Total_Change}{Cumulative delta in time period, by probability.}
#' \item{Total_logodds_Change}{Cumulative delta in time period, by logodds.}
#' \item{Total_Change_Even}{Cumulative delta toward even odds in time period, by probability.}
#' \item{Total_logodds_Change_Even}{Cumulative delta toward even odds in time period, by logodds.}
#' @export
#'
#' @examples
#' \dontrun{
#' questions_myPredictions_byExcitement <-
#'   MetaculR_excitement(
#'     questions_myPredictions)
#' }

MetaculR_excitement <- function(MetaculR_questions, days = 30) {
  ## no visible binding for global variable solution
  x1 <- Date <- q2_delta <- q2_logodds_delta <- q2_delta_even <- q2_logodds_delta_even <- NULL

  assertthat::assert_that(
    is.list(MetaculR_questions) &
      !is.data.frame(MetaculR_questions),
    msg = "Are you sure that is a MetaculR_questions object returned by the Metaculus API?"
  )
  assertthat::assert_that(
    length(MetaculR_questions[[2]]) == 3,
    msg = "MetaculR currently only works on MetaculR_questions objects that are greater than one page of results (20 questions) from the Metaculus API. Please make more predictions and/or relax your search criteria (e.g., use `MetaculR_myPredictions()` instead of `MetaculR_myPredictions_Resolved()`) and try again."
  )

  binary_questions <- data.frame(
    id = MetaculR__questions_id(MetaculR_questions,
                                scope = "open_community",
                                days = days),
    title = unlist(lapply(MetaculR_questions, function(x) if(TRUE) {x$results$title[which(x$results$possibilities$type == "binary" & as.Date(x$results$close_time) >= as.Date(Sys.time()) - days & unlist(lapply(x$results$community_prediction$history, function(z) nrow(z) > 0)))]})),
    Total_Change = unlist(lapply(MetaculR_questions, function(x) lapply(x$results$community_prediction$history[which(x$results$possibilities$type == "binary" & as.Date(x$results$close_time) >= as.Date(Sys.time()) - days)], function(z) if(nrow(z) > 0) {z %>% dplyr::mutate(Date = as.POSIXct(t, origin = "1970-01-01 00:00.00 UTC"), q2_delta = x1$q2 - dplyr::lag(x1$q2)) %>% dplyr::filter(as.Date(Date) >= as.Date(Sys.time()) - days) %>% dplyr::summarize(Total_Change = sum(abs(q2_delta)))}))),
    Total_logodds_Change = unlist(lapply(MetaculR_questions, function(x) lapply(x$results$community_prediction$history[which(x$results$possibilities$type == "binary" & as.Date(x$results$close_time) >= as.Date(Sys.time()) - days)], function(z) if(nrow(z) > 0) {z %>% dplyr::mutate(Date = as.POSIXct(t, origin = "1970-01-01 00:00.00 UTC"), q2_logodds_delta = log(x1$q2 / (1 - x1$q2)) - log(dplyr::lag(x1$q2) / (1 - dplyr::lag(x1$q2)))) %>% dplyr::filter(as.Date(Date) >= as.Date(Sys.time()) - days) %>% dplyr::summarize(Total_logodds_Change = sum(abs(q2_logodds_delta)))}))),
    Total_Change_Even = unlist(lapply(MetaculR_questions, function(x) lapply(x$results$community_prediction$history[which(x$results$possibilities$type == "binary" & as.Date(x$results$close_time) >= as.Date(Sys.time()) - days)], function(z) if(nrow(z) > 0) {z %>% dplyr::mutate(Date = as.POSIXct(t, origin = "1970-01-01 00:00.00 UTC"), q2_delta_even = ifelse(x1$q2 > 0.5 & x1$q2 - dplyr::lag(x1$q2) < 0, x1$q2 - dplyr::lag(x1$q2), ifelse(x1$q2 < 0.5 & x1$q2 - dplyr::lag(x1$q2) > 0, x1$q2 - dplyr::lag(x1$q2), 0))) %>% dplyr::filter(as.Date(Date) >= as.Date(Sys.time()) - days) %>% dplyr::summarize(Total_Change_Even = sum(abs(q2_delta_even)))}))),
    Total_logodds_Change_Even = unlist(lapply(MetaculR_questions, function(x) lapply(x$results$community_prediction$history[which(x$results$possibilities$type == "binary" & as.Date(x$results$close_time) >= as.Date(Sys.time()) - days)], function(z) if(nrow(z) > 0) {z %>% dplyr::mutate(Date = as.POSIXct(t, origin = "1970-01-01 00:00.00 UTC"), q2_logodds_delta_even = ifelse(x1$q2 > 0.5 & x1$q2 - dplyr::lag(x1$q2) < 0, log(x1$q2 / (1 - x1$q2)) - log(dplyr::lag(x1$q2) / (1 - dplyr::lag(x1$q2))), ifelse(x1$q2 < 0.5 & x1$q2 - dplyr::lag(x1$q2) > 0, log(x1$q2 / (1 - x1$q2)) - log(dplyr::lag(x1$q2) / (1 - dplyr::lag(x1$q2))), 0))) %>% dplyr::filter(as.Date(Date) >= as.Date(Sys.time()) - days) %>% dplyr::summarize(Total_logodds_Change_Even = sum(abs(q2_logodds_delta_even)))}))))
}





#' Easily translate R dataframes to Metaculus Markdown
#'
#' @param df A dataframe.
#'
#' @return A Markdown table.
#' @export
#'
#' @examples
#' \dontrun{
#' my_data <- data.frame(Year = c(2020,2021), Value = c(6, 7.2))
#'
#' MetaculR_markdown_table(my_data)
#' }

MetaculR_markdown_table <- function(df) {
  kable <- df %>%
    knitr::kable()

  kable %>%
    clipr::write_clip()

  print(kable)
  message("

The above Markdown table has already been copied to your clipboard for use wherever Metaculus accepts Markdown input.")
}





#' Aggregate Community Forecasts for MetaculR
#'
#' Provides different results of aggregating current community forecasts to help you make your next forecast.
#'
#' Sevilla (2021) found a Metaculus baseline of 0.36 looking at ~900 questions. While Sevilla has at times referred to the geometric mean of odds, this function uses the equivalent mean of logodds. Also note that mu + (d - 1)(mu + b) (Neyman & Roughgarden) is equivalent to b + d(mu + b), this function uses the former.
#'
#' @param MetaculR_questions A MetaculR_questions object
#' @param Metaculus_id The ID of the question to plot
#' @param baseline Climatological baseline for binary questions
#'
#' @references Neyman, E., & Roughgarden, T. (2022). Are You Smarter Than a Random Expert? The Robust Aggregation of Substitutable Signals. ArXiv:2111.03153 \[Cs\]. <https://arxiv.org/abs/2111.03153>
#'
#' Sevilla, J. (2021, December 29). Principled extremizing of aggregated forecasts. <https://forum.effectivealtruism.org/posts/biL94PKfeHmgHY6qe/principled-extremizing-of-aggregated-forecasts>
#'
#' @return A dataframe of forecast aggregations.
#' \item{id}{Question ID.}
#' \item{community_q2}{Community median.}
#' \item{community_ave}{Community mean.}
#' \item{community_q2_unweighted}{Community median, unweighted by recency.}
#' \item{community_ave_unweighted}{Community mean, unweighted by recency.}
#' \item{community_mean_logodds}{Community mean of logodds.}
#' \item{community_mean_logodds_extremized_baseline}{Community mean of logodds, extremized with reference to a baseline. If the baseline is 0.5, this is "classical extremizing."}
#' @export
#'
#' @examples
#' \dontrun{
#' MetaculR_aggregate_forecasts(
#'   MetaculR_questions = questions_myPredictions,
#'   Metaculus_id = 10004)
#' }

MetaculR_aggregated_forecasts <- function(MetaculR_questions, Metaculus_id, baseline = 0.5) {
  ## no visible binding for global variable solution
  Logodds <- Odds <- Prod <- mu_ext_baseline <- mu_g <- mu_g_odds <- mu_logodds <- mu_logodds_ext_baseline <- w <- x <- NULL

  n = unlist(lapply(MetaculR_questions, function(x) if(TRUE) {x$results$number_of_predictions[which(x$results$id == Metaculus_id)]}))

  d = n * (sqrt(3*n^2 - 3*n + 1) - 2) / (n^2 - n - 1)

  aggregates <- data.frame(
    ID = Metaculus_id,
    community_q2 = unlist(lapply(MetaculR_questions, function(x) if(TRUE) {x$results$community_prediction$full$q2[which(x$results$id == Metaculus_id)]})),
    community_ave = data.frame(x = seq(from = 0.01, to = 0.99, by = 0.01), w = unlist(lapply(MetaculR_questions, function(x) if(TRUE) {x$results$community_prediction$full$y[which(x$results$id == Metaculus_id)]}))) %>% dplyr::mutate(Prod = x * w) %>% dplyr::summarize(mu = sum(Prod) / sum(w)) %>% dplyr::pull(),
    community_q2_unweighted = unlist(lapply(MetaculR_questions, function(x) if(TRUE) {x$results$community_prediction$unweighted$q2[which(x$results$id == Metaculus_id)]})),
    community_ave_unweighted = data.frame(x = seq(from = 0.01, to = 0.99, by = 0.01), w = unlist(lapply(MetaculR_questions, function(x) if(TRUE) {x$results$community_prediction$unweighted$y[which(x$results$id == Metaculus_id)]}))) %>% dplyr::mutate(Prod = x * w) %>% dplyr::summarize(mu = sum(Prod) / sum(w)) %>% dplyr::pull(),
    community_mean_logodds = data.frame(x = seq(from = 0.01, to = 0.99, by = 0.01), w = unlist(lapply(MetaculR_questions, function(x) if(TRUE) {x$results$community_prediction$full$y[which(x$results$id == Metaculus_id)]}))) %>% dplyr::mutate(Odds = x / (1 - x), Logodds = log(x / (1 - x))) %>% dplyr::summarize(mu_g_odds = prod(Odds^w)^(1/sum(w)), mu_logodds = stats::weighted.mean(Logodds, w)) %>% dplyr::mutate(mu_g = exp(mu_logodds) / (exp(mu_logodds) + 1), mu_g2 = mu_g_odds / (mu_g_odds + 1)) %>% dplyr::pull(mu_g), ### Could also be the equivalent: summarize(mu_g = exp(weighted.mean(log(x), w)))
    community_mean_logodds_extremized_baseline = data.frame(x = seq(from = 0.01, to = 0.99, by = 0.01), w = unlist(lapply(MetaculR_questions, function(x) if(TRUE) {x$results$community_prediction$full$y[which(x$results$id == Metaculus_id)]}))) %>% dplyr::mutate(Odds = x / (1 - x), Logodds = log(x / (1 - x))) %>% dplyr::summarize(mu_g_odds = prod(Odds^w)^(1/sum(w)), mu_logodds = stats::weighted.mean(Logodds, w)) %>% dplyr::mutate(mu_logodds_ext_baseline = mu_logodds + (d - 1) * (mu_logodds - log(baseline / (1 - baseline)))) %>% dplyr::mutate(mu_ext_baseline = exp(mu_logodds_ext_baseline) / (exp(mu_logodds_ext_baseline) + 1)) %>% dplyr::pull(mu_ext_baseline)
  )

  return(aggregates)
}










#' Generate probabilistic consensus from multiple parameterized forecasts
#'
#' @param f A list of forecasts (see example for necessary structure).
#'
#' @references McAndrew, T., & Reich, N. G. (2020). An expert judgment model to predict early stages of the COVID-19 outbreak in the United States \[Preprint\]. Infectious Diseases (except HIV/AIDS). https://doi.org/10.1101/2020.09.21.20196725
#'
#' @return A list of forecasts.
#' \item{pdf}{A dataframe of probability density functions corresponding to original forecasts and consensus forecast.}
#' \item{cdf}{A dataframe of cumulative distribution functions corresponding to original forecasts and consensus forecast.}
#' \item{summary}{A dataframe of summary statistics corresponding to original forecasts and consensus forecast, i.e., 10th, 25th, 50th, 75th, 90th centiles and mean.}
#' @export
#'
#' @examples
#' \dontrun{
#' forecasts <- list(list(range = c(0, 250), resolution = 1),
#'   list(source = "Pishkalo",
#'     dist = "Norm",
#'     params = c("mu", "sd"),
#'     values = c(116, 12),
#'     weight = 0.2),
#'   list(source = "Miao",
#'     dist = "Norm",
#'     params = c("mu", "sd"),
#'     values = c(121.5, 32.9)),
#'   list(source = "Labonville",
#'     dist = "TPD",
#'     params = c("min", "mode", "max"),
#'     values = c(89-14, 89, 89+29)),
#'   list(source = "NOAA",
#'     dist = "PCT",
#'     params = c(0.2, 0.8),
#'     values = c(95, 130)),
#'   list(source = "Han",
#'     dist = "Norm",
#'     params = c("mu", "sd"),
#'     values = c(228, 40.5)),
#'   list(source = "Dani",
#'     dist = "Norm",
#'     params = c("mu", "sd"),
#'     values = c(159, 22.3)),
#'   list(source = "Li",
#'     dist = "Norm",
#'     params = c("mu", "sd"),
#'     values = c(168, 6.3)),
#'   list(source = "Singh",
#'     dist = "Norm",
#'     params = c("mu", "sd"),
#'     values = c(89, 9)))
#'
#' MetaculR_probabilistic_consensus(
#'   f = forecasts)
#' }

MetaculR_probabilistic_consensus <- function(f) {
  consensus_df <- data.frame(x = seq(f[[1]]$range[1], f[[1]]$range[2], f[[1]]$resolution))

  for(l in 2:length(f)) {
    switch (f[[l]]$dist,
      ### a series of percentiles, often including a median, e.g., 0.1, 0.5, 0.9
      "PCT" = {
        consensus_df[[f[[l]]$source]] <- NA

        ### first param
        consensus_df[which(consensus_df$x < f[[l]]$values[1]),
                     f[[l]]$source] <-
          f[[l]]$params[1] /
          length(which(consensus_df$x < f[[l]]$values[1]))
        ### middle params
        for(p in 2:(length(f[[l]]$params) - 1)) {
          consensus_df[which(consensus_df$x < f[[l]]$values[p] &
                               consensus_df$x >= f[[l]]$values[p - 1]),
                       f[[l]]$source] <-
            (f[[l]]$params[p] -
               f[[l]]$params[p - 1]) /
            length(which(consensus_df$x < f[[l]]$values[p] &
                           consensus_df$x >= f[[l]]$values[p - 1]))
        }
        ### last param
        consensus_df[which(consensus_df$x <= f[[1]]$range[2] &
                             consensus_df$x >= f[[l]]$values[length(f[[l]]$params)]),
                     f[[l]]$source] <-
          (1 -
             f[[l]]$params[length(f[[l]]$params)]) /
          length(which(consensus_df$x <= f[[1]]$range[2] &
                         consensus_df$x >= f[[l]]$values[length(f[[l]]$params)]))

        # normalize to 1
        consensus_df[ , f[[l]]$source] <-
          consensus_df[ , f[[l]]$source] *
          (1 / sum(consensus_df[ , f[[l]]$source], na.rm = TRUE))
      },

      ### a range with a mode, e.g., min, mode, max
      "TPD" = {
        consensus_df[[f[[l]]$source]] <- NA

        ### first param
        consensus_df[which(consensus_df$x < f[[l]]$values[1]),
                     f[[l]]$source] <-
          0
        ### middle params
        consensus_df[which(consensus_df$x < f[[l]]$values[2] &
                             consensus_df$x >= f[[l]]$values[1]),
                     f[[l]]$source] <-
          (2 * (consensus_df$x[which(consensus_df$x < f[[l]]$values[2] &
                        consensus_df$x >= f[[l]]$values[1])] -
             f[[l]]$values[1])) /
          ((f[[l]]$values[3] - f[[l]]$values[1]) *
          (f[[l]]$values[2] - f[[l]]$values[1]))

        consensus_df[which(consensus_df$x == f[[l]]$values[2]),
                     f[[l]]$source] <-
          2 / (f[[l]]$values[3] - f[[l]]$values[1])

        consensus_df[which(consensus_df$x <= f[[l]]$values[3] &
                             consensus_df$x >= f[[l]]$values[2]),
                     f[[l]]$source] <-
          (2 * (f[[l]]$values[3] -
                  consensus_df$x[which(consensus_df$x <= f[[l]]$values[3] &
                        consensus_df$x >= f[[l]]$values[2])])) /
          ((f[[l]]$values[3] - f[[l]]$values[1]) *
             (f[[l]]$values[3] - f[[l]]$values[2]))

        ### last param
        consensus_df[which(consensus_df$x > f[[l]]$values[3]),
                     f[[l]]$source] <-
          0

        # normalize to 1
        consensus_df[ , f[[l]]$source] <-
          consensus_df[ , f[[l]]$source] *
          (1 / sum(consensus_df[ , f[[l]]$source], na.rm = TRUE))
      },
      ### Floor
      "Floor" = {

      },
      ### built-in functions
      "Norm" = {
        consensus_df[[f[[l]]$source]] <-
          stats::dnorm(x = consensus_df$x,
                mean = f[[l]]$values[which(f[[l]]$params == "mu")],
                sd = f[[l]]$values[which(f[[l]]$params == "sd")])

        # normalize to 1
        consensus_df[ , f[[l]]$source] <-
          consensus_df[ , f[[l]]$source] *
          (1 / sum(consensus_df[ , f[[l]]$source], na.rm = TRUE))
      }
    )
  }

  weights <- consensus_df
  for(l in 2:length(f)) {
    if(length(f[[l]]$weight) == 0) {
      weights[ , l] <- 1
    } else {
      weights[ , l] <- f[[l]]$weight
    }
  }

  consensus_df$agg <- rowMeans(consensus_df[ , -1] * weights[ , -1], na.rm = TRUE)

  # normalize to 1
  consensus_df$agg <-
    consensus_df$agg *
    (1 / sum(consensus_df$agg, na.rm = TRUE))

  cdfs <- data.frame(
    cbind(x = consensus_df$x,
          apply(consensus_df[ , -1],
                MARGIN = 2,
                FUN = function(x) cumsum(x) / sum(x))))

  summary_df <- data.frame()
  for(col in 2:ncol(consensus_df)) {
    ecdf_fn <- spatstat.geom::ewcdf(consensus_df$x,
                                    weights = consensus_df[ , col])
    summary_df <- rbind(summary_df,
                        data.frame(source = colnames(consensus_df)[col],
                                   q10 = stats::quantile(ecdf_fn, p = 0.1),
                                   q25 = stats::quantile(ecdf_fn, p = 0.25),
                                   q50 = stats::quantile(ecdf_fn, p = 0.5),
                                   mu = mean(ecdf_fn),
                                   q75 = stats::quantile(ecdf_fn, p = 0.75),
                                   q90 = stats::quantile(ecdf_fn, p = 0.9)))
  }

  return(list(pdf = consensus_df,
              cdf = cdfs,
              summary = summary_df))
}





MetaculR_predict <- function(api_domain = "www", Metaculus_id = NULL, prediction = NULL, csrftoken = NULL) {
  if(Sys.getenv("Metaculus_username") == "") {
    stop("No username in .Renviron!")
  }

  endpoint <- paste0("https://", api_domain, ".metaculus.com/api2/questions/", Metaculus_id, "/predict/")

  response <- httr::POST(url = endpoint,
                         user_agent,
                         httr::accept_json(),
                         httr::content_type_json(),
                         httr::add_headers(Referer = paste0("https://", api_domain, ".metaculus.com/api2"),
                                           `X-CSRFToken` = csrftoken),
                         body = jsonlite::toJSON(list(prediction = prediction),
                                                 auto_unbox = TRUE),
                         encode = "json")

  return(jsonlite::fromJSON(rawToChar(response$content)))
}
