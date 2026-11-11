#' @title Convert a dataset of type [`log`] into a preprocessed format.
#'
#' @description an event log is converted into a tibble where each row contains a cumulative sequence of activities per case.
#' This sequence will eventually be feeded to the Transformer model's token embedding layer.
#'
#' @param log \code{\link{log}}: Object of class \code{\link{log}} or derivatives (grouped_log, eventlog,
#' activitylog, etc.).
#' @param task [`character`]: a process monitoring task for which to prepare an event log.
#' @param features [`character`] (default [`NULL`]): additional features. Appends attributes (if present) numeric_features and/or categorical_features to a preprocessed event log.
#' @param ... additional arguments.
#'
#' @return a preprocessed dataset of class [`ppred_examples_df`].
#' @examples
#' library(processpredictR)
#' library(eventdataR)
#'
#' prepare_examples(patients, "next_activity")
#'
#'
#' @export
#'
prepare_examples <- function(log, task = c("outcome", "next_activity",
                                              "next_time", "remaining_time",
                                              "remaining_trace"), features = NULL, ...) {
  UseMethod("prepare_examples")

}

#' @export
prepare_examples.eventlog <- function(log, task = c("outcome", "next_activity",
                                                       "next_time", "remaining_time",
                                                       "remaining_trace"), features = NULL, ...) {

  AIID <- NULL
  start <- NULL
  .order <- NULL
  AID <- NULL
  . <- NULL
  TS <- NULL
  CID <- NULL
  RID <- NULL
  start_time <- NULL
  end_time <- NULL
  min_order <- NULL
  outcome <- NULL
  time_before_activity <- NULL
  time_till_next_activity <- NULL
  previous_duration <- NULL
  remaining_trace_list <- NULL
  remaining_trace <- NULL
  prefix_list <- NULL
  ith_case <- NULL
  prefix <- NULL
  k <- NULL
  RID <- NULL

  task <- rlang::arg_match(task)


  m <- mapping(log)

  log %>%
    select(AIID = activity_instance_id(log), any_of(features), force_df = T) %>%
    distinct() -> feature_data

  log_dt <- log %>%
    rename(AID = activity_id(log),
           CID = case_id(log),
           RID = resource_id(log),
           AIID = activity_instance_id(log),
           TS = timestamp(log)) %>%
    as.data.table() %>%
    mutate(AID = as.character(AID))

  log_dt[,
         .('start_time' = min(TS),
           'end_time' = max(TS),
           'min_order' = min(.order)),
         by = .(AID, AIID, CID, RID)] %>%
    prepare_examples_main(m, task, features, feature_data, ...)

}

#' @export
prepare_examples.activitylog <- function(log, task = c("outcome", "next_activity",
                                                          "next_time", "remaining_time",
                                                          "remaining_trace"), features = NULL, ...) {

  AIID <- NULL
  start <- NULL
  .order <- NULL
  AID <- NULL
  . <- NULL
  TS <- NULL
  CID <- NULL
  RID <- NULL
  start_time <- NULL
  end_time <- NULL
  min_order <- NULL
  outcome <- NULL
  time_before_activity <- NULL
  time_till_next_activity <- NULL
  previous_duration <- NULL
  remaining_trace_list <- NULL
  remaining_trace <- NULL
  prefix_list <- NULL
  ith_case <- NULL
  prefix <- NULL
  k <- NULL
  RID <- NULL

  task <- rlang::arg_match(task)

  log %>%
    mutate(AIID = 1:n()) -> log

  m <- mapping(log)

  log %>%
    as_tibble() %>%
    select(AIID, any_of(features)) -> feature_data

  log_dt <- log %>%
    as_tibble() %>%
    select(AIID,
           AID = activity_id(log),
           CID = case_id(log),
           RID = resource_id(log),
           start_time = start,
           end_time = complete,
           min_order = .order) %>%
    as.data.table() %>%
    mutate(AID = as.character(AID)) %>%
    prepare_examples_main(m, task, features, feature_data, ...)
}


prepare_examples_main <- function(log, mapping, task, features, feature_data, ...) {

  AIID <- NULL
  start <- NULL
  .order <- NULL
  AID <- NULL
  . <- NULL
  TS <- NULL
  CID <- NULL
  RID <- NULL
  start_time <- NULL
  end_time <- NULL
  min_order <- NULL
  outcome <- NULL
  time_before_activity <- NULL
  time_till_next_activity <- NULL
  previous_duration <- NULL
  remaining_trace_list <- NULL
  remaining_trace <- NULL
  prefix_list <- NULL
  ith_case <- NULL
  prefix <- NULL
  k <- NULL
  RID <- NULL

  log[order(start_time, end_time, min_order),
   c("ith_case", "k","prefix_list", "remaining_trace_list") := .(
     .GRP,
     (1:.N)-1,
     Reduce(c, AID, accumulate = TRUE),
     shift(Reduce(c, AID, accumulate = TRUE, right = TRUE), fill = "endpoint", n = -1)
   ),
   .(CID)] -> log

  if(task == "outcome") {

    log[, c("outcome") := .(last(AID), ...), by = .(CID)] %>%
      mutate(outcome = forcats::fct_collapse(outcome, ...)) -> log

    standard_features <- character()

  } else if(task == "next_activity") {

    log[, c("next_activity") := .(shift(AID, n = -1, fill = "endpoint")), .(CID)] -> log

    standard_features <- character()
  } else if(task == "next_time") {

    log[order(start_time, end_time, min_order),
               c("latest_duration",
                 "throughput_time",
                 "processing_time",
                 "time_before_activity",
                 "time_till_next_activity") := .(
                   as.double(end_time - start_time, units = "secs"),
                   as.double(end_time - min(start_time), units = "secs"),
                   cumsum(as.double(end_time - start_time, units = "secs")),
                   as.double(start_time - shift(end_time), units = "secs"),
                   as.double(shift(start_time, n = -1) - end_time, units = "secs")
                 ),
               .(CID)][,c("time_before_activity") := .(if_else(is.na(time_before_activity), 0, time_before_activity))] %>%
      filter(!is.na(time_till_next_activity)) -> log

    standard_features <- c("latest_duration","throughput_time","processing_time","time_before_activity")

  } else if(task == "remaining_time") {

    log[order(start_time, end_time, min_order),
               c("previous_duration",
                 "throughput_time",
                 "processing_time",
                 "remaining_time") := .(
                   shift(as.double(end_time - start_time, units = "secs")),
                   as.double(end_time - min(start_time), units = "secs"),
                   cumsum(as.double(end_time - start_time, units = "secs")),
                   as.double(last(end_time) - end_time, units = "secs")
                 ),
               .(CID)][,c("previous_duration") := .(if_else(is.na(previous_duration), 0, previous_duration))] -> log

    standard_features <- c("previous_duration","throughput_time","processing_time")

  } else if(task == "remaining_trace") {

    # new_log_dt[order(start_time, end_time, min_order),
    #            c("remaining_trace_list") := Reduce(c, AID, accumulate = TRUE, right = TRUE), .(CID)] %>%
    log %>%
      mutate(remaining_trace = map_chr(remaining_trace_list, paste, collapse = " - "),
             remaining_trace = if_else(is.na(remaining_trace), "endpoint", remaining_trace)) -> log

    standard_features <- character()

  }

  y_var <- switch(task,
                  outcome = "outcome",
                  next_activity = "next_activity",
                  next_time = "time_till_next_activity",
                  remaining_time = "remaining_time",
                  remaining_trace = "remaining_trace")



  log %>%
    merge(feature_data, by = "AIID") %>%
    as_tibble() %>%
    mutate(prefix = map_chr(prefix_list, paste, collapse = " - ")) %>%
    select(ith_case, !!sym(mapping$case_id) := CID, prefix, prefix_list, any_of(c(y_var, standard_features, setdiff(features, unlist(mapping)))), k,
           !!sym(mapping$activity_id) := AID, !!sym(mapping$resource_id) := RID,
           start_time, end_time, remaining_trace_list) %>%
    as_tibble() -> output

  output %>%
    arrange(ith_case, k) -> output

  class(output) <- c("ppred_examples_df", class(output))

  attr(output, "task") <- task
  attr(output, "y_var") <- y_var
  attr(output, "features") <- c(standard_features, features)
  attr(output, "mapping") <- mapping
  attr(output, "max_case_length") <- max_case_length(output)
  vocabulary <- create_vocabulary(output)
  if (task %in% c("outcome", "next_activity", "remaining_trace")) num_outputs <- output[[task]] %>% unique() %>% length()
  else num_outputs <- 1
  attr(output, "num_outputs") <- num_outputs %>% as.integer()
  attr(output, "vocab_size") <- vocabulary$keys_x %>% length() %>% as.integer()
  attr(output, "vocabulary") <- vocabulary

  if (!is.null(attr(output, "features"))) {
    output <- hot_encode_feats(output)
  }
  return(output)
}
