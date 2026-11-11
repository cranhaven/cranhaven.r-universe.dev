#' Create a vocabulary
#'
#' @description Creates a vocabulary of activities and outcome labels.
#'
#' @return A [`list`] consisting of:
#' \itemize{
#'  \item {`"keys_x"`}: [`list`] of activity labels
#'  \item {`"keys_y"`}: [`list`] of outcome labels (none for tasks `"next_time"` and `"remaining_time"`)
#' }
#'
#' @inheritParams tokenize
#'
#' @export
create_vocabulary <- function(processed_df) {

  #OUTCOME
  if (get_task(processed_df) == "outcome") {
    #activity_names <- processed_df %>% pull(activity_id(processed_df)) %>% unique() %>% as.character()
    activity_names <- processed_df[[attr(processed_df, "mapping")$activity_id]] %>% as.character() %>% unique()
    activity_names <- c("PAD", "UNK") %>%
      append(activity_names)
    outcome_names <- processed_df$outcome %>% unique() %>% as.character()
    values_x <- unique(append(activity_names, outcome_names))

    #outcome tokens
    keys_y <- outcome_names %>% as.list()
    #outs <- data.frame(activity = unlist(keys_y)) %>% mutate(key_id = row_number() - 1)

    #activities tokens
    keys_x <- as.list(values_x)
    #acts <- data.frame(activity_name = keys_x %>% unlist()) %>% mutate(key_id = row_number() - 1)

    list(keys_x = keys_x, keys_y = keys_y)

  }

  #NEXT_ACTIVITY
  else if (get_task(processed_df) == "next_activity") {
    activity_names <- processed_df[[attr(processed_df, "mapping")$activity_id]] %>% as.character() %>% unique()
    activity_names <- c("PAD", "UNK") %>%
      append(activity_names)
    outcome_names <- processed_df$next_activity %>% unique()
    values_x <- unique(append(activity_names, outcome_names))

    #activities tokens
    keys_x <- as.list(values_x)
    #acts <- data.frame(activity_name = keys_x %>% unlist()) %>% mutate(key_id = row_number() - 1)

    #outcome tokens
    keys_y <- outcome_names %>% as.list()
    #outs <- data.frame(activity = unlist(keys_y)) %>% mutate(key_id = row_number() - 1)

    list(keys_x = keys_x, keys_y = keys_y)

  }

  #NEXT_TIME & REMAINING_TIME
  else if (get_task(processed_df) %in% c("next_time","remaining_time")) {
    activity_names <- processed_df[[attr(processed_df, "mapping")$activity_id]] %>% as.character() %>% unique()
    activity_names <- c("PAD", "UNK") %>%
      append(activity_names)
    values_x <- unique(activity_names)

    #activities tokens
    keys_x <- as.list(values_x)
    #acts <- data.frame(activity_name = keys_x %>% unlist()) %>% mutate(key_id = row_number() - 1)

    #acts
    list(keys_x = keys_x)

  }

  #REMAINING TRACE
  else if (get_task(processed_df) == "remaining_trace") {

    activity_names <- processed_df[[attr(processed_df, "mapping")$activity_id]] %>% as.character() %>% unique()
    activity_names <- c("PAD", "UNK") %>%
      append(activity_names)
    values_x <- unique(activity_names)

    #activities tokens
    keys_x <- as.list(values_x)
    #acts <- data.frame(activity_name = keys_x %>% unlist()) %>% mutate(key_id = row_number() - 1)

    keys_y <- processed_df$remaining_trace %>% unique() %>% as.list()
    #outs <- data.frame(activity = unlist(keys_y)) %>% mutate(key_id = row_number() - 1)

    list(keys_x = keys_x, keys_y = keys_y)
  }

}




