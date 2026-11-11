#' Calculate the maximum length of a case / number of activities in the longest trace in an event log
#'
#' @param processed_df A processed dataset of class [`ppred_examples_df`] returned by `prepare_examples()`.
#' @return An `integer` number of the maximum case length (longest trace) in an event log.
#' @examples
#' library(processpredictR)
#' library(eventdataR)
#'
#' df <- prepare_examples(patients)
#' max_case_length(df)
#'
#'@export
max_case_length <- function(processed_df) {
  processed_df$prefix_list %>%
    purrr::map_int(length) %>%
    max()
}


#max_case_length <- function(log) {
#   UseMethod("max_case_length")
# }
#

# max_case_length.log <- function(log) {
#   log %>%
#     trace_length() %>%
#     max() %>% as.integer()
#
#   #processed_df$traces %>% lengths() %>% max()    #for processed_df as input
# }
