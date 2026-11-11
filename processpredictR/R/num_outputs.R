#' Calculate number of outputs (target variables)
#'
#' @param processed_df A processed dataset of class [`ppred_examples_df`].
#' @return an `integer` number of outputs for supplying as an argument to a Transformer model, i.e. number of unique labels for a specific process monitoring task.
#' @examples
#' library(processpredictR)
#' library(eventdataR)
#' df <- prepare_examples(patients)
#' num_outputs(df)
#'
#' @export
num_outputs <- function(processed_df) {

  task <- get_task(processed_df)
  processed_df[[task]] %>% unique() %>% length()

}
