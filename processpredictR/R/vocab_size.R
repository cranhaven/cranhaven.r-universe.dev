#' Calculate the vocabulary size, i.e. the sum of number of activities, outcome labels and padding keys
#'
#' @param processed_df A processed dataset of class [`ppred_examples_df`] from `prepare_examples()`.
#' @return an `integer` number of vocabulary size to define the Transformer model.
#' @examples
#' library(processpredictR)
#' library(eventdataR)
#' df <- prepare_examples(patients)
#' vocab_size(df)
#'
#'@export
vocab_size <- function(processed_df) {

  vocabulary <- get_vocabulary(processed_df)
  vocabulary$keys_x %>% length() %>% as.integer()

  # else if (task == "remaining_trace") {
  #   activity_names <- processed_df[[bupaR::activity_id(processed_df)]] %>% as.character() %>% unique()
  #   activity_names <- c("PAD", "UNK") %>%
  #     append(activity_names)
  #   length(unique(activity_names)) %>% as.integer()

}
