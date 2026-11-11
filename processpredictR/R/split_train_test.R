#' Splits the preprocessed [`data.frame`].
#'
#' @description Returns train- and test dataframes as a list.
#'
#' @param processed_df A preprocessed object of type [`ppred_examples_df`] returned by `prepare_examples()`.
#' @param split [`numeric`] (default 0.7): A train-test split ratio.
#' @return A [`list`] containing the train- and the test set objects.
#' @examples
#' library(processpredictR)
#' library(eventdataR)
#'
#' df <- prepare_examples(patients, "next_activity")
#' split_train_test(df, split = 0.8)
#'
#' @export
split_train_test <- function(processed_df, split = 0.7) { #, trace_length_bins = 5) {
  UseMethod("split_train_test")
}

#' @export
split_train_test.ppred_examples_df <- function(processed_df, split = 0.7) { #, trace_length_bins = 5) { # OR n_distinct(log[case_id(log)])

  ith_case <- NULL
  start_time <- NULL

  # if (is.null(trace_length_bins)) {

  # Arrange and pull unique cases
  unique_cases_ordered <- processed_df %>% arrange(ith_case, start_time) %>% pull(ith_case) %>% unique()
  sample_size <- n_distinct(unique_cases_ordered) * split
  train_ind <- unique_cases_ordered[1:sample_size] # id for the first n = sample_size cases for the train set

    # id's train
    #unique_cases_train <- unique_cases_ordered[train_ind] # train dataset
    train_df <-  processed_df %>% filter(ith_case %in% train_ind) #unique_cases_train)
    class(train_df) <- c("ppred_examples_df", class(train_df))

    # id's test
    test_df <- processed_df %>% filter(!(ith_case %in% train_ind))
    class(test_df) <- c("ppred_examples_df", class(test_df))

    list(train_df = train_df, test_df = test_df)

  # }

  # # x %>% mutate(trace_length = lengths(traces)) %>% mutate(trace_bucket = ntile(trace_length, max(trace_length)))
  # else {
  # #trace_length_bins [`numeric`] (default [`NULL`]): A number of trace length bins to use for stratification.
  # #If [`NULL`], does not stratify for similar trace length in both train- and test dataframes.

  #   processed_df %>%
  #     #make list of case id + trace length
  #     group_by(ith_case) %>%
  #     summarize(trace_length = max(k) + 1) %>%
  #     # make bins based on trace length
  #     mutate(bin = ntile(trace_length, trace_length_bins)) %>%
  #     # for each bin, sample the fraction of the event log according to split
  #     group_by(bin) %>%
  #     sample_frac(split) %>%
  #     # pull the case ids from these fractions
  #     pull(ith_case) -> unique_cases_train
  #
  #   train_df <-  processed_df %>% filter(ith_case %in% unique_cases_train)
  #   class(train_df) <- c("ppred_examples_df", class(train_df))
  #
  #   test_df <- processed_df %>% filter(!(ith_case %in% unique_cases_train))
  #   class(test_df) <- c("ppred_examples_df", class(test_df))
  #
  #   list(train_df = train_df, test_df = test_df)
  #
  # }


#
# unique_cases_train <- unique_cases[1:sample_size] # train dataset
# unique_cases_train <- data.frame(ith_case = unique_cases_train)
#
# unique_cases_test <- unique_cases[(sample_size+1):n_distinct(unique_cases)]
# unique_cases_test <- data.frame(ith_case = unique_cases_test)
#
# train_df <- inner_join(processed_df, unique_cases_train, by = "ith_case")
# test_df <- inner_join(processed_df, unique_cases_test, by = "ith_case")
#
#   list(train_df = train_df, test_df = test_df)

}
