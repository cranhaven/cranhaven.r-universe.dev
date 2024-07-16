#' Predict new data with trained learner.
#' @param data A dataframe.
#'
#' @param learner A R6 class object.
#' @importFrom dplyr rename mutate across select bind_cols
#' @importFrom tidyselect everything
#' @importFrom data.table as.data.table
#' @return A data frame that contains features and 'predict' class.
#' @export
mi_predict_new <- function(data, learner) {
  data %>%
    mutate(across(.cols = everything(), .fns = as.numeric)) %>%
    learner$predict_newdata(newdata = ., task = NULL) # %>%
  as.data.table() %>%
    bind_cols(data, .) %>%
    select(-"truth") %>%
    select("response", everything()) %>%
    rename("response" = learner$id)
}
