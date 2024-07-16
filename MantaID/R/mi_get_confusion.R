#' Compute the confusion matrix for the predict result.
#'
#' @param ifnet Logical.Whether the data is obtained by a deep learning model.
#' @param result_list A list return from model training functions.
#'
#' @importFrom data.table as.data.table
#' @importFrom dplyr select
#' @importFrom caret confusionMatrix
#' @return A `confusionMatrix` object.
#' @export
mi_get_confusion <- function(result_list, ifnet = FALSE) {
  if (ifnet) {
    return(result_list[[2]])
  }
  matri_tr <- result_list[[2]] %>%
    as.data.table() %>%
    select(-1)
  confusionMatrix(matri_tr %>% pull(2), matri_tr %>% pull(1))
}
