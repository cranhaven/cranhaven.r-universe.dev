#' Observe the distribution of the false response of test set.
#' @param predict A R6 class `PredictionClassif`.
#' @importFrom data.table as.data.table
#' @importFrom dplyr filter group_by count
#' @importFrom magrittr %>% %T>%
#' @export
#' @return A tibble data frame that records the number of wrong predictions for each category ID;
mi_get_miss <- function(predict) {
  stopifnot(any(class(predict) == "PredictionClassif"))
  result <- predict %>%
    as.data.table() %>%
    filter("truth" != "response") %>%
    group_by("truth") %>%
    count()
}
