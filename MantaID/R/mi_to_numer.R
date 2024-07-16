#' Convert data to numeric, and for ID column convert with fixed levels.
#' @export
#' @importFrom  dplyr across mutate
#' @param data A tibble with n position column(pos1,pos2,...) and class column.
#' @param levels Characters accommodated in IDs.
#' @return A numeric data frame with numerical or factor type columns.
#' @examples
#' data(mi_data_procID)
#' mi_to_numer(mi_data_procID)
mi_to_numer <- function(data, levels = c("*", 0:9, letters, LETTERS, "_", ".", "-", " ", "/", "\\", ":")) {
  data %>%
    mutate(across(.cols = -"class", .fns = ~ factor(.x, levels = levels))) %>%
    mutate(across(.cols = "class", .fns = factor)) %>%
    mutate(across(.cols = -"class", .fns = as.numeric))
}
