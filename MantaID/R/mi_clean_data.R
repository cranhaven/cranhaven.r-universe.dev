#' Reshape data and delete meaningless rows.
#'
#' @param data A dataframe or tibble or data.table or matrix. Names of the column will be regard as the class of ID included in column.
#' @param placeholder Character vectors. IDs included in `placeholder` will be omitted.
#' @param cols Character vectors. Columns of `data` that contain the IDs
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr select mutate_at pull slice
#' @importFrom tidyselect everything
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map
#' @importFrom magrittr %<>%
#' @export
#' @return A tibble with two columns("ID" and "class")
#' @examples
#' data <- tibble::tibble(
#' "class1" = c("A","B","C","D"),
#' 'class2' = c("E","F","G","H"),
#' 'class3' = c("L","M","-","O")
#' )
#' mi_clean_data(data)
mi_clean_data <- function(data, cols = everything(), placeholder = c("-")) {
  data %<>% as_tibble() %>%
    select(cols) %>%
    mutate_at(colnames(.), ~ as.character(.x)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "class",
      values_to = "ID",
      names_repair = "minimal"
    ) %>%
    select("ID", "class")
  index <- map(data %>% pull("ID"), ~ !.x %in% placeholder)
  data %>% slice(which(index == TRUE))
}