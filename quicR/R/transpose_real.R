#' Transpose Real-Time Data
#'
#' Transposes the real-time data table exported by the BMG software. Accepts
#' output from the function, "get_real".
#'
#' @param data A dataframe generated from get_real.
#'
#' @return A transposed dataframe containing real-time normalized fluorescence values.
#'
#' @importFrom dplyr rename
#' @importFrom dplyr mutate_at
#' @importFrom dplyr %>%
#' @importFrom janitor row_to_names
#'
#' @export

# Still in the works
transpose_real <- function(data) {
  colnames(data) %>%
    rbind(data) %>%
    unname() %>%
    t() %>%
    as.data.frame() %>%
    row_to_names(1) %>%
    rename("Sample IDs" = "Time") %>%
    mutate_at(-c(1), function(y) as.numeric(as.character(y)))
}
