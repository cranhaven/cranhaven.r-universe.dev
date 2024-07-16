#' Split the string into individual characters and complete the character vector to the maximum length.
#'
#' @param str The string to be splited.
#' @param pad_len The length of longest id, i.e. the maxlength.
#' @export
#' @return Splited character vector.
#' @examples
#' string_test = "Good Job"
#' length = 15
#' mi_split_str(string_test,length)
mi_split_str <- function(str, pad_len) {
  str %>%
    as.character() %>%
    strsplit(split = "") %>%
    unlist() %>%
    c(., rep("*", pad_len - length(.)))
}