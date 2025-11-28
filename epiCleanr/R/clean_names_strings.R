#' Clean variable names or column names in various styles
#'
#' This function transforms variable names or column names into one of the
#' standard cleaned formats specified by the `style` argument. It offers more
#' flexibility than \code{\link[=janitor]{janitor::clean_names()}}
#' function by supporting individual strings and providing multiple naming
#' styles.
#'
#' @param input A data frame, tibble, matrix, list, or character vector
#'        representing the names to be cleaned.
#' @param style A character string specifying the naming style to use.
#'        Available options are "snake_case" (default), "camel_case", and
#'        "simple_clean".
#'
#' @return The object with cleaned names.
#'
#' @examples
#'
#' library(data.table)
#' library(zoo)
#' library(xts)
#'
#' # For data frame with snake_case (default)
#' data("iris")
#' cleaned_iris <- clean_names_strings(iris)
#' colnames(cleaned_iris)
#'
#' # For data frame with camel_case
#' cleaned_iris_camel <- clean_names_strings(iris, style = "camel_case")
#' colnames(cleaned_iris_camel)
#'
#' # For character vector
#' original_names <- c("Some Column", "Another-Column!", "Yet Another Column")
#' cleaned_names <- clean_names_strings(original_names, style = "simple_clean")
#' print(cleaned_names)
#'
#' # For matrix
#' mat <- matrix(1:4, ncol = 2)
#' colnames(mat) <- c("Some Column", "Another Column")
#' cleaned_mat <- clean_names_strings(mat)
#' colnames(cleaned_mat)
#'
#' # For list
#' lst <- list("Some Column" = 1, "Another Column" = 2)
#' cleaned_lst <- clean_names_strings(lst)
#' names(cleaned_lst)
#'
#' # For xts object
#' xts_obj <- xts(x = matrix(1:4, ncol = 2),
#'                order.by = as.Date('2021-01-01') + 0:1)
#' colnames(xts_obj) <- c("Some Column", "Another Column")
#' cleaned_xts <- clean_names_strings(xts_obj)
#' print(colnames(cleaned_xts))
#'
#  # For zoo object
#' zoo_obj <- zoo(matrix(1:4, ncol = 2), order.by = 1:2)
#' colnames(zoo_obj) <- c("Some Column", "Another Column")
#' cleaned_zoo <- clean_names_strings(zoo_obj)
#' print(colnames(cleaned_zoo))
#'
#' # for Data table
#' dt <- data.table("Some Column" = 1:2, "Another Column" = 3:4)
#' cleaned_dt <- clean_names_strings(dt)
#' print(names(cleaned_dt))
#'
#' @seealso \code{\link[=janitor]{janitor::clean_names()}}
#' @importFrom purrr map_chr
#' @importFrom tibble is_tibble
#' @importFrom janitor clean_names
#'
#' @export

clean_names_strings <- function(input, style = "snake_case") {

  snake_case <- function(var) {
    var <- tolower(var)
    var <- gsub("[[:space:]|[:punct:]]+", "_", var)
    var <- gsub("([a-z])([A-Z])", "\\1_\\2", var)
    gsub("^_|_$", "", var)
  }

  camel_case <- function(var) {
    var <- gsub("[[:space:]|[:punct:]]+", "", var)
    R.utils::toCamelCase(var)
  }

  simple_clean <- function(var) {
    var <- ifelse(is.na(var), NA, stringr::str_remove_all(var, "\\W"))
    var <- tolower(var)
    var <- stringi::stri_trans_general(var, "latin-ascii")
    stringr::str_remove_all(var, "[[:punct:][:space:]]")
  }

  clean_fn <- switch(style,
                     "snake_case" = snake_case,
                     "camel_case" = camel_case,
                     "simple_clean" = simple_clean,
                     stop("Invalid style option. Choose 'snake_case',
                          'camel_case', or 'simple_clean'."))

  if (is.data.frame(input) || tibble::is_tibble(input)) {
    cleaned_colnames <- purrr::map_chr(names(input), clean_fn)
    names(input) <- cleaned_colnames
  } else if (is.character(input)) {
    return(purrr::map_chr(input, clean_fn))
  } else if (is.matrix(input)) {
    colnames(input) <- purrr::map_chr(colnames(input), clean_fn)
  } else if (is.list(input)) {
    names(input) <- purrr::map_chr(names(input), clean_fn)
  } else {
    stop("Unsupported input type.")
  }

  return(input)
}
