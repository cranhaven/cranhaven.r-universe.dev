# tidyrwl -----------------------------------------------------------------

#' @title tidy and untidy ring width data
#' @description little helper functions to convert dataframes from the data
#'   format used in multiple dendro-related R packages such as \pkg{dplR} to
#'   tidy data used in the \pkg{tidyverse} and vice versa
#' @name tidyrwl
#' @param crn a chronology as obtained from \code{\link[dplR]{chron}}
#' @param tidy_crn a tidy chronology as obtained from \code{\link{tidy_crn}}
#' @param rwl ring width data as obtained from \code{\link[dplR]{read.rwl}}
#' @param tidy_rwl tidy ring width data as obtained from \code{\link{tidy_rwl}}
#' @param value_col column name of the value column in the tidy tibble of the
#'   input resp output object
#' @importFrom dplyr %>%
#' @return data frames or tibbles
NULL
globalVariables(".")

#' @rdname tidyrwl
#' @export
tidy_crn <- function(crn) {
  crn %>% tibble::rownames_to_column(var = "year")
}

#' @rdname tidyrwl
#' @export
untidy_crn <- function(tidy_crn) {
  tmp <- as.data.frame(tidy_crn)
  rownames(tmp) <- tmp[, "year"]
  out <- tmp[, !names(tmp) %in% "year"]
  class(out) <- c("crn", "data.frame")
  out
}

#' @rdname tidyrwl
#' @export
tidy_rwl <- function(rwl, value_col = "rwl") {
  # silence R CMD check
  year <- series <- NULL
  
  rwl %>%
    tibble::rownames_to_column("year") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    tidyr::pivot_longer(cols = -year, names_to = "series",
                        values_to = value_col, values_drop_na = TRUE) %>%
    dplyr::arrange(series)
}

#' @rdname tidyrwl
#' @export
untidy_rwl <- function(tidy_rwl, value_col = "rwl") {
  out <- tidy_rwl %>%
    tidyr::pivot_wider(names_from = "series",
                       values_from = tidyselect::all_of(value_col)) %>%
    tibble::remove_rownames() %>%
    as.data.frame() %>%
    tibble::column_to_rownames("year") %>%
    .[order(as.numeric(row.names(.))), ]
  class(out) <- c("rwl", "data.frame")
  out
}
