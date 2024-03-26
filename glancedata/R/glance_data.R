## -------------------------------------------------------------------

check_type <- function(x) {
  ## Types to check for.
  out <-
    c(all(is.na(x)),
      is.numeric(x),
      is.logical(x),
      is.factor(x),
      is.character(x))

  ## Index of type
  out <- which.max(out)

  ## Check column type is defined. "5" is the number if specifies
  ## types.
  if (!(out %in% 1:5)) stop("Column type not recognized.")

  ## Return the column type.
  switch(out,
         "NA only",
         "numerical",
         "logical",
         "factor",
         "categorical")
}

summarize_num_vector <- function(x, fun, ...) {
  if (!(is.numeric(x) | is.logical(x))) return (NA)
  if (all(is.na(x))) return (NA)
  fun(x, ...)
}


##' @importFrom dplyr %>% transmute
##' @importFrom forcats fct_count
print_tally <- function(x, n = 5) {
  levels <- unique(x)
  n_levels <- length(levels)

  f <- "ignoreme"

  ## if (is.na(n_levels)) return (NA)
  if (n_levels > n) return("Too many unique values")

  ## Tally factor levels
  out <- x %>%
      as.character() %>%
      fct_count()

  ## Create column with counts
  out <- transmute(out, count = paste0(f, ": ", n))$count

  ## Return a single string with tallied values separated by commas.
  paste0(out, collapse = ", ")
}

count_distinc_values <- function (x) {
  distinct_values <- length(unique(x))
  na_present <- any(is.na(x))
  distinct_values - na_present
}


##' Glance Data
##'
##' Provides a summary of data with the the following columns:
##' \describe{
##' \item{\code{name}}{Name of the column.}
##' \item{\code{type}}{Type of the column, equal to "numerical",
##' "logical", "factor", "categorical", or "NA only".}
##' \item{\code{distinct_values}}{Count of distinct values. It ignores
##' NA values. Thus, if a columns only has NAs, then the value of this
##' field will be zero.}
##' \item{\code{minimum}}{Minimum of numerical columns excluding NA
##' values.}
##' \item{\code{median}}{Median of numerical columns excluding NA
##' values.}
##' \item{\code{maximum}}{Maximum of numerical columns excluding NA
##' values.}
##' \item{\code{mean}}{Mean of numerical variables. It ignores NAs.}
##' \item{\code{sd}}{Standard deviation of numerical variables. It
##' ignores NAs.}
##' \item{\code{na_proportion}}{Proportion of NAs.}
##' \item{\code{count}}{Tally of values if the column has 5 values at
##' most. This value (5) can be modified with the parameter
##' \code{limit2tally}.}
##' \item{\code{sample_values}}{Sample of (different) values in each
##' column.}
##' }
##'
##' @param x A dataframe with named columns.
##' @param limit2tally One of the summaries is a tally of the distinct
##'     values on each column. If there are too many different values
##'     in a column, this summary would be meaningless. This
##'     \code{limit2tally} is the limit of distinct values to
##'     tally. If there are more than that it returns
##'     "Too many unique values".
##' @return A \code{tibble}.
##' @importFrom tibble tibble
##' @importFrom purrr map_chr map_dbl map_int
##' @importFrom stats median sd
##' @importFrom dplyr %>%
##' @importFrom utils head
##' @examples
##' glance_data(iris)
##' @author Guillermo Basulto-Elias
##' @export
glance_data <- function(x, limit2tally = 20) {
    x <- as.list(x)

    tibble(
        name = names(x),
        type = map_chr(x, check_type),
        distinct_values = map_int(x, count_distinc_values),
        minimum =
            map_dbl(x, summarize_num_vector, min, na.rm = TRUE),
        median =
            map_dbl(x, summarize_num_vector,
                    median, na.rm = TRUE),
        maximum =
            map_dbl(x, summarize_num_vector, max, na.rm = TRUE),
        mean =
            map_dbl(x, summarize_num_vector, mean, na.rm = TRUE),
        sd =
            map_dbl(x, summarize_num_vector, sd, na.rm = TRUE),
        na_proportion =
            map_dbl(x, ~ mean(is.na(.x))),
        count = map_chr(x, print_tally,
                        n = limit2tally),
        sample_values =
            map_chr(x,
                    function(y) {
                        y %>%
                            unique() %>%
                            head() %>%
                            paste(collapse = ", ")
                    })
    )
}

