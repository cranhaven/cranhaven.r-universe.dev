#' Augmented data frame filtering.
#'
#' @description
#' Imagine \code{dplyr::\link[dplyr]{filter}} that includes neighboring observations.
#' Choose how many observations to include by adjusting inputs \code{sift.col} and \code{scope}.
#'
#' @details
#' \code{sift()} can be understood as a 2-step process:
#'
#' \enumerate{
#'   \item \code{.data} is passed to \code{dplyr::\link[dplyr]{filter}}, using subsetting expression(s) provided in \code{...}. We'll refer to these intermediate results as "key" observations.
#'   \item For each key observation, \code{sift} expands the row selection bidirectionally along dimension specified by \code{sift.col}. Any row from the original dataset within \code{scope} units of a key observation is captured in the final result.
#' }
#'
#' Essentially, this allows us to "peek" at neighboring rows surrounding the key observations.
#'
#' @param .data A data frame.
#' @param sift.col Column name, as symbol, to serve as "sifting/augmenting" dimension. Must be non-missing and coercible to numeric.
#' @param scope Specifies augmentation bandwidth relative to "key" observations. Parameter should share the same scale as \code{sift.col}.
#'
#' If length 1, bandwidth used is +/- \code{scope}.
#'
#' If length 2, bandwidth used is (-\code{scope[1]}, +\code{scope[2]}).
#'
#' @param ... Expressions passed to \code{dplyr::\link[dplyr]{filter}}, of which the results serve as the "key" observations. The same data-masking rules used in \code{dplyr::\link[dplyr]{filter}} apply here.
#'
#' @return
#' A sifted data frame, with 2 additional columns:
#' \itemize{
#'   \item \code{.cluster <int>}: Identifies resulting group formed by each key observation and its neighboring rows.  When the key observations are close enough together, the clusters will overlap.
#'   \item \code{.key <lgl>}: \code{TRUE} indicates key observation.
#' }
#'
#' @export
#' @import dplyr
#'
#' @examples
#' # See current events from same timeframe as 2020 Utah Monolith discovery.
#' sift(nyt2020, pub_date, scope = 2, grepl("Monolith", headline))
#'
#' # or Biden's presidential victory.
#' sift(nyt2020, pub_date, scope = 2, grepl("Biden is elected", headline))
#'
#' # We can specify lower & upper scope to see what happened AFTER Trump tested positive.
#' sift(nyt2020, pub_date, scope = c(0, 2), grepl("Trump Tests Positive", headline))
#'
#' # sift recognizes dplyr group specification.
#' library(dplyr)
#' library(mopac)
#' express %>%
#'  group_by(direction) %>%
#'  sift(time, 30, plate == "EAS-1671") # row augmentation performed within groups.
sift <- function(.data, sift.col, scope, ...) {
  UseMethod("sift")
}

#' @export
sift.data.frame <- function(.data, sift.col, scope, ...) {

  x1 <- double()
  x2 <- double()
  .key = NA

  # Coerce sift.col to numeric
  if(rlang::is_missing(rlang::enexpr(sift.col))) stop("Must supply sift.col (variable to sift along)")

  tryCatch(
    warning = function(cnd) {
      stop("input sift.col must be coercible to <numeric>")
    },
    x <- as.numeric(pull(.data, {{sift.col}}))
  )

  # verify scope
  if(rlang::is_missing(rlang::enexpr(scope))) stop("Must supply scope amount")

  tryCatch(
    warning = function(cnd) {
      stop("input scope must be coercible to <numeric>")
    },
    scope <- as.numeric(scope)
  )

  if (length(scope) == 1) {
    scope[2] <- scope[1]
  }

  scope <- scope[1:2]

  if (anyNA(scope)) stop("input scope must not contain missing values")

  # check ... (dplyr::filter will detect invalid expr)
  if(missing(...)) {
    message("No logical expressions supplied.")
    return(.data %>% mutate(.cluster = NA_integer_, .key = NA))
  }

  .siftIndex <- seq_len(nrow(.data))

  loc <- .data %>%
    tibble::add_column(.siftIndex = .siftIndex) %>%
    ungroup() %>%
    filter(!!!rlang::exprs(...)) %>%
    pull(.siftIndex)

  loc <- .siftIndex %in% loc

  if(rlang::is_empty(x[loc]) | all(is.na(x[loc]))) {
    return(filter(.data, FALSE) %>% mutate(.cluster = integer(), .key = logical()))
  }

  .data[[".key"]] <- loc

  gi <- group_indices(.data)

  df <- tibble(loc, gi, x, i = seq_along(x)) %>%
    arrange(x) %>%
    group_by(gi) %>%
    mutate(x1 = if_else(loc, x - scope[1], NA_real_),
           x2 = if_else(loc, x + scope[2], NA_real_)) %>%
    # below is opportunity for future Rcpp optimization
    tidyr::fill(x1, .direction = "up") %>%
    tidyr::fill(x2, .direction = "down") %>%
    filter(x > x1 | x < x2)

  dplyr_row_slice(.data, df$i) %>%
    mutate(.cluster = kluster({{sift.col}}, bw = max(scope), fixed = TRUE), .before = .key)
}
