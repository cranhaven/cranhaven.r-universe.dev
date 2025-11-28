#' Adjust P-values
#'
#' Adjust P-values of a \code{\link{tidy_summary}} object
#'
#' @param x a \code{\link{tidy_summary}} object.
#' @param method (chr, default = "BH") a valid method for
#'     \code{\link[stats]{p.adjust}}
#'
#' @return a \code{\link{tidy_summary}} object with the Ps adjusted
#' @export
#'
#' @examples
#' \donttest{
#'   library(Hmisc)
#'   my_summary <- summary(Species ~ ., data = iris,
#'     method = "reverse",
#'     test = TRUE
#'   )
#'
#'   tidy_summary(my_summary, prtest = "P") %>%
#'     adjust_p()
#' }
adjust_p <- function(x, method) {
  UseMethod("adjust_p")
}












#' @rdname adjust_p
#' @export
adjust_p.tidy_summary <- function(x, method = "BH") {
  if (is.null(x[["P-value"]])) {
    ui_oops("
      The object {ui_code('x')} does not have a {ui_field('P-value')} column.
      Have you select {ui_code('test = TRUE')} into {ui_code('summary()')}
      and set argument {ui_code('prtest = \"P\"')} into the
      {ui_code('print()')}, the {ui_code('summary_interact()')}, or
      the {ui_code('tidy_summary()')} function?
    ")
    ui_oops("{ui_code('x')} is returned without changes.")
    return(x)
  }

  adj_methods <- c(
    "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr",
    "none"
  )
  if (!method %in% adj_methods) {
    ui_stop(
      "method selected is {ui_value(method)}.
    It must be one of {ui_value(adj_methods)}.
    Please, provide a valid method."
    )
  }


  # The first one is not empty because it is the header
  are_ps <- x[["P-value"]] %>%
    stringr::str_detect("^ +$", negate = TRUE) %>%
    `[<-`(1L, FALSE)

  ps <- x[["P-value"]] %>%
    stringr::str_replace("<", "") %>%
    `[`(are_ps) %>%
    as.numeric()

  # (Can someone find an alternative method to conclude "<0.001"
  # maintaining consistency round(3L) for the other values, and with the
  # further padding that comes after? If so, please purpose it! :-)
  ps_adj <- stats::p.adjust(ps, method = method) %>% round(3L)
  ps_adj[ps_adj == 0.001] <- "<=0.001"

  # returned string-values must conserve the original lenght
  nchar_ps <- nchar(x[["P-value"]][[1L]])
  ps_adj <- stringr::str_pad(ps_adj, nchar_ps)

  x[["P-value"]][are_ps] <- ps_adj

  ui_done("P adjusted with {method} method.")
  x
}
