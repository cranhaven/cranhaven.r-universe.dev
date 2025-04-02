# This is here instead of in testthat because `manage_cases` doesn't
# seem to load testthat.R
expect_doppelganger <- function(title, fig, path = NULL, ...) {
    testthat::skip_if_not_installed("vdiffr")
    vdiffr::expect_doppelganger(title, fig, path = path, ...)
}

Sys.setlocale("LC_COLLATE", "C") ## What CRAN does; affects sort order
set.seed(999) ## To ensure that tests that involve randomness are reproducible

"%>%" <- magrittr::`%>%`

crplyr:::.onLoad()

with_POST_crplyr <- function(resp, expr) {
    ## Mock a POST that returns something, like a Location header pulled from 201
    force(resp)
    with_mocked_bindings(crPOST = function(...) resp, eval.parent(expr), .package = "crunch")
}
