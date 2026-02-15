#' Record a new GUI test sequence
#'
#' Simply run 'recordNewTest()' and the test creation GUI will pop up.
#'
#' @keywords internal
#' @return None
recordNewTest <- function(){
    requireNamespace("testthat")
    requireNamespace("shinytest")

    shinytest::recordTest("inst/appTests")
}
