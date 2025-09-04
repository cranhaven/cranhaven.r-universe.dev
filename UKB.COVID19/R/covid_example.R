#' Provide working directory for UKB.COVID19 example files
#'
#' @param path path to file
#'
#' @examples
#' covid_example('results/covariate.txt')
#'
#' @return Outputs the working directory for UKB.COVID19 example files.
#' 
#' @export 

covid_example <- function(path) {
  system.file("extdata", path, package = "UKB.COVID19", mustWork = TRUE)
}
