#' Path to Example Data
#'
#' @return Local path to example text file, containing user stories for Operational
#'     Research mathmatical models.
#'
#' @export
#'
#' @family Discogs data and functions
#'
#' @examples
#' example_stories()
example_stories <- function() {
  system.file("extdata", "RSC-RefinedStories.txt", package = "oRus")
}
