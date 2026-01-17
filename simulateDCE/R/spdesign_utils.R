#' Title Extracts beta values from an spdesign object
#'
#' @param input_list the list where the parameters are stored. Usually this is `design$utility`
#'
#' @return A named list with parameter values which can be used in `sim_all`
#' @export
#'
#' @examples
#' d <- system.file("extdata", "CSA", "linear", "BLIeff.RDS", package = "simulateDCE")
#' extract_b_values(readRDS(d)$utility)
#'
extract_b_values <- function(input_list) {
  extract_b_value <- function(input_string) {
    matches <- gregexpr("b_\\w+\\[(-?\\d+\\.?\\d*)\\]", input_string)
    matches <- regmatches(input_string, matches)
    matches <- unlist(matches)


    names <- gsub("\\[.*", "", matches)

    values <- gsub(".*\\[(-?\\d+\\.?\\d*)\\]", "\\1", matches)

    result <- stats::setNames(as.numeric(values), names)
    return(result)
  }

  b_values <- unlist(lapply(input_list, extract_b_value))
  b_values <- stats::setNames(b_values, gsub("alt\\d+\\.", "", names(b_values)))
  return(as.list(b_values))
}
