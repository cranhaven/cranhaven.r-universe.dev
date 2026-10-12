#' Convert Vietnam provincial ID
#'
#' Converts Vietnam's provinces' names and ID across different formats. Handles
#' diacritics and different spellings.
#'
#' @param sourcevar Character vector that contains the codes or province names to be converted
#' @param origin String that indicates the coding scheme of origin
#' @param destination String that indicates the coding scheme of destination
#'
#' @keywords vietnamcode
#' @note Supports the following coding schemes:
#' \itemize{
#'  \item{province_name}
#'  \item{province_name_diacritics}
#'  \item{enterprise_censusm enterprise_census_old, enterprise_census_c: }{the same as GSO code}
#'  \item{pci: }{Provincial Competitiveness Index}
#' }
#'
#' @export
#' @examples
#' codes.of.origin <- vietnamcode::vietnamcode_data$province_name # Vector of values to be converted
#' vietnamcode(codes.of.origin, "province_name", "pci")
vietnamcode <- function(sourcevar,
  origin = c("province_name",
             "enterprise_census", "enterprise_census_old", "enterprise_census_c",
             "pci"),
  destination = c("province_name", "province_name_diacritics",
                "enterprise_census", "enterprise_census_old", "enterprise_census_c",
                "pci")) {
  if (is.null(sourcevar))
    stop("sourcevar cannot be NULL")

  origin <- match.arg(origin)
  destination <- match.arg(destination)

  # Sanitize province name to lower case and ASCII
  if (origin %in% c("province_name")) {
    sourcevar <- tolower(convert_unicode_to_ascii(sourcevar))

    # Multiple regex search
    tmp <- sapply(stats::na.omit(vietnamcode::vietnamcode_data[["regex"]]), grepl,
           unique(sourcevar), ignore.case = TRUE, perl = TRUE)
    if (is.vector(tmp)) {
      regex_index = which_or_NA(tmp)
    } else if (is.matrix(tmp)) {
      regex_index = apply(tmp, 1, which_or_NA)
    }

    match_table <- data.frame(source = unique(sourcevar),
                              regex_index = regex_index)

    destination_index <- match_table[match(sourcevar, match_table[["source"]]),
                                     "regex_index"]
  } else {
    destination_index <- match(sourcevar, vietnamcode::vietnamcode_data[[origin]])
  }

  return(vietnamcode::vietnamcode_data[destination_index, destination])
}
