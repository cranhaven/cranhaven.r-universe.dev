#' Check genotype file
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Checks to ensure the content and structure are appropriate for a genotype
#' file. These checks are simply based on expected columns and legal domains.
#'
#' @return A genotype file that has been checked to ensure the column types and
#' number required are present. The returned genotype file has the first column
#' name forced to "id".
#'
#' @param genotype dataframe with genotype data
#' @importFrom stringi stri_c stri_detect_fixed stri_detect_regex
#' @export
#' @examples
#' library(nprcgenekeepr)
#' ped <- nprcgenekeepr::qcPed
#' ped <- ped[order(ped$id), ]
#' genotype <- data.frame(
#'   id = ped$id[50 + 1:20],
#'   first_name = paste0("first_name", 1:20),
#'   second_name = paste0("second_name", 1:20),
#'   stringsAsFactors = FALSE
#' )
#'
#' ## checkGenotypeFile disallows dataframe with < 3 columns
#' tryCatch(
#'   {
#'     checkGenotypeFile(genotype[, c("id", "first_name")])
#'   },
#'   warning = function(w) {
#'     cat("Warning produced")
#'   },
#'   error = function(e) {
#'     cat("Error produced")
#'   }
#' )
checkGenotypeFile <- function(genotype) {
  cols <- names(genotype)
  if (length(cols) < 3L) {
    stop("Genotype file must have at least three columns.")
  } else if (!stri_detect_fixed(tolower(cols[1L]), "id")) {
    stop("Genotype file must have 'id' as the first column.")
  } else if (any(tolower(cols) %in% c("first", "second"))) {
    stop("Genotype file cannot have a column named 'first' or 'second'.")
  } else {
    for (i in 2L:3L) {
      alleles <- unique(genotype[, i][!is.na(genotype[, i])])
      numbers <- suppressWarnings(as.integer(alleles))
      numbers <- numbers[!is.na(numbers)]
      if (any(numbers > 10000L)) {
        numberStr <- stri_c(format(numbers[numbers > 10000L],
          scientific = FALSE
        ), sep = ", ")
        stop(stri_c("Possible collision on allele(s) interpreted as a number
                    > 10000: ", numberStr, collapse = ", "))
      }
      # Anything goes
      # if (any(stri_detect_regex(alleles, "[;:\"']+"))) {
      #   stop(stri_c("Alleles have one or more of the following characters,
      #               which are not currently supported: ", ";:\"'"))
      # }
    }
  }
  names(genotype) <- c("id", cols[2L:length(cols)])
  genotype
}
