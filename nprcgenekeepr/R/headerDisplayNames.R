#' Convert internal column names to display or header names.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Converts the column names of a Pedigree or Genetic value Report to
#' something more descriptive.
#'
#' @return Updated list of column names
#'
#' @param headers a character vector of column (header) names
#' @export
#' @examples
#' library(nprcgenekeepr)
#' headerDisplayNames(headers = c("id", "sire", "dam", "sex", "birth", "age"))
headerDisplayNames <- function(headers) {
  nameConversion <- c(
    id = "Ego ID",
    sire = "Sire ID",
    dam = "Dam ID",
    sex = "Sex",
    gen = "Generation #",
    birth = "Birth Date",
    exit = "Exit Date",
    death = "Death",
    age = "Age (in years)",
    ancestry = "Ancestry",
    population = "Breeding Colony Member",
    group = "Subset Member",
    origin = "Origin",
    departure = "Departure",
    indivMeanKin = "Individual Mean Kinship",
    zScores = "Z-score (Mean Kinship)",
    genomeUnique = "Genome Uniqueness",
    totalOffspring = "Total Offspring",
    livingOffspring = "Living Offspring",
    rank = "Rank",
    value = "Value Designation",
    status = "Status",
    vasxOvx = "Vasectomy or Ovariectomy Status",
    gu = "Genome Uniqueness (%)",
    pedNum = "Pedigree #",
    spf = "SPF",
    condition = "Condition",
    siretype = "Sire Type",
    damtype = "Dam Type",
    numberofparentsknown = "Number of Parents Known",
    arrivalatcenter = "Arrival at Center",
    `fromcenter?` = "From Center ?",
    first = "First Allele Code",
    second = "Second Allele Code",
    first_name = "First Allele",
    second_name = "Second Allele",
    recordStatus = "Original/ Added" # nolint: nonportable_path_linter
  )
  as.character(nameConversion[headers])
}
