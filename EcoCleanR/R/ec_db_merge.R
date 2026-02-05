#' Merge the Data sets Extracted from Various datasources.
#' @description
#' condition to run this function:
#' all the data frames should have same fields follwing DwC standards:
#' e.g. attribute_list <- c("source","catalogNumber", "basisOfRecord", "occurrenceStatus", "institutionCode",	"verbatimEventDate",	"scientificName",	"individualCount",	"organismQuantity",	"abundance",	"decimalLatitude",	"decimalLongitude",	"coordinateUncertaintyInMeters",	"locality",	"verbatimLocality",	"municipality",	"county",	"stateProvince",	"country",	"countryCode")
#' Assign manually the source name in "source" field. example - gbif, obis, invertEBase etc
#' Assign values of individual count or organism count into abundance. Most online sources has one of them updated with specimen count.
#' this function depends on successful download of data files, it also allow to input csv files from local system
#' @param db_list list of data frames which we want to merge. e.g. GBIF, iDigbio, InvertEBase and any local file.

#' @param datatype default "modern". datatype accept text input as "modern" or "fossil"
#' @param occurrenceStatus default name for occurrenceStatus column is occurrenceStatus but a different name can be inserted if required.
#' @param basisOfRecord default name for basis of record column is basis of record but a different name can be inserted if required.

#' @return A data frame of occurrence records filtered to include only those classified as "modern" or "fossil".
#' @export
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @examples
#'
#' db1 <- data.frame(
#'   species = "A",
#'   decimalLongitude = c(-120, -117, NA, NA),
#'   decimalLatitude = c(20, 34, NA, NA),
#'   catalogNumber = c("12345", "89888", "LACM8898", "SDNHM6767"),
#'   occurrenceStatus = c("present", "", "ABSENT", "Present"),
#'   basisOfRecord = c("preserved_specimen", "", "fossilspecimen", "material_sample"),
#'   source = "db1",
#'   abundance = c(1, NA, 8, 23)
#' )
#'
#' db2 <- data.frame(
#'   species = "A",
#'   decimalLongitude = c(-120.2, -117.1, NA, NA),
#'   decimalLatitude = c(20.2, 34.1, NA, NA),
#'   catalogNumber = c("123452", "898828", "LACM82898", "SDNHM62767"),
#'   occurrenceStatus = c("present", "", "ABSENT", "Present"),
#'   basisOfRecord = c("preserved_specimen", "", "fossilspecimen", "material_sample"),
#'   source = "db2",
#'   abundance = c(1, 2, 3, 19)
#' )
#' db_list <- list(db1, db2)
#' merge_modern_data <- ec_db_merge(db_list = db_list, "modern")
#'
ec_db_merge <- function(db_list, datatype = "modern", occurrenceStatus = "occurrenceStatus", basisOfRecord = "basisOfRecord") {
  # Collect all input databases

  # Filter out invalid inputs
  db_list <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, db_list)

  if (length(db_list) == 0) {
    warning("No valid databases provided.")
    return(NULL)
  }

  # Combine all valid data frames
  Mixdb.occ <- do.call(rbind, db_list)

  # Clean and filter based on datatype
  Mixdb.occ <- Mixdb.occ %>%
    mutate(
      occurrenceStatus = ifelse(is.na(occurrenceStatus), "", occurrenceStatus)
    ) %>%
    filter(tolower(occurrenceStatus) != "absent")

  if (datatype == "modern") {
    Mixdb.occ <- Mixdb.occ %>%
      filter(
        !tolower(basisOfRecord) %in% c("fossil_specimen", "fossilspecimen", "material_sample")
      ) %>%
      mutate(basisOfRecord = "modern")
  } else if (datatype == "fossil") {
    Mixdb.occ <- Mixdb.occ %>%
      filter(tolower(basisOfRecord) %in% c("fossil_specimen", "fossilspecimen")) %>%
      mutate(basisOfRecord = "fossil")
  } else {
    warning("Invalid datatype. Choose 'modern' or 'fossil'.")
    return(NULL)
  }

  return(Mixdb.occ)
}
