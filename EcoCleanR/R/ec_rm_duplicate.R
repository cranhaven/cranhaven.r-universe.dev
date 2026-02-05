#' Remove Duplicate Records from the Merged Data
#' @details This function will provide a cleaned_catalog column as output, which has catalog numbers standardize and removed duplicates based on generated cleaned_catalog and abundance columns of data.
#' mandatory fields are catalogNumber, source and abundance
#' @param data this is merge data frame which is a output file after running ec_db_merge
#' @param catalogNumber this is a mandatory field which consider unique for each occurrence record.
#' @param abundance this is a mandatory field which has created while data extraction by combining individual count and quantity fields (may vary from one source to another, we aim to standardize those as "abundance").
#'
#' @return A data frame which has unique catalog numbers. the output file will have cleaned_catalog field instead of catalogNumber. Also the unique record will be chosen with the abundance value if there is any.
#' @export
#' @importFrom rlang sym
#' @import dplyr
#' @examples
#'
#' db1 <- data.frame(
#'   species = "A",
#'   decimalLongitude = c(-120.2, -117.1, NA, NA),
#'   decimalLatitude = c(20.2, 34.1, NA, NA),
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
#' ecodata <- ec_rm_duplicate(merge_modern_data,
#'   catalogNumber = "catalogNumber",
#'   abundance = "abundance"
#' )
#'
ec_rm_duplicate <- function(data, catalogNumber = "catalogNumber", abundance = "abundance") {
  # Convert column names from character to symbols for tidy evaluation
  cat_col <- rlang::sym(catalogNumber)
  abun_col <- rlang::sym(abundance)

  # Remove 'source' column if it exists
  data <- data[, !names(data) %in% "source"]


  # Clean catalog number: remove leading zeros for numeric, capitalize others
  data <- data %>%
    dplyr::mutate(
      cleaned_catalog = sapply(!!cat_col, function(x) {
        if (grepl("^[0-9]+$", x)) {
          return(as.numeric(x))
        } else {
          return(toupper(x))
        }
      })
    )

  # Remove original catalogNumber column
  data <- data %>% dplyr::select(-!!cat_col)

  # Remove duplicates, prioritizing by non-NA numeric abundance
  data <- data %>%
    dplyr::group_by(.data$cleaned_catalog) %>%
    dplyr::slice(
      if (any(!is.na(!!abun_col) & is.numeric(!!abun_col))) {
        which(!is.na(!!abun_col) & is.numeric(!!abun_col))[1]
      } else {
        1
      }
    ) %>%
    dplyr::ungroup()

  return(data)
}
