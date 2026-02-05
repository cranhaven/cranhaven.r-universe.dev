#' Check Accepted Synonyms from WoRMs Taxonomy
#' @param species_name input species name.e.g. Mexacanthina lugubris
#' @param data data table which has information of all occurrence data of the selected species
#' @param scientificName default set to scientificName, this is a column in the data extracted from online sources, may have various synonyms of species_name.
#' @return A table with two columns, column one represent the accepted synonyms, and column two demonstrate the unique species names from the occurrence data base with the number of records tagged under species names.
#' @importFrom taxize get_wormsid
#' @importFrom taxize synonyms
#' @import dplyr
#' @export
#'
#' @examples
#' species_name <- "Mexacanthina lugubris"
#' data <- data.frame(
#'   scientificName = "Mexacanthina lugubris",
#'   decimalLongitude = c(-120, -78, -110, -60, -75, -130, -10, 5),
#'   decimalLatitude = c(20, 34, 30, 10, 40, 25, 15, 35)
#' )
#' comparison <- ec_worms_synonym(species_name, data, scientificName = "scientificName")
#' print(comparison)
#'
ec_worms_synonym <- function(species_name, data, scientificName = "scientificName") {
  # Step 1: Get WoRMS ID and synonyms
  worms_id <- get_wormsid(species_name)
  worms_syn <- synonyms(worms_id[1], db = "worms")

  # Step 2: Extract unique scientific names from WoRMS synonyms and input ecodata
  worms_syn_df <- worms_syn[[1]]
  unique_worms <- unique(dplyr::select(worms_syn_df, "scientificname")) # this is a column name from worms database.
  ecodata_counts <- data %>%
    dplyr::group_by(.data$scientificName) %>%
    dplyr::summarise(record_count = n(), .groups = "drop")

  ecodata_counts <- ecodata_counts %>%
    dplyr::mutate(scientificName_with_count = paste0(.data$scientificName, " (", .data$record_count, ")"))

  # Step 3: Convert to vectors and sort
  vec_worms <- sort(c(as.vector(unique_worms$scientificname), species_name))
  vec_data <- ecodata_counts$scientificName_with_count

  # Step 4: Make lengths equal
  max_length <- max(length(vec_worms), length(vec_data))
  vec_worms <- c(vec_worms, rep(NA, max_length - length(vec_worms)))
  vec_data <- c(vec_data, rep(NA, max_length - length(vec_data)))

  # Step 5: Create a comparison table
  comparison_table <- data.frame(
    Accepted_syn_worms = vec_worms,
    ecodata_syn_with_count = vec_data
  )

  # Return the comparison table
  return(comparison_table)
}
