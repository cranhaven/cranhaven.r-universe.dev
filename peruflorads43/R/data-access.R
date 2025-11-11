#' Get Threatened Species Database
#'
#' @description
#' Retrieves the threatened plant species database for Peru. This function
#' provides controlled access to the internal datasets used by the package.
#'
#' @param type Character string specifying which database version to retrieve.
#'   Options are:
#'   \itemize{
#'     \item \code{"original"} (default): Original nomenclature from DS 043-2006-AG (2006)
#'     \item \code{"updated"}: Updated nomenclature with current taxonomic consensus
#'   }
#'
#' @return A tibble containing the threatened species database.
#'
#' @section Database Structure:
#' **Original Database** (\code{type = "original"}):
#' \itemize{
#'   \item ~777 species as listed in DS 043-2006-AG
#'   \item Supports quaternomial names (Rank 4)
#'   \item Includes both accepted names and synonyms
#'   \item Columns: scientific_name, genus, species, tag, infraspecies,
#'     tag_2, infraspecies_2, threat_category, accepted_name_author,
#'     taxonomic_status, accepted_name, family, protected_ds_043
#' }
#'
#' **Updated Database** (\code{type = "updated"}):
#' \itemize{
#'   \item Updated nomenclature using WCVP and POWO
#'   \item Supports trinomial names (Rank 3 maximum)
#'   \item Only accepted names (synonyms resolved)
#'   \item Columns: scientific_name, genus, species, tag_acc, infraspecies,
#'     threat_category, accepted_name_author, taxonomic_status,
#'     accepted_name, family, protected_ds_043
#' }
#'
#' @section Threat Categories:
#' \describe{
#'   \item{CR}{Critically Endangered}
#'   \item{EN}{Endangered}
#'   \item{VU}{Vulnerable}
#'   \item{NT}{Near Threatened}
#' }
#'
#' @section Legal Context:
#' Data based on Supreme Decree DS 043-2006-AG, Ministry of Agriculture,
#' Peru (July 13, 2006), which establishes the official list of threatened
#' wild flora species in Peru.
#'
#' @note
#' This function is primarily for advanced users who need direct access
#' to the database structure. For most use cases, use the higher-level
#' functions: \code{\link{is_threatened_peru}} or \code{\link{is_ds043_2006_ag}}.
#'
#' @seealso
#' \code{\link{is_threatened_peru}} to check threat status of species
#' \code{\link{is_ds043_2006_ag}} to check DS 043 protection status
#'
#' @export
#' @examples
#' \donttest{
#' # Get original database
#' db_original <- get_threatened_database(type = "original")
#' str(db_original)
#' nrow(db_original)
#'
#' # Get updated database
#' db_updated <- get_threatened_database(type = "updated")
#' str(db_updated)
#'
#' # Compare number of species
#' n_original <- nrow(db_original)
#' n_updated <- nrow(db_updated)
#' cat("Original:", n_original, "| Updated:", n_updated, "\n")
#'
#' # Count by threat category
#' table(db_original$threat_category)
#'
#' # Find critically endangered orchids
#' orchids <- db_original[db_original$family == "ORCHIDACEAE" &
#'                        db_original$threat_category == "CR", ]
#' head(orchids$scientific_name)
#' }
get_threatened_database <- function(type = c("original", "updated")) {
  type <- match.arg(type)

  # Validate input
  if (!type %in% c("original", "updated")) {
    stop(
      "Invalid type: '", type, "'. Must be 'original' or 'updated'.",
      call. = FALSE
    )
  }

  # Use internal helper function
  tryCatch({
    get_threatened_data(type = type)
  }, error = function(e) {
    stop(
      "Failed to load '", type, "' database.\n",
      "Error: ", e$message,
      "\nThis may indicate a package installation problem.",
      call. = FALSE
    )
  })
}


#' Get Database Summary Statistics
#'
#' @description
#' Provides summary statistics for the threatened species databases.
#'
#' @param type Character string: "original", "updated", or "both" (default).
#'
#' @return A tibble with summary statistics.
#'
#' @export
#' @examples
#' \donttest{
#' # Get summary of both databases
#' summary_stats <- get_database_summary()
#' print(summary_stats)
#'
#' # Get summary of just the original
#' summary_original <- get_database_summary("original")
#' print(summary_original)
#' }
get_database_summary <- function(type = c("both", "original", "updated")) {
  type <- match.arg(type)

  summarize_db <- function(db, db_name) {
    tibble::tibble(
      Database = db_name,
      Total_Species = nrow(db),
      CR = sum(db$threat_category == "CR", na.rm = TRUE),
      EN = sum(db$threat_category == "EN", na.rm = TRUE),
      VU = sum(db$threat_category == "VU", na.rm = TRUE),
      NT = sum(db$threat_category == "NT", na.rm = TRUE),
      Families = length(unique(db$family)),
      Genera = length(unique(db$genus)),
      Has_Infraspecies = sum(!is.na(db$infraspecies)),
      Max_Rank = if("infraspecies_2" %in% names(db)) {
        if(any(!is.na(db$infraspecies_2))) 4 else 3
      } else {
        3
      }
    )
  }

  if (type == "both") {
    db_orig <- get_threatened_database("original")
    db_upd <- get_threatened_database("updated")

    rbind(
      summarize_db(db_orig, "Original (2006)"),
      summarize_db(db_upd, "Updated")
    )
  } else {
    db <- get_threatened_database(type)
    summarize_db(db, type)
  }
}

