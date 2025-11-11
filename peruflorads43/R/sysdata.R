#' Threatened Plant Species of Peru - Original List (DS 043-2006-AG)
#'
#' @description
#' Dataset containing the original threatened plant species list from Peru
#' as published in Supreme Decree DS 043-2006-AG (2006). This dataset preserves
#' the original nomenclature as it appeared in the 2006 decree.
#'
#' @format A tibble with the following columns:
#' \describe{
#'   \item{scientific_name}{Character. Full scientific name as listed in DS 043-2006-AG}
#'   \item{genus}{Character. Genus name}
#'   \item{species}{Character. Specific epithet}
#'   \item{tag}{Character. Infraspecific rank category (e.g., "SUBSP.", "VAR.", "F.")}
#'   \item{infraspecies}{Character. First infraspecific epithet}
#'   \item{tag_2}{Character. Second infraspecific rank category (for quaternomial names)}
#'   \item{infraspecies_2}{Character. Second infraspecific epithet (for quaternomial names)}
#'   \item{threat_category}{Character. IUCN threat category (CR, EN, VU, NT)}
#'   \item{accepted_name_author}{Character. Author citation for the name}
#'   \item{taxonomic_status}{Character. Taxonomic status (Accepted, Synonym)}
#'   \item{accepted_name}{Character. Current accepted name if this is a synonym}
#'   \item{family}{Character. Plant family}
#'   \item{protected_ds_043}{Logical. TRUE if protected under DS 043-2006-AG}
#' }
#'
#' @details
#' This dataset represents the threatened plant species list as originally
#' published in 2006. Some of these names may now be considered synonyms or
#' have updated nomenclature. For the updated nomenclature version, see
#' \code{\link{threatenedperu_syn}}.
#'
#' The dataset supports up to quaternomial names (Rank 4), meaning it can
#' represent species with two levels of infraspecific categories, such as:
#' \emph{Genus species subsp. infrasp1 f. infrasp2}
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
#' This dataset is based on Supreme Decree DS 043-2006-AG, which establishes
#' the official list of threatened wild flora species in Peru. The decree was
#' published by Peru's Ministry of Agriculture (MINAG) on July 13, 2006.
#'
#' @source
#' Supreme Decree DS 043-2006-AG, Ministry of Agriculture, Peru (2006).
#' Available at: \url{https://www.minam.gob.pe/}
#'
#' @seealso
#' \code{\link{threatenedperu_syn}} for the updated nomenclature version
#' \code{\link{is_ds043_2006_ag}} to check if species are protected
#' \code{\link{is_threatened_peru}} to query threat status
#'
#' @keywords internal
#' @name internal-datasets
#' @noRd
NULL


#' Threatened Plant Species of Peru - Updated Nomenclature
#'
#' @description
#' Dataset containing threatened plant species from DS 043-2006-AG with
#' updated nomenclature reflecting current taxonomic consensus. This allows
#' users with modern species names to find matches even when nomenclature
#' has changed since 2006.
#'
#' @format A tibble with the following columns:
#' \describe{
#'   \item{scientific_name}{Character. Original name from DS 043-2006-AG}
#'   \item{genus}{Character. Genus name (updated)}
#'   \item{species}{Character. Specific epithet (updated)}
#'   \item{tag_acc}{Character. Infraspecific rank category for accepted name}
#'   \item{infraspecies}{Character. Infraspecific epithet (updated)}
#'   \item{threat_category}{Character. IUCN threat category (CR, EN, VU, NT)}
#'   \item{accepted_name_author}{Character. Author citation}
#'   \item{taxonomic_status}{Character. Always "Accepted" in this dataset}
#'   \item{accepted_name}{Character. Current accepted scientific name}
#'   \item{family}{Character. Plant family}
#'   \item{protected_ds_043}{Logical. TRUE if protected under DS 043-2006-AG}
#' }
#'
#' @details
#' This dataset differs from \code{\link{threatenedperu}} in that:
#' \itemize{
#'   \item All names reflect current taxonomic consensus
#'   \item Synonyms have been resolved to accepted names
#'   \item Only accepted names are included
#'   \item Supports up to trinomial names (Rank 3 maximum)
#'   \item Uses \code{tag_acc} instead of \code{tag} for infraspecific ranks
#' }
#'
#' **Important:** This dataset does NOT support Rank 4 (quaternomial names).
#' The column \code{infraspecies_2} is not present in this version.
#'
#' @section Use Cases:
#' This dataset is ideal when:
#' \itemize{
#'   \item Working with recent species lists using current nomenclature
#'   \item Need to match modern names against DS 043-2006-AG protections
#'   \item Want to avoid synonym confusion
#'   \item Conducting conservation assessments with updated taxonomy
#' }
#'
#' @section Relationship to DS 043-2006-AG:
#' While this dataset uses updated nomenclature, all species listed here
#' are still protected under DS 043-2006-AG. The legal protection applies
#' regardless of nomenclatural changes.
#'
#' @source
#' Updated from Supreme Decree DS 043-2006-AG using current taxonomic
#' references including World Checklist of Vascular Plants (WCVP) and
#' Plants of the World Online (POWO).
#'
#' @seealso
#' \code{\link{threatenedperu}} for the original 2006 nomenclature
#' \code{\link{is_ds043_2006_ag}} to query both databases simultaneously
#' \code{\link{comparison_table_ds043}} to compare nomenclatural changes
#'
#' @keywords internal
#' @name internal-datasets
#' @noRd
NULL
