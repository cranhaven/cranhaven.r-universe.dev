#' Kew Tree of Life species index
#'
#' A normalized species/specimen index exported from Kew Tree of Life Explorer.
#' Each row represents one Tree of Life sequence record and includes taxonomic
#' metadata, specimen metadata, recovery statistics, and the FASTA URL for the
#' corresponding `fasta/by_recovery` file.
#'
#' @format A tibble with 20485 rows and 16 columns:
#' \describe{
#'   \item{sequence_id}{Kew Tree of Life sequence identifier.}
#'   \item{data_source}{Project, dataset, or repository source.}
#'   \item{order}{Taxonomic order.}
#'   \item{family}{Taxonomic family.}
#'   \item{genus}{Taxonomic genus.}
#'   \item{specific_epithet}{Specific epithet or infraspecific name text.}
#'   \item{specimen_reference}{Voucher or specimen reference.}
#'   \item{specimen_barcode}{Specimen barcode where available.}
#'   \item{collection_date}{Collection year where available.}
#'   \item{country_of_origin}{Country of origin where available.}
#'   \item{material_sampled}{Sample material type.}
#'   \item{no_of_genes_recovered}{Number of recovered Angiosperms353 genes.}
#'   \item{no_of_bp_recovered}{Number of recovered base pairs.}
#'   \item{fasta_file_url}{Remote FASTA URL for this sequence recovery.}
#'   \item{scientific_name}{Combined genus and specific epithet.}
#'   \item{fasta_file_name}{Basename of `fasta_file_url`.}
#' }
#'
#' @source Kew Tree of Life Explorer species list.
"tol_species"
