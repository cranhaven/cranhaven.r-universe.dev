#' Read a downloaded Kew Tree of Life manifest
#'
#' @param file Local path to a manifest file.
#' @param manifest Manifest type. Used only to assign column names when known.
#' @param ... Additional arguments passed to [utils::read.delim()].
#'
#' @return A data frame.
#' @export
tol_manifest <- function(file, manifest = NULL, ...) {
  if (!file.exists(file)) {
    stop("Manifest file does not exist: ", file, call. = FALSE)
  }

  data <- utils::read.delim(
    file,
    header = FALSE,
    sep = "\t",
    quote = "",
    comment.char = "",
    stringsAsFactors = FALSE,
    ...
  )

  manifest <- manifest %||% sub("\\.txt$", "", basename(file))
  names <- manifest_columns(manifest)
  if (!is.null(names) && length(names) == ncol(data)) {
    colnames(data) <- names
  }

  data
}

manifest_columns <- function(manifest) {
  columns <- list(
    sequence_manifest = c(
      "repository_name", "sequence_identifier", "sequence_type",
      "scientific_species_name", "project_name"
    ),
    deleted_sequences = c(
      "repository_name", "sequence_identifier", "sequence_type",
      "scientific_species_name", "first_included_release",
      "deleted_from_release", "deletion_reason"
    ),
    specimen_manifest = c(
      "scientific_species_name", "collection_id", "specimen_id_or_barcode",
      "voucher_information", "specimen_url"
    ),
    revised_specimen_nomenclature = c(
      "repository_name", "sequence_identifier", "old_species_name",
      "new_species_name", "first_release_with_new_name"
    ),
    gene_manifest = c(
      "gene_id", "exemplar_gene_name", "exemplar_species",
      "database_name", "record_id", "url", "in_tree"
    )
  )

  columns[[manifest]]
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
