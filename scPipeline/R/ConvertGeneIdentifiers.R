# Function to convert gene identifiers in a Seurat object
#' Convert Gene Identifiers in a Seurat Object
#'
#' This function takes a Seurat object with gene identifiers as row names (e.g., RefSeq, Ensembl, Entrez)
#' and converts those identifiers to gene symbols (or Ensembl Gene IDs) using the biomaRt package.
#' The function can handle various types of gene identifiers and returns a Seurat object with updated row names.
#'
#' @importFrom biomaRt useMart getBM
#' @param seurat_object A Seurat object. The row names of the Seurat object's data or assay slot should represent gene identifiers (e.g., RefSeq, Ensembl, or Entrez IDs).
#' @param id_type A string specifying the type of the input gene identifiers. Options are: "refseq", "ensembl", "entrez". Default is "refseq".
#' @param to_id_type A string specifying the type of output gene identifiers. Options are: "symbol", "ensembl". Default is "symbol".
#' @return A Seurat object with updated gene names (row names) based on the specified conversion.
#' @export
#' @examples
#' \donttest{
#' # Read 10X counts data from matrix.mtx, barcodes.tsv and genes.tsv
#' library(Seurat)
#' counts <- Read10X(data.dir = "../inst/extdata", gene.column = 1)
#'
#' # Create Seurat object without batch correction
#' seurat_obj <- SeuratPreprocess(counts)
#' seurat_obj <- SeuratLowDim(seurat_obj)
#' # Convert RefSeq IDs to gene symbols
#' seurat_obj_converted <- ConvertGeneIdentifiers(
#'   seurat_obj,
#'   id_type = "refseq",
#'   to_id_type = "symbol"
#' )
#' }

ConvertGeneIdentifiers <- function(seurat_object, id_type = "refseq", to_id_type = "symbol") {

  # Set up biomaRt connection based on human genes
  ensembl <- useMart("ensembl", dataset = "hsapiens_gene_ensembl")

  # Mapping dictionary for filter and attributes based on the input ID type
  filter_map <- list(
    "refseq" = "refseq_mrna",
    "ensembl" = "ensembl_gene_id",
    "entrez" = "entrezgene_id"
  )

  attribute_map <- list(
    "symbol" = "external_gene_name",
    "ensembl" = "ensembl_gene_id"
  )

  # Check if the provided ID type is valid
  if (!(id_type %in% names(filter_map))) {
    stop("Invalid 'id_type'. Choose from 'refseq', 'ensembl', or 'entrez'.")
  }

  # Check if the provided to_id_type is valid
  if (!(to_id_type %in% names(attribute_map))) {
    stop("Invalid 'to_id_type'. Choose from 'symbol' or 'ensembl'.")
  }

  # Get the filter and attribute based on provided parameters
  id_filter <- filter_map[[id_type]]
  id_attribute <- attribute_map[[to_id_type]]

  # Get row names (gene identifiers) from the Seurat object
  gene_identifiers <- rownames(seurat_object)

  # Query biomaRt to map identifiers to the target gene names
  gene_info <- getBM(attributes = c(id_filter, id_attribute),
                     filters = id_filter,
                     values = gene_identifiers,
                     mart = ensembl)

  # Merge the results to map original gene identifiers to the new gene names
  gene_mapping <- gene_info[, c(id_filter, id_attribute)]
  colnames(gene_mapping) <- c("original_id", "new_gene_name")

  # Match the gene identifiers from Seurat object with the results from biomaRt
  updated_gene_names <- merge(data.frame(original_id = gene_identifiers),
                              gene_mapping,
                              by = "original_id",
                              all.x = TRUE)

  # Update Seurat object with new gene names as rownames
  rownames(seurat_object) <- updated_gene_names$new_gene_name
  return(seurat_object)
}
