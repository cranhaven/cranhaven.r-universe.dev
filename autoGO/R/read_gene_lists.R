#' @title Read all the genes lists
#'
#' @description Function employed for reading all the gene lists on which the enrichment will be performed.
#' @param gene_lists_path Specify the full path to the folder where the gene lists have to read from (all fitting files in any subdirectory will be loaded).
#' @param log2FC_threshold Threshold value for log2(Fold Change) for considering genes as differentially expressed (default = 0).
#' @param padj_threshold Threshold value for adjusted p-value significance (Defaults to 0.05).
#' @param which_list It can be: "up_genes","down_genes","up_down_genes","everything". Select a list of genes to perform the enrichment. Respectively, both up and down regulated genes (up_down_genes), only up regulated genes (up_genes), only down regulated genes (down_genes), or (everything) allow to load all the three kind of lists separately and it is employed also for lists not from differential analysis..
#' @param from_autoGO Default is TRUE, set FALSE if the gene list you want to upload are not from a differential expression analysis.
#' @param files_format (Default = NULL). When from_autoGO = FALSE it is mandatory to provide the extension of the list of genes to upload.
#' @return List of gene lists, each one being a one-dimensional data.frame.
#' @examples
#' \dontrun{
#' gene_lists <- read_gene_lists(
#'   gene_lists_path = "./results",
#'   log2FC_threshold = 0,
#'   padj_threshold = 0.05,
#'   which_list = "down_genes",
#'   from_autoGO = T,
#'   files_format = NULL
#' )
#' }
#' @export


read_gene_lists <- function(gene_lists_path = "./results",
                            log2FC_threshold = 0,
                            padj_threshold = 0.05,
                            which_list = c(
                              "up_genes",
                              "down_genes",
                              "up_down_genes",
                              "everything"
                            ),
                            from_autoGO = TRUE,
                            files_format = NULL) {
  if (from_autoGO) {
    gene_lists_files <- list.files(path = gene_lists_path, pattern = ".*genes_list_.*.txt", recursive = TRUE, full.names = TRUE)
    to_read <- gene_lists_files[grepl(pattern = paste0("thFC", log2FC_threshold, "_thPval", padj_threshold), gene_lists_files)]
  } else if (!from_autoGO) {
    if (is.null(files_format)) {
      stop("when from_autoGO is FALSE, files_format must be specified, eg .txt")
    }
    gene_lists_files <- list.files(path = gene_lists_path, pattern = files_format, recursive = TRUE, full.names = TRUE)
    to_read <- gene_lists_files
    which_list <- "everything"
  }

  if (which_list == "up_down_genes") {
    to_read <- to_read[grepl(pattern = "/up_down_genes", to_read)]
  } else if (which_list == "up_genes") {
    to_read <- to_read[grepl(pattern = "/up_genes", to_read)]
  } else if (which_list == "down_genes") {
    to_read <- to_read[grepl(pattern = "/down_genes", to_read)]
  } else if (which_list == "everything") {
    to_read <- to_read
  }

  # FIXME: double slash after results?

  gene_lists <- lapply(to_read, function(x) read.table(x, header = FALSE, sep = "\n"))
  names(gene_lists) <- tools::file_path_sans_ext(to_read)

  return(gene_lists)
}
