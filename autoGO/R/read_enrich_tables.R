#' @title Read enrichment results from tables
#'
#' @description Helper function to read all the enrichment results in order to proceed with the visualization in an automated way.
#' @param enrich_table_path Specify the full path to the folder where enrichment tables have to read from (all fitting files in any subdirectory will be loaded).
#' @param log2FC_threshold Threshold value for log2(Fold Change) for considering genes as differentially expressed (default = 0).
#' @param padj_threshold Threshold value for adjusted p-value significance (Defaults to 0.05).
#' @param which_list It can be: "up_genes","down_genes","up_down_genes","everything". Select a list of genes to perform the enrichment. Respectively, only up regulated genes (up_genes), only down regulated genes (down_genes), both up and down regulated genes (up_down_genes), or (everything) allow to load all the three kind of lists separately and it is employed also for lists not from differential analysis.
#' @param from_autoGO Default is TRUE, set to FALSE if the lists you want to upload are not from a differential expression analysis.
#' @param files_format Default is NULL, when from_autoGO = FALSE it is mandatory to provide the extension of the list of genes you want to upload.
#' @return List of enrichment tables, each one being a tibble object.
#' @examples
#' \dontrun{
#' enrich_tables <- read_enrich_tables(
#'   enrich_table_path = "./results",
#'   log2FC_threshold = 0,
#'   padj_threshold = 0.05,
#'   which_list = "down_genes",
#'   from_autoGO = T,
#'   files_format = NULL
#' )
#' }
#' @export


read_enrich_tables <- function(enrich_table_path = "./results",
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
  enrich_lists_path <- list.files(path = enrich_table_path, recursive = T)
  enrich_lists_path <- file.path(enrich_table_path, enrich_lists_path)
  enrich_lists_path <- enrich_lists_path[!grepl(pattern = ".xlsx", x = enrich_lists_path)]
  enrich_lists_path <- enrich_lists_path[grepl(pattern = "enrichment_tables", x = enrich_lists_path)]

  if (from_autoGO) {
    to_read <- enrich_lists_path[grepl(pattern = paste0("thFC", log2FC_threshold, "_thPval", padj_threshold), enrich_lists_path)]
  } else if (!from_autoGO) {
    if (is.null(files_format)) {
      stop("when from_autoGO is FALSE, files_format must be specified, eg .txt")
    }
    if (is.null(files_format)) stop("Required parameter: 'files_format' for extension of enrichment tables, should be like '.txt' or '.tsv', etc.")
    enrich_lists_path <- list.files(path = enrich_table_path, pattern = files_format, recursive = T)
    to_read <- enrich_lists_path
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

  enrich_tables <- lapply(to_read, function(x) read_tsv(x, col_types = cols()))
  names(enrich_tables) <- tools::file_path_sans_ext(to_read)

  return(enrich_tables)
}
