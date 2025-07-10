#' @title Filtering DESeq2 results
#'
#' @description We could be in a position to carry out multiple filters on the results of the differential analysis, in order not to repeat all the deseq_analysis.R code which provides for the actual computation of the differential analysis, the filtering_DE.R function has been implemented to be able to filter the file(s) " * _allres.tsv " and generate all the folders and files associated with the specific filters applied.
#' @description The function automatically searches inside the folders where_results and outfolder the file(s) (See ?deseq_analysis()) "_allres.tsv" and generates folders and files in the same folders with the new filters for foldchange and pvalue respectively.
#' @param padj_threshold (Default = 0.05) Threshold value for adjusted p-value filtering.
#' @param log2FC_threshold (Default = 0) Threshold value for log2(Fold Change) filtering.
#' @param outfolder (Default = "./results") Name of the folder in which the new output is written.
#' @param save_excel (Default = FALSE) Write output in MS Excel file format (.xlsx).
#' @return No return value. Files will be produced as part of normal execution.
#' @examples
#' \dontrun{
#' filtering_DE(
#'   padj_threshold = 0.05,
#'   log2FC_threshold = 1,
#'   outfolder = "./results",
#'   save_excel = F
#' )
#' }
#' @export


filtering_DE <- function(padj_threshold = 0.05,
                         log2FC_threshold = 1,
                         outfolder = "./results",
                         save_excel = FALSE) {
  # there can be only one _allres.tsv file per comparison directory
  all_res <- list.files(
    path = outfolder,
    pattern = "_allres.tsv", recursive = TRUE
  )

  # add full path to result files
  all_res_path <- file.path(outfolder, all_res)

  # load result dataframes
  list_of_comparisons <- lapply(all_res_path, function(x) read_tsv(x, col_types = cols()))

  # extract the "comparison" name for each result
  names(list_of_comparisons) <- gsub("\\/.*", "", all_res)

  for (comparison_name in names(list_of_comparisons)) {
    data <- list_of_comparisons[[comparison_name]]
    filtered <- data %>%
      dplyr::filter(.data$padj < padj_threshold & abs(.data$log2FoldChange) > log2FC_threshold)

    groups_fold <- file.path(outfolder, comparison_name)
    groups_fold_filtered <- paste0("filtered_DE", "_thFC", log2FC_threshold, "_thPval", padj_threshold)
    groups_fold_filtered_path <- file.path(groups_fold, groups_fold_filtered)
    groups_fold_thresh_up_down <- file.path(groups_fold_filtered_path, "up_down_genes")
    groups_fold_thresh_up <- file.path(groups_fold_filtered_path, "up_genes")
    groups_fold_thresh_down <- file.path(groups_fold_filtered_path, "down_genes")

    # saving filtered results in different folders by thresholds
    if (!dir.exists(groups_fold_filtered_path)) dir.create(groups_fold_filtered_path, recursive = T)
    filename <- paste0("filtered_DE_", comparison_name, "_thFC", log2FC_threshold, "_thPval", padj_threshold)
    write_tsv(filtered, file.path(groups_fold_filtered_path, paste0(filename, ".tsv")))
    if (save_excel) openxlsx::write.xlsx(filtered, file = file.path(groups_fold_filtered_path, paste0(filename, ".xlsx")), row.names = F)

    # saving gene lists
    if (!dir.exists(groups_fold_thresh_up_down)) dir.create(groups_fold_thresh_up_down, recursive = T)
    filename <- paste0("up_down_genes_list_", comparison_name, "_thFC", log2FC_threshold, "_thPval", padj_threshold, ".txt")
    write.table(filtered$genes, file.path(groups_fold_thresh_up_down, filename), quote = F, row.names = F, col.names = F)

    if (!dir.exists(groups_fold_thresh_up)) dir.create(groups_fold_thresh_up, recursive = T)
    filename <- paste0("up_genes_list_", comparison_name, "_thFC", log2FC_threshold, "_thPval", padj_threshold, ".txt")
    write.table(filtered$genes[filtered$log2FoldChange > 0], file.path(groups_fold_thresh_up, filename), quote = F, row.names = F, col.names = F)

    if (!dir.exists(groups_fold_thresh_down)) dir.create(groups_fold_thresh_down, recursive = T)
    filename <- paste0("down_genes_list_", comparison_name, "_thFC", log2FC_threshold, "_thPval", padj_threshold, ".txt")
    write.table(filtered$genes[filtered$log2FoldChange < 0], file.path(groups_fold_thresh_down, filename), quote = F, row.names = F, col.names = F)
  }
}
