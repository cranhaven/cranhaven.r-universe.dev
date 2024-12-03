#' Cells to Gene Signature
#'
#' Queries for cells based on given values, fetches genes associated with those cells,
#' and filters for unique gene identifiers based on specified criteria such as fold change.
#' This function streamlines the process of identifying significant gene signatures from
#' cell query results. The function further filters results based on a threshold
#' determined by the mean and standard deviation of foldChange values.
#'
#' @param queryValues A vector of query values to search for cells.
#' @param data A data frame containing gene expression data to be mapped.
#' @param dataMeta A data frame containing metadata for each entry in `data`.
#' @param uniqueRowID The name of the column in `dataMeta` that uniquely identifies each row.
#' @param options A list of options for the API call, including endpoint and timeout settings.
#' @return A list containing `data`, the final data frame with gene signatures, and 
#'   `mapping`, a data frame of gene to cell mappings.
#' @examples
#' \dontrun{
#' cells_to_gene_signature(
#'   queryValues = c("CL0001054", "CL0002397"),
#'   data = yourData,
#'   dataMeta = yourDataMeta,
#'   uniqueRowID = "yourUniqueIdentifierColumnName"
#' )
#' }
#' @importFrom dplyr mutate filter left_join if_else group_by summarise
#' @importFrom tidyr pivot_longer pivot_wider
#' @export
cells_to_gene_signature <- function(queryValues, data, dataMeta, uniqueRowID = "", options = list(timeout = 10000)) {
  # Validate inputs
  if (is.null(queryValues) || length(queryValues) == 0) {
      stop("queryValues must be provided and not empty.")
  }

  # Validate uniqueRowID
  if(uniqueRowID == "" || !uniqueRowID %in% names(dataMeta)) {
    stop("uniqueRowID must be provided and exist in dataMeta.")
  }

  # Suggest cells based on queryValues
  cell_suggest_results <- cells_suggest(queryValues, options = options)
  cell_suggest_results <- cell_suggest_results$content$result

  if (length(cell_suggest_results) == 0) {
    stop("No cell IDs found for the given queryValues.")
  }

  # Format queryValues for cell search
  queryValues <- cells_search_format(cell_suggest_results, cells_lineages = "both")

  # Search for cells
  cell_search_results <- cells_search(queryValues = queryValues,
                                      fieldsFilter = c("geneID", "symbol", "crossReference.enseGeneID",
                                                       "singleCellExpressions.effectSizes.i", "singleCellExpressions.effectSizes.s", "singleCellExpressions.effectSizes.c"),
                                      searchType = "or",
                                      orderBy = "geneID",
                                      sortDirection = "asc",
                                      responseType = "json",
                                      matchType = "exact",
                                      organismType = list(c(9606)),
                                      limit = 50000,
                                      debug = 1,
                                      options = options)
  cell_search_results <- cell_search_results$content$results

  # Transpose data for gene to cell mapping
  data_transposed_cell <- extract_data(cell_search_results, list(
      "geneID" = "mappedGeneID",
      "symbol" = "mappedSymbol",
      "crossReference$enseGeneID" = "mappedEnseGeneID",
      "singleCellExpressions$effectSizes" = list(c("cell_id" = "mappedId", "cell_term" = "mappedTerm", "markerScore" = "markerScore", "scoreThreshold" = "markerScoreThreshold", "foldChange" = "foldChange"))
  ))

  # Calculate the threshold: mean + 1 standard deviation of foldChange
  threshold <- mean(data_transposed_cell$foldChange) + stats::sd(data_transposed_cell$foldChange)

  # Further filter unique_cells based on the calculated threshold
  filtered_data <- data_transposed_cell %>%
      dplyr::filter(foldChange > threshold)


  # Ensure dataMeta contains the uniqueRowID column
  if(!uniqueRowID %in% names(data)) {
    # Use uniqueRowID to add or update sampleID in data from dataMeta
    data <- dplyr::mutate(data, uniqueSampleIDInternal = dataMeta[[uniqueRowID]])
  }

  # Pivot input_data to long format, preserving the sampleID
  input_data_long <- pivot_longer(data, 
                                  cols = -uniqueSampleIDInternal, # Keep sampleID from being treated as a column to pivot
                                  names_to = "geneSymbol", 
                                  values_to = "expressionValue")

  # Join input_data_long with filtered_data on geneSymbol
  gene_cell_mapping <- left_join(input_data_long, 
                                 filtered_data, 
                                 by = c("geneSymbol" = "mappedSymbol"),
                                 relationship = "many-to-many")

  # First, fill NA in mappedTerm with a placeholder or geneSymbol itself
  gene_cell_mapping_tf <- gene_cell_mapping %>%
      mutate(mappedTerm = if_else(is.na(mappedTerm), paste("GeneSymbol:", geneSymbol), mappedTerm))

  # Then, aggregate expression values by uniqueRowID and mappedTerm (or geneSymbol if mappedTerm is NA)
  cell_expression_summary <- gene_cell_mapping_tf %>%
      group_by(uniqueSampleIDInternal, mappedTerm) %>%
      summarise(totalExpression = median(expressionValue, na.rm = TRUE), .groups = 'drop')


  # If you want to pivot wider for a sample x cell type matrix, you can do:
  cell_expression_matrix <- cell_expression_summary %>%
      pivot_wider(names_from = mappedTerm, values_from = totalExpression, values_fill = list(totalExpression = 0))

  cell_expression_df <- as.data.frame(cell_expression_matrix)

  final_data <- dplyr::left_join(dataMeta, cell_expression_df, by = setNames("uniqueSampleIDInternal", uniqueRowID))

  return(list(data = final_data, mapping = gene_cell_mapping))
}

