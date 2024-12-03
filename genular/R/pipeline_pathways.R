#' Pathway to Cell Signature
#'
#' This function queries for pathways based on given values, fetches genes associated with 
#' those pathways, and filters for unique cell identifiers based on specified criteria. If 
#' pathway IDs are already known, they can be directly provided to skip the query step.
#'
#' @param queryValues A vector of query values to search for pathways. Optional if 
#' `pathway_ids` is provided.
#' @param pathway_ids A vector of pathway IDs to be used directly. Optional if `queryValues` 
#' is provided.
#' @param options A list of options for the API call, including endpoint and timeout settings.
#' @return A data frame of unique effect sizes, filtered by a foldChange threshold and 
#' deduplicated by cell_id.
#' @examples
#' \dontrun{
#' pathway_to_cell_signature(
#'   queryValues = c("Adaptive Immune System"), 
#'   options = list(timeout = 10000)
#' )
#' }
#' @export
pathway_to_cell_signature <- function(queryValues = NULL, pathway_ids = NULL, options = list(timeout = 10000)) {
	
	# Validate inputs
	if (is.null(queryValues) && is.null(pathway_ids)) {
		stop("Either queryValues or pathway_ids must be provided.")
	}

	# Fetch pathway suggestions based on queryValues
	if (is.null(pathway_ids)) {
	    pathway_suggest <- pathways_suggest(queryValues, options = options)
	    
	    # Extract the maximum score from the results
	    max_score <- max(sapply(pathway_suggest$results, function(item) item$matchScore))
	    
	    # Filter pathway_suggest$results to include only those items with the max score
	    filtered_results <- Filter(function(item) item$matchScore == max_score, pathway_suggest$results)
	    
	    # Extract pathway IDs from the filtered results
	    pathway_ids <- sapply(filtered_results, function(item) item$id)
	    pathway_ids <- as.vector(pathway_ids)
	}

    if (length(pathway_ids) == 0) {
        stop("No pathway IDs found for the given queryValues.")
    } else if (length(pathway_ids) == 1) {
    	pathway_ids <- list(c(pathway_ids))
    }

	## Get genes for pathways
	all_gene_results_pathways <- fetch_all_gene_search_results(
	    queryFields = list(c("ontology.id")),
	    queryValues = pathway_ids,
	    fieldsFilter = c("crossReference.enseGeneID",
	                     "geneID",
	                     "symbol",
	                     "ontology.id",
	                     "singleCellExpressions.effectSizes.i", "singleCellExpressions.effectSizes.s", "singleCellExpressions.effectSizes.c"),
	    searchType = "or",
	    orderBy = "geneID",
	    sortDirection = "asc",
	    responseType = "json",
	    matchType = "exact",
	    organismType = list(c(9606)),
	    ontologyCategories = list(),
	    limit = 1000,
	    debug = 1,
	    options = options
	)

    if (length(all_gene_results_pathways) == 0) {
        stop("No gene results found for the given pathway IDs.")
    }

  	# Filter gene results by foldChange >= 2
	all_gene_results_pathways_filtered <- lapply(all_gene_results_pathways, function(gene) {
	    if (!is.null(gene$singleCellExpressions) && !is.null(gene$singleCellExpressions$effectSizes)) {
	        gene$singleCellExpressions$effectSizes <- Filter(function(effectSize) {
	            !is.null(effectSize$foldChange) && effectSize$foldChange >= 2
	        }, gene$singleCellExpressions$effectSizes)
	    }
	    return(gene)
	})

  	# Extract and deduplicate effectSizes by cell_id
	all_effectSizes <- dplyr::bind_rows(lapply(all_gene_results_pathways_filtered, function(gene) {
	    if (!is.null(gene$singleCellExpressions) && !is.null(gene$singleCellExpressions$effectSizes)) {
	        return(dplyr::bind_rows(gene$singleCellExpressions$effectSizes))
	    }
	}))

	unique_cells <- all_effectSizes %>%
	    dplyr::distinct(cell_id, .keep_all = TRUE)

	# Calculate the threshold: mean + 1 standard deviation of foldChange
	threshold <- mean(unique_cells$foldChange) + stats::sd(unique_cells$foldChange)

	# Further filter unique_cells based on the calculated threshold
	highly_significant_cells <- unique_cells %>%
	    dplyr::filter(foldChange > threshold)
    
	return(highly_significant_cells)
}
