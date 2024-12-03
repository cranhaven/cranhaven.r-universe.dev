#' Sort Input Parameters
#'
#' This internal function sorts various types of input parameters to ensure consistent ordering.
#'
#' @param input The input parameter to be sorted.
#'
#' @return The sorted input.
#'
#' @keywords internal
#' @noRd
sort_input <- function(input) {
    if (is.list(input)) {
        # Apply sorting to each vector within the list
        return(lapply(input, sort))
    } else if (is.numeric(input) || is.character(input)) {
        # Sort directly if it's a numeric or character vector
        return(sort(input))
    }
    # Return the input as-is if it doesn't match the expected types
    return(input)
}

#' Extract Data Based on Mappings
#'
#' This function iterates over a list of gene results, extracting and transforming data 
#' according to a provided mapping schema. It handles both direct mappings and nested 
#' array mappings, creating a comprehensive data frame with extracted data.
#'
#' @param all_gene_results A list of gene results, where each element is a list 
#'        containing gene information that might include nested structures.
#' @param mappings A list defining the mapping from input data structure to 
#'        output data frame columns. It supports direct mappings as well as 
#'        mappings for nested structures. The default mappings are provided.
#'        Each mapping should be a character vector for direct mappings or 
#'        a list of vectors for nested mappings.
#'
#' @return A data frame where each row corresponds to an entry in the input list, 
#'         and each column corresponds to one of the specified mappings. For nested 
#'         array mappings, multiple rows will be generated based on array entries,
#'         duplicating other information as needed.
#'
#' @examples
#' \donttest{
#' # Assuming all_gene_results is your input data
#'
#' all_gene_results <- fetch_all_gene_search_results(
#'   queryFields = list(c("symbol")),
#'   queryValues = c("A1CF", "A2M", "A4GALT", "A4GNT"),
#'   fieldsFilter = c("geneID", "symbol", "crossReference.enseGeneID", 
#'                    "mRNAExpressions.proteinAtlas.c", "ontology.id", 
#'                    "ontology.term", "ontology.cat"),
#'   searchType = "or",
#'   orderBy = "geneID",
#'   sortDirection = "asc",
#'   responseType = "json",
#'   matchType = "exact",
#'   organismType = list(c(9606)),
#'   ontologyCategories = list(),
#'   limit = 100,
#'   options = list(api_key = "3147dda5fa023c9763d38bddb472dd28", timeout = 10000)
#' )
#'
#' data_transposed <- extract_data(all_gene_results, list(
#'     "geneID" = "mappedGeneID",
#'     "symbol" = "mappedSymbol",
#'     "crossReference$enseGeneID" = "mappedEnseGeneID",
#'     "mRNAExpressions$proteinAtlas" = list(c("c" = "mappedC")),
#'     "ontology" = list(c("id" = "mappedId", "term" = "mappedTerm", "cat" = "mappedCat"))
#' ))
#' }
#' @export
extract_data <- function(all_gene_results, mappings = list(
      "geneID" = "mappedGeneID",
      "symbol" = "mappedSymbol",
      "crossReference$enseGeneID" = "mappedEnseGeneID",
      "mRNAExpressions$proteinAtlas" = list(c("c" = "mappedC")),
      "ontology" = list(c("id" = "mappedId", "term" = "mappedTerm", "cat" = "mappedCat"))
    )) {
    # Initialize a list to store each row of output data
    data_rows <- list()
    
    # Iterate over each gene result in the input list
    for (result in all_gene_results) {
        # Prepare a base row structure from non-array mappings
        base_row_data <- list()
        for (key in names(mappings)) {
            if (is.character(mappings[[key]])) {
                # Extract direct and single-level nested structures
                if (grepl("\\$", key)) {
                    keys_split <- unlist(strsplit(key, "\\$"))
                    nested_data <- result
                    for (subkey in keys_split) {
                        nested_data <- nested_data[[subkey]]
                        if (is.null(nested_data)) break
                    }
                    base_row_data[[mappings[[key]]]] <- if (is.null(nested_data)) NA else nested_data
                } else {
                    base_row_data[[mappings[[key]]]] <- if (is.null(result[[key]])) NA else result[[key]]
                }
            }
        }
        
        # Iterate over each gene result again for nested array mappings
        for (key in names(mappings)) {
            if (is.list(mappings[[key]])) {
                # Handle nested arrays
                nested_data <- result
                key_parts <- strsplit(key, "\\$")[[1]]
                for (key_part in key_parts) {
                    nested_data <- nested_data[[key_part]]
                    if (is.null(nested_data)) break
                }
                
                if (is.null(nested_data)) {
                    # If the nested data is missing, just add the base row
                    data_rows[[length(data_rows) + 1]] <- base_row_data
                } else {
                    # For each entry in the nested array, create a new row
                    for (nested_item in nested_data) {
                        row_data <- base_row_data
                        for (subkey in names(mappings[[key]][[1]])) {
                            mapped_name <- mappings[[key]][[1]][[subkey]]
                            row_data[[mapped_name]] <- if (is.null(nested_item[[subkey]])) NA else nested_item[[subkey]]
                        }
                        data_rows[[length(data_rows) + 1]] <- row_data
                    }
                }
            }
        }
    }
    
    # Step 1: Identify all unique column names
    all_columns <- unique(unlist(lapply(data_rows, names)))
    
    # Step 2: Ensure each item has all columns, adding NA where necessary
    data_rows_augmented <- lapply(data_rows, function(row) {
        # For each possible column, check if the row has it; if not, add it with NA
        for (column in all_columns) {
            if (!column %in% names(row)) {
                row[[column]] <- NA
            }
        }
        # Ensure the columns are in the same order for every row
        return(row[all_columns])
    })
    
    data_rows_augmented <- lapply(data_rows_augmented, as.data.frame, stringsAsFactors = FALSE)
    
    # Step 3: Combine the rows into a single data frame
    gene_data_transposed <- do.call(rbind, data_rows_augmented)
    
    # Optionally, convert the result to a data frame and fix row names
    gene_data_transposed <- as.data.frame(gene_data_transposed, stringsAsFactors = FALSE)
    rownames(gene_data_transposed) <- NULL
    
    return(gene_data_transposed)
}

#' Convert Gene Expression Data to Pathway-Level Features
#'
#' Transforms a gene expression matrix into pathway-level features per sample suitable for machine learning applications.
#' This function maps genes to their corresponding biological pathways, removes redundant pathways, calculates a 
#' \code{PathwayGeneScore} based on median gene expression and pathway variance, and optionally includes pathways not shared 
#' across multiple genes. Unmapped genes are retained as individual features in the final dataset.
#'
#' @param input_data A dataframe containing gene expression data, where rows represent samples and columns represent genes.
#'        Each cell contains the expression level of a gene in a specific sample.
#' @param data_transposed A dataframe containing gene-to-pathway mappings, with at least two columns: \code{mappedSymbol} 
#'        (gene symbols) and \code{mappedId} (unique pathway identifiers).
#' @param keep_non_shared A logical flag indicating whether to include pathways mapped to a single gene. 
#'        Defaults to \code{TRUE}. If set to \code{FALSE}, pathways mapped to fewer than two genes will be excluded from the final dataset.
#'
#' @return A dataframe where each row corresponds to a sample, and each column represents either a pathway-level feature 
#'         (\code{PathwayGeneScore}) or an unmapped gene's expression. Pathway features encapsulate the median expression 
#'         of genes within the pathway, adjusted by gene count and pathway variance. Unmapped genes are included as 
#'         individual features to retain comprehensive gene expression information.
#'
#' @examples
#' \donttest{
#' # Sample gene expression data
#' input_data <- data.frame(
#'   A1CF = c(2, 3, 3, 3),
#'   A2M = c(3, 4, 3, 3),
#'   A4GALT = c(3, 4, 3, 4),
#'   A4GNT = c(3, 4, 3, 3),
#'   ABC1 = c(2, 2, 2, 2),
#'   ABC2 = c(4, 4, 4, 4)
#' )
#'
#' # Sample gene-pathway mapping data
#' data_transposed <- data.frame(
#'   mappedSymbol = c("A4GNT", "A4GALT", "A2M", "A4GALT", "A2M", "A2M", "ABC1", "ABC2"),
#'   mappedId = c("GO:0000139", "GO:0000139", "GO:0001553", "GO:0001576",
#'                "GO:0001869", "GO:0002020", "GO:0000139", "GO:0000139")
#' )
#'
#' # Convert gene expression data to pathway-level features, including non-shared pathways
#' final_data <- convert_gene_expression_to_pathway_features(input_data, data_transposed, 
#'                                                           keep_non_shared = TRUE)
#' print(final_data)
#' }
#' @importFrom stats var
#' @export
convert_gene_expression_to_pathway_features <- function(input_data, 
    data_transposed, keep_non_shared = TRUE) {
  
  # Remove 'gene' column if present to prevent conflicts during pivoting
  if ("gene" %in% colnames(input_data)) {
    input_data <- input_data %>% dplyr::select(-gene)
  }
  
  # Add a sample identifier column
  input_data$sample_id <- paste0("Sample", 1:nrow(input_data))
  
  # Reshape the input data from wide to long format
  # Each row represents a gene expression value for a specific sample
  input_data_long <- input_data %>%
    tidyr::pivot_longer(
      cols = -sample_id,       # All columns except 'sample_id'
      names_to = "gene",       # New column 'gene' for gene names
      values_to = "expression" # New column 'expression' for expression values
    )
  
  # Obtain a unique mapping of genes to pathways
  gene_pathway_mapping <- data_transposed %>%
    dplyr::select(mappedSymbol, mappedId) %>% # Select relevant columns
    dplyr::distinct()                         # Remove duplicate mappings
  
  # **Redundancy Removal: Identify and Remove Duplicate Pathways**
  
  # Create a list of genes for each pathway
  pathway_gene_list <- gene_pathway_mapping %>%
    dplyr::group_by(mappedId) %>%
    dplyr::summarise(
      genes = paste(sort(unique(mappedSymbol)), collapse = ","),
      .groups = 'drop'
    )
  
  # Identify duplicate pathways based on identical gene sets
  duplicate_pathways <- pathway_gene_list %>%
    dplyr::group_by(genes) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::arrange(genes) %>%
    dplyr::ungroup()
  
  # Retain only one pathway per unique gene set
  unique_pathways <- pathway_gene_list %>%
    dplyr::group_by(genes) %>%
    dplyr::slice(1) %>%       # Keep the first occurrence
    dplyr::ungroup()
  
  # **Fixing the semi_join: Join only by 'mappedId'**
  # Previously, semi_join was incorrectly trying to join by 'mappedId' and 'mappedSymbol'
  # Since 'unique_pathways' does not contain 'mappedSymbol', we only join by 'mappedId'
  gene_pathway_mapping_unique <- gene_pathway_mapping %>%
    dplyr::semi_join(unique_pathways, by = "mappedId")
  
  # Merge gene expression data with unique pathway mapping
  input_data_long_mapped <- input_data_long %>%
    dplyr::left_join(
      gene_pathway_mapping_unique,
      by = c("gene" = "mappedSymbol"),
      relationship = "many-to-many"    # Handle many-to-many relationships
    )
  
  # Count the total number of unique genes mapped to each pathway across all samples
  genes_per_pathway <- gene_pathway_mapping_unique %>%
    dplyr::group_by(mappedId) %>%
    dplyr::summarise(
      total_genes_in_pathway = dplyr::n_distinct(mappedSymbol),
      .groups = 'drop'
    )
  
  # For each sample and pathway, compute:
  # - The median expression of genes in the pathway
  # - The number of genes contributing to the pathway in that sample
  pathway_expression <- input_data_long_mapped %>%
    dplyr::filter(!is.na(mappedId)) %>%            # Exclude genes not mapped to any pathway
    dplyr::group_by(sample_id, mappedId) %>%
    dplyr::summarise(
      median_expression = median(expression, na.rm = TRUE),
      genes_in_sample_pathway = dplyr::n_distinct(gene),
      .groups = 'drop'
    )
  
  # Merge pathway expression data with the total gene counts per pathway
  pathway_expression <- pathway_expression %>%
    dplyr::left_join(genes_per_pathway, by = "mappedId")
  
  # Calculate pathway variance across all samples
  pathway_variance <- pathway_expression %>%
    dplyr::group_by(mappedId) %>%
    dplyr::summarise(
      pathway_variance = var(median_expression, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Merge pathway variance with pathway_expression
  pathway_expression <- pathway_expression %>%
    dplyr::left_join(pathway_variance, by = "mappedId")
  
  # Calculate PathwayGeneScore
  pathway_expression <- pathway_expression %>%
    dplyr::mutate(
      # Conditionally set median_expression to NA based on keep_non_shared flag
      median_expression = if_else(
        !keep_non_shared & total_genes_in_pathway < 2,
        NA_real_,
        median_expression
      ),
      # Calculate PathwayGeneScore only if median_expression is not NA
      PathwayGeneScore = if_else(
        !is.na(median_expression),
        median_expression * log(genes_in_sample_pathway + 1) / pathway_variance,
        NA_real_
      )
    )
  
  # Create a complete set of combinations of sample IDs and pathways
  # This ensures all pathways are included for each sample, filling missing values with NA
  complete_pathways <- expand.grid(
    sample_id = unique(input_data_long$sample_id),
    mappedId = unique(gene_pathway_mapping_unique$mappedId),
    stringsAsFactors = FALSE
  )
  
  # Merge the complete pathways with the computed PathwayGeneScore
  pathway_expression_complete <- complete_pathways %>%
    dplyr::left_join(pathway_expression, by = c("sample_id", "mappedId"))
  
  # Pivot the PathwayGeneScore to wide format
  # Each pathway becomes a column, and each row represents a sample
  pathway_expression_wide <- pathway_expression_complete %>%
    dplyr::select(sample_id, mappedId, PathwayGeneScore) %>%
    tidyr::pivot_wider(
      names_from = mappedId,
      values_from = PathwayGeneScore
    )
  
  # Identify genes that are not mapped to any pathway
  unmapped_genes <- input_data_long_mapped %>%
    dplyr::filter(is.na(mappedId)) %>%        # Genes without pathway mappings
    dplyr::select(sample_id, gene, expression)
  
  # Pivot the unmapped genes to wide format to include them as individual features
  unmapped_genes_wide <- unmapped_genes %>%
    tidyr::pivot_wider(
      names_from = gene,
      values_from = expression
    )
  
  # Merge the pathway expressions and unmapped gene expressions into the final dataset
  final_data <- pathway_expression_wide %>%
    dplyr::left_join(unmapped_genes_wide, by = "sample_id")
  
  # Optionally, remove the 'sample_id' column if not needed
  final_data <- final_data %>% dplyr::select(-sample_id)
  
  # Return the final dataset ready for machine learning
  return(final_data)
}


#' Summarize Data by Category
#'
#' This function summarizes input data by categories defined in the mapping data. It supports
#' summary methods such as median and mean, and allows additional options like retaining 
#' missing categories or appending category IDs to names.
#'
#' @param input_data A data frame where each column represents a gene or an identifier,
#'        and each row represents an observation or a sample.
#' @param mapping_data A data frame that maps identifiers to categories, which must 
#'        include the columns specified by `identifier` and 'category'. Optionally, 
#'        it can contain 'category_id' for additional categorization details.
#' @param identifier The name of the column in `mapping_data` that corresponds to 
#'        the identifiers in the columns of `input_data`.
#' @param keep_missing A logical value indicating whether to retain identifiers in 
#'        `input_data` that are not found in `mapping_data`. If TRUE, they are kept as 
#'        separate categories.
#' @param keep_ids A logical value indicating whether to append category IDs to the 
#'        category names in the summary output.
#' @param summary_method The method used for summarizing within categories. 
#'        Currently supports "median" and "mean".

#' @return A data frame where each column represents a category and each row represents
#'         the summarized value of that category for the corresponding observation/sample.
#'
#' @examples
#' \donttest{
#' # Create a sample input data frame with gene expression levels
#' input_data <- data.frame(
#'     A1CF = c(2, 3, 3, 3),
#'     A2M = c(3, 4, 3, 3),
#'     A4GALT = c(3, 4, 3, 4),
#'     A4GNT = c(3, 4, 3, 3)
#' )
#' 
#' # Fetch gene-related data based on specified fields and conditions
#' # The function `fetch_all_gene_search_results` is presumably defined elsewhere
#' # and retrieves information from a biological database
#' all_gene_results <- fetch_all_gene_search_results(
#'     queryFields = list(c("symbol")),                 # Query by gene symbols
#'     queryValues = colnames(input_data),              # Gene symbols to query
#'     fieldsFilter = c(                                # Fields to extract from the results
#'         "geneID",
#'         "symbol",
#'         "crossReference.enseGeneID",
#'         "mRNAExpressions.proteinAtlas.c",
#'         "ontology.id",
#'         "ontology.term",
#'         "ontology.cat"
#'     ),
#'     searchType = "or",                               # Search type (OR condition for queries)
#'     orderBy = "geneID",                              # Ordering criteria
#'     sortDirection = "asc",                           # Sort direction (ascending)
#'     responseType = "json",                           # Format of the returned data
#'     matchType = "exact",                             # Type of match for the query
#'     organismType = list(c(9606)),                    # Organism type (e.g., Homo sapiens)
#'     ontologyCategories = list(),                     # Ontology categories to include
#'     limit = 100,                                     # Limit on the number of results
#'     options = list(api_key = "your_api_key", timeout = 10000)  # Additional options
#' )
#' 
#' # Transform the fetched gene data based on specified mappings
#' data_transposed <- extract_data(
#'     all_gene_results,
#'     list(
#'         "geneID" = "mappedGeneID",
#'         "symbol" = "mappedSymbol",
#'         "crossReference$enseGeneID" = "mappedEnseGeneID",
#'         "mRNAExpressions$proteinAtlas" = list(c("c" = "mappedC")),
#'         "ontology" = list(c(
#'             "id" = "mappedId",
#'             "term" = "mappedTerm",
#'             "cat" = "mappedCat"
#'         ))
#'     )
#' )
#' 
#' # Manually create a similar structure to the expected output of `extract_data`
#' # This mimics the processed and transposed gene data
#' data_transposed <- data.frame(
#'     mappedGeneID = c(2, 2, 2, 2, 2, 2),
#'     mappedSymbol = rep("A2M", 6),
#'     mappedEnseGeneID = rep("ENSG00000175899", 6),
#'     mappedC = c("gdT-cell", NA, NA, NA, NA, NA),
#'     mappedId = c(
#'         NA,
#'         "R-HSA-109582",
#'         "R-HSA-1474244",
#'         "R-HSA-382551",
#'         "R-HSA-140877",
#'         "R-HSA-1474228"
#'     ),
#'     mappedTerm = c(
#'         NA,
#'         "Hemostasis",
#'         "Extracellular matrix organization",
#'         "Transport of small molecules",
#'         "Formation of Fibrin Clot (Clotting Cascade)",
#'         "Degradation of the extracellular matrix"
#'     ),
#'     mappedCat = c(NA, 10, 10, 10, 11, 11),
#'     stringsAsFactors = FALSE
#' )
#' 
#' library(dplyr)
#' # Process and group the data by symbol, then summarize and arrange by terms
#' data_transposed_pathways <- data_transposed %>%
#'     dplyr::group_by(mappedSymbol) %>%
#'     dplyr::arrange(mappedTerm, .by_group = TRUE) %>%
#'     dplyr::summarize(
#'         category = first(mappedTerm),
#'         category_id = first(mappedId)
#'     )
#' 
#' # Display the first few rows of the grouped data
#' # print(head(data_transposed_pathways))
#' 
#' # Summarize the original input data by the categories defined in the processed gene data
#' # This function call summarizes expression levels by the gene's associated pathway or term
#' result_data_pathways <- summerize_by_category(
#'     input_data,
#'     data_transposed_pathways,
#'     identifier = "mappedSymbol",
#'     keep_missing = FALSE,
#'     keep_ids = FALSE,
#'     summary_method = "median"
#' )
#' }
#' @importFrom stats median
#' @importFrom dplyr %>%
#' @importFrom stats setNames
#' @export
summerize_by_category <- function(input_data, mapping_data, 
                                  identifier = "symbol",
                                  keep_missing = FALSE, 
                                  keep_ids = FALSE,
                                  summary_method = "median") {
    # Filter out NA category entries
    valid_mapping_data <- mapping_data[!is.na(mapping_data$category), ]
    
    # Append category ID to the category name if required
    if (keep_ids) {
        valid_mapping_data <- valid_mapping_data %>%
            dplyr::mutate(category = paste(category, category_id, sep = "_"))
    }
    
    categories <- unique(valid_mapping_data$category)
    category_data_list <- setNames(vector("list", length(categories)), categories)
    
    # Define a function that applies the chosen summarization method
    apply_summary <- function(data, method) {
        if (method == "median") {
            return(apply(data, 1, median, na.rm = TRUE))
        } else if (method == "mean") {
            return(apply(data, 1, mean, na.rm = TRUE))
        } else {
            stop("Unsupported summary method")
        }
    }
    
    # Iterate over each category to calculate the summary
    for (category in categories) {
        genes_in_category <- valid_mapping_data[[identifier]][valid_mapping_data$category == category]
        genes_in_category <- genes_in_category[genes_in_category %in% colnames(input_data)]
        category_data <- input_data[, genes_in_category, drop = FALSE]
        
        # Apply the selected summary method
        category_summary <- if (ncol(category_data) > 0) apply_summary(category_data, summary_method) else rep(NA, nrow(input_data))
        category_data_list[[category]] <- category_summary
    }
    
    # Handle missing genes if the flag is true
    if (keep_missing) {
        known_genes <- valid_mapping_data[[identifier]]
        missing_genes <- setdiff(colnames(input_data), known_genes)
        
        for (missing_gene in missing_genes) {
            category_data_list[[missing_gene]] <- input_data[[missing_gene]]
        }
    }
    
    # Combine the category summaries into a single data frame
    result_data <- do.call(cbind, category_data_list)
    
    return(result_data)
}
