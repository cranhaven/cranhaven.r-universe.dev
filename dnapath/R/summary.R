#' Rename genes in the differential network analysis
#' 
#' @param x A 'dnapath_list' or 'dnapath' object from \code{\link{dnapath}},
#' a pathway list, or a vector of gene names. 
#' @param gene_mat (Optional) A matrix of key value pairs. The first column should contain
#' current gene names, and the second column the new names. Any genes that are
#' not in this matrix will retain their current names. This can be any 
#' user-defined mapping, or the mapping obtained using 
#' \code{\link{entrez_to_symbol}} or \code{\link{symbol_to_entrez}}.
#' @param to (Optional) Setting `to = "symbol"` will rename entrezgene IDs to 
#' gene symbols; this will automatically call the `entrez_to_symbol()` function 
#' to obtain the mapping for `gene_mat`. The `species` argument must also be 
#' specified when `to` is used. 
#' @param species (Optional) Must be specified when setting `to = "symbol"`. This
#' argument is passed into \code{\link{entrez_to_symbol}}.
#' @param ... Additional arugments are passed into \code{\link{entrez_to_symbol}}
#' in the case that `to` and `species` are specified. This may be useful to
#' specify the `dir_save` argument to save the mapping obtained from
#' biomaRt for offline use.
#' @return Returns `x` with all gene names updated according to `gene_mat`.
#' @note 
#' Internet connection is required to connect to use 
#' \code{\link{entrez_to_symbol}} or \code{\link{symbol_to_entrez}}.
#' 
#' @seealso 
#' \code{\link{entrez_to_symbol}}, \code{\link{symbol_to_entrez}}
#' @export
#' @examples 
#' data(meso)
#' data(p53_pathways)
#' set.seed(0)
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways,
#'                    group_labels = meso$groups, n_perm = 10)
#' summary(results[[1]])  # Summary of pathway 1; note that it uses entrezgene IDs.
#' 
#' \donttest{
#' # Rename the entrezgene IDs into gene symbols.
#' results_sym <- rename_genes(results, to = "symbol", species = "human")
#' summary(results_sym[[1]]) # Now the summary uses gene symbols.
#' }
rename_genes <- function(x, gene_mat = NULL, to = NULL, species = NULL, ...) {
  if(!is.null(to)) {
    if(is.null(species)) {
      stop("species must be specifed when using to.")
    }
    if(grepl("symbol", to[1], ignore.case = TRUE)) {
      
      genes <- get_genes(x)
      gene_mat <- entrez_to_symbol(genes, species = species, ...)
    } else {
      stop('to = "', to[1], '" is unavailable.', 
           ' Only `to = "symbol"` is currently implemented.')
    }
  } else if(is.null(gene_mat)) {
    stop("Arguments `gene_mat`, `to`, and `species` cannot all be NULL.")
  }
  
  if(is.vector(gene_mat) && length(gene_mat) == 2) {
    gene_mat <- matrix(gene_mat, ncol = 2)
  } else if(ncol(gene_mat) != 2) {
    stop(deparse(substitute(gene_mat)), " is not a 2 column matrix or vector ", 
         "of length 2.")
  }
  
  # Check if either column in gene_mat is a factor. If so, coerce into character.
  if(is.factor(gene_mat[, 1])) 
    gene_mat[, 1] <- as.character(gene_mat[, 1])
  if(is.factor(gene_mat[, 2])) 
    gene_mat[, 2] <- as.character(gene_mat[, 2])
  
  if(is(x, "dnapath") || is(x, "dnapath_list")) {
    new_gene_names <- x$param$gene_names # Start with the old names.
    # Subset gene_mat onto genes contained in the dnapath results.
    gene_mat <- gene_mat[gene_mat[, 1] %in% new_gene_names, ]
    # Update old genes with new gene name.
    new_gene_names <- unname(sapply(new_gene_names, function(x) {
      index <- which(gene_mat[, 1] == x)
      if(length(index) == 0)
        return(x) # No new name found, keep original.
      if(length(index) > 1)
        warning("Duplicate names found for ", x, ".")
      return(gene_mat[index, 2])
    }))
    
    index_cols <- match(x$param$gene_names, colnames(x$param$x))
    x$param$gene_names <- new_gene_names
    colnames(x$param$x) <- new_gene_names
    
  } else if(is.list(x) &&
            (all(sapply(x, is.character)) || all(sapply(x, is.numeric)))) {
    # If x appears to be a pathway_list, then update its gene names.
    
    # Update old genes with new gene name.
    x <- lapply(x, function(gene_set) {
      unname(sapply(gene_set, function(gene) {
        index <- which(gene_mat[, 1] == gene)
        if(length(index) == 0)
          return(gene) # No new name found, keep original.
        if(length(index) > 1)
          warning("Duplicate names found for ", gene, ".")
        return(gene_mat[index, 2])
      }))
    })
  } else if(is.character(x) || is.numeric(x)) {
    # If x appears to be a single pathway (a vector of strings), update its names.
    x <- unname(sapply(x, function(gene) {
      index <- which(gene_mat[, 1] == gene)
      if(length(index) == 0)
        return(gene) # No new name found, keep original.
      if(length(index) > 1)
        warning("Duplicate names found for ", gene, ".")
      return(gene_mat[index, 2])
    }))
  } else {
    stop(deparse(substitute(x)), " must be a 'dnapath_list' or 'dnapath'", 
         " object, a pathway list, or a vector of gene names.")
  }
  
  return(x)
}


#' Get the gene names from a differential network analysis
#' 
#' @param x A 'dnapath_list' or 'dnapath' object from \code{\link{dnapath}},
#' or a pathway list. 
#' @return Returns a vector containing all the genes in `x`.
#' @seealso 
#' \code{\link{rename_genes}}, \code{\link{entrez_to_symbol}}, 
#' \code{\link{symbol_to_entrez}}
#' @export
#' @examples 
#' data(meso)
#' data(p53_pathways)
#' set.seed(0)
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways,
#'                    group_labels = meso$groups, n_perm = 10)
#' genes <- get_genes(results)
get_genes <- function(x) {
  if(is(x, "dnapath_list")) {
    genes <- x$param$gene_names
  } else if(is(x, "dnapath")) {
    index <- x$pathway$genes
    genes <- x$param$gene_names[index]
  } else if(is.list(x) &&
            (all(sapply(x, is.character)) || all(sapply(x, is.numeric)))) {
    # If x looks like a pathway list.
    genes <- unique(unlist(x))
  } else if(is.character(x) || is.numeric(x)) {
    # If x looks like a single pathway.
    genes <- x
  } else {
    stop(deparse(substitute(x)), " must be a 'dnapath_list' or 'dnapath'", 
         " object, a pathway list, or a vector of gene names.")
  }
  
  # Remove any names (which may be present when x is pathway).
  genes <- unname(genes)
  
  return(genes)
}


#' Remove pathways with non-significant DC scores.
#' 
#' @param x A 'dnapath_list' object from \code{\link{dnapath}}.
#' @param alpha_pathway Threshold for pathway p-values to determine significance.
#' If NULL, defaults to 0.05 or the minimum possible threshold (based on the
#' number of permutatiosn that were run).
#' @param monotonized If TRUE, monotonized p-values are used.
#' @return A 'dnapath_list' object containing only those pathways with differential
#' connectivity p-values below `alpha`.
#' @export
#' @examples 
#' data(meso)
#' data(p53_pathways)
#' set.seed(0)
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways,
#'                    group_labels = meso$groups, n_perm = 10)
#' results_sig <- filter_pathways(results)
filter_pathways <- function(x, alpha_pathway = NULL, monotonized = FALSE) {
  if(!is(x, "dnapath_list"))
    stop(deparse(substitute(x)), " is not a 'dnapath_list' object.")
  
  if(is.null(alpha_pathway)) {
    alpha_pathway <- max(0.05, get_min_alpha(x))
  }
  if(alpha_pathway < get_min_alpha(x)) {
    warning("alpha_pathway = ", alpha_pathway, " is too low given the number of ",
            "permutations. Setting to ", get_min_alpha(x))
    alpha_pathway <- get_min_alpha(x)
  }
  
  if(monotonized) {
    index <- which(sapply(x$pathway, 
                          function(path) path$p_value_path_mono <= alpha_pathway))
  } else {
    index <- which(sapply(x$pathway, 
                          function(path) path$p_value_path <= alpha_pathway))
  }
  
  if(length(index) > 0) {
    x <- subset(x, pathways = index)
  } else {
    return(NULL)
  }
  
  x <- sort(x)
  return(x)
}


#' Summarize differential connectivity of genes in a pathway
#' 
#' @param x A 'dnapath' object of a single pathway.
#' @param alpha Threshold for p-values of gene DC scores, used to
#' subset the x. If NULL (or 1), x for all genes are shown.
#' @param monotonized If TRUE, monotonized p-values are used.
#' @return A tibble summarizing the differential connectivity of genes in
#' the pathway.
#' @keywords internal
summarize_genes_for_pathway <- function(x, alpha = NULL, monotonized = FALSE) {
  if(!is(x, "dnapath"))
    stop(deparse(substitute(x)), " is not a 'dnapath' object.")
  
  if(is.null(alpha)) {
    alpha <- 1
  }
  if(alpha < get_min_alpha(x)) {
    warning("alpha = ", alpha, " is too low given the number of ",
            "permutations. Setting to ", get_min_alpha(x))
    alpha <- get_min_alpha(x)
  }
  
  genes <- get_genes(x)
  
  if(is.null(x$pathway$name)) x$pathway$name <- "unnamed"
  
  
  if(monotonized) {
    p_values <- x$pathway$p_value_genes_mono
  } else {
    p_values <- x$pathway$p_value_genes
  }
  
  if(any(is.na(p_values))) {
    # p_values will be NA if the inferred network is empty.
    index <- NULL
  } else {
    index <- which(p_values <= alpha)
  }
  
  if(length(index) == 0) {
    # If no genes meet the threshold, return an empty tibble.
    return(tibble::tibble(pathway = character(),
                          genes = character(),
                          dc_score = numeric(),
                          p_value = numeric(),
                          mean_expr1 = numeric(),
                          mean_expr2 = numeric()))
  }
  
  mean_expr <- get_mean_expr_mat(x)
  
  df <- tibble::tibble(pathway = x$pathway$name,
                       genes = genes[index],
                       dc_score = x$pathway$d_genes[index],
                       p_value = p_values[index],
                       mean_expr1 = mean_expr[1, index],
                       mean_expr2 = mean_expr[2, index])
  
  df <- dplyr::arrange(df, p_value, desc(pmax(mean_expr1, mean_expr2)))
  
  return(df)
}



#' Summarize the differential connectivity of pathways.
#' 
#' @param x A 'dnapath_list' object from \code{\link{dnapath}}.
#' @param alpha Threshold for p-values of pathway DC scores. 
#' Defaults to 1, which leads to results for all pathways being shown.
#' @param alpha_gene Threshold for p-values of gene DC scores. Used to determine
#' the number of genes that are differentially connected within each pathway.
#' Defaults to 0.1 or the minimum possible threshold for the number
#' of permutations performed, whichever is greater.
#' @param monotonized If TRUE, monotonized p-values are used.
#' @return A tibble summarizing the differential connectivity of genes in
#' the pathway.
#' @seealso 
#' \code{\link{summarize_genes}}, \code{\link{summarize_edges}}
#' @export
#' @examples 
#' data(meso)
#' data(p53_pathways)
#' set.seed(0)
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways,
#'                    group_labels = meso$groups, n_perm = 10)
#' summarize_pathways(results)
summarize_pathways <- function(x, alpha = 1, alpha_gene = 0.1,
                               monotonized = FALSE) {
  if(is(x, "dnapath")) {
    # If a single pathway is provided, return a summary of the edges.
    return(summarize_edges(x))
  } else if(!is(x, "dnapath_list")) {
    stop(deparse(substitute(x)), " is not a 'dnapath_list' or ",
         "'dnapath' object.")
  }
  
  if(is.null(alpha)) {
    alpha <- 1
  }
  if(alpha < get_min_alpha(x)) {
    warning("alpha = ", alpha, " is too low given the number of ",
            "permutations. Setting to ", get_min_alpha(x))
    alpha <- get_min_alpha(x)
  }
  
  if(monotonized) {
    p_value <- sapply(x$pathway, function(path) path$p_value_path_mono)
  } else {
    p_value <- sapply(x$pathway, function(path) path$p_value_path)
  }
  
  if(alpha == 1) {
    # Include pathways with NA p-values, which can result from empty networks.
    index <- 1:length(p_value)
  } else {
    index <- which(p_value <= alpha)
  }
  
  if(length(index) == 0) 
    # If no genes meet the threshold, return an empty tibble.
    return(tibble::tibble(pathway = character(),
                          dc_score = double(),
                          p_value = double(),
                          n_genes = integer(),
                          n_dc = integer(),
                          mean_expr1 = double(),
                          mean_expr2 = double()))
  
  # Remove any pathways that have high DC p-value.
  x <- x[index]
  pathway <- names(x)
  d_pathway <- sapply(x$pathway, function(x) x$d_pathway)
  p_value <- p_value[index]
  n_genes <- sapply(x$pathway, function(x) x$n_genes)
  
  if(is.null(alpha_gene)) {
    alpha_gene <- 0.1
  }
  if(alpha_gene < get_min_alpha(x)) {
    warning("alpha_gene = ", alpha_gene, " is too low given the number of ",
            "permutations. Setting to ", get_min_alpha(x))
    alpha_gene <- get_min_alpha(x)
  }
  
  if(monotonized) {
    n_genes_dc <- sapply(x$pathway, function(x) 
      sum(x$p_value_genes_mono <= alpha_gene, na.rm = TRUE))
  } else {
    n_genes_dc <- sapply(x$pathway, function(x) 
      sum(x$p_value_genes <= alpha_gene, na.rm = TRUE))
  }
  
  mean_expr1 <- rep(0, length(x))
  mean_expr2 <- rep(0, length(x))
  mean_expr <- get_mean_expr_mat(x)
  for(i in 1:length(x)) {
    index_genes <- x$pathway[[i]]$genes
    mean_expr1[i] <- mean(mean_expr[1, index_genes])
    mean_expr2[i] <- mean(mean_expr[2, index_genes])
  }
  
  # Note, mean_expr are divided by n_genes, the total number of possible genes
  # in the pathway. This way, if there are any missing genes from the expression
  # profile, they are counted as having 0 expression.
  
  df <- tibble::tibble(pathway = pathway,
                       dc_score = d_pathway,
                       p_value = p_value,
                       n_genes = n_genes,
                       n_dc = n_genes_dc,
                       mean_expr1 = mean_expr1,
                       mean_expr2 = mean_expr2)
  
  alpha_gene <- round(alpha_gene, 3)
  colnames(df)[5] <- paste0("n_dc (", ifelse(alpha_gene == 0, 
                                             "<0.001", alpha_gene), ")")
  
  return(df)
}


#' Summarize the differential connectivity of genes over all pathways.
#' 
#' @param x A 'dnapath_list' object from \code{\link{dnapath}}.
#' @param alpha Threshold for p-values of gene DC scores. Used to determine
#' the number of pathways that each gene is differentially connected in.
#' Defaults to 0.1 or the minimum possible threshold for the number
#' of permutations performed, whichever is greater.
#' @param monotonized If TRUE, monotonized p-values are used.
#' @return A tibble summarizing the differential connectivity of genes across
#' all pathways.
#' @seealso 
#' \code{\link{summarize_pathways}}, \code{\link{summarize_edges}}
#' @export
#' @examples 
#' data(meso)
#' data(p53_pathways)
#' set.seed(0)
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways,
#'                    group_labels = meso$groups, n_perm = 10)
#' summarize_genes(results) # Summary of genes across all pathways.
#' summarize_genes(results[[1]]) # Summary of genes within the first pathway.
summarize_genes <- function(x, alpha = 0.1, monotonized = FALSE) {
  if(is(x, "dnapath")) {
    return(summarize_genes_for_pathway(x, alpha, monotonized))
  } else if(!is(x, "dnapath_list")) {
    stop(deparse(substitute(x)), " is not a 'dnapath_list' or ",
         "'dnapath' object.")
  }
  
  if(is.null(alpha)) {
    alpha <- max(0.1, get_min_alpha(x))
  }
  if(alpha < get_min_alpha(x)) {
    warning("alpha = ", alpha, " is too low given the number of ",
            "permutations. Setting to ", get_min_alpha(x))
    alpha <- get_min_alpha(x)
  }
  
  # If there are no pathway results, return an empty table.
  if(length(x) == 0) {
    return(tibble::tibble(gene = character(0),
                          n_pathways = integer(0),
                          n_dc = double(0),
                          mean_expr1 = double(0),
                          mean_expr2 = double(0)))
  }
  
  gene_list <- x$param$gene_names
  n_pathways <- x$param$n_pathways_containing_gene
  p <- length(gene_list)
  mean_expr <- get_mean_expr_mat(x)
  
  n_dc <- integer(p)
  
  for(i in 1:length(x)) {
    index <- x$pathway[[i]]$genes
    # First check that p_values are not NA, which happens if inferred network is empty.
    if(!any(is.na(x$pathway[[i]]$p_value_genes))) {
      if(monotonized) {
        n_dc[index] <- n_dc[index] + (x$pathway[[i]]$p_value_genes_mono <= alpha)
      } else {
        n_dc[index] <- n_dc[index] + (x$pathway[[i]]$p_value_genes <= alpha)
      }
    }
  }
  
  # Create summary table.
  # Subset on genes that were significantly DC in at least 1 pathway.
  index <- n_pathways > 0
  df <- tibble::tibble(gene = gene_list[index],
                       n_pathways = n_pathways[index],
                       n_dc = n_dc[index],
                       mean_expr1 = mean_expr[1, index],
                       mean_expr2 = mean_expr[2, index])
  
  df <- dplyr::arrange(df, desc(n_dc), desc(pmax(mean_expr1, mean_expr2)))
  
  # Note, pathways containing a gene can be found by using:
  #  > subset(x, genes = gene_name)
  
  alpha <- round(alpha, 3)
  colnames(df)[3] <- paste0("n_dc (", 
                            ifelse(alpha == 0, "<0.001", alpha), 
                            ")")
  
  return(df)
}

#' Summarize differential connections for a pathway
#' 
#' @param x A 'dnapath' object from \code{\link{dnapath}}.
#' @param alpha Threshold for p-values of edge DC scores.
#' Defaults to 0.1 or the minimum possible threshold for the number
#' of permutations performed, whichever is greater.
#' @param monotonized If TRUE, monotonized p-values are used.
#' @param require_dc_genes If TRUE, the gene-level differential connectivity
#' p-value of the two genes for a given edge are also considered when deciding
#' whether an edge is differentially connected. If neither gene is significantly
#' differentially connected, then the edge between them will not be either.
#' @return A tibble summarizing the differential connections in
#' the pathway.
#' @seealso 
#' \code{\link{summarize_pathways}}, \code{\link{summarize_genes}}
#' @export
#' @examples 
#' data(meso)
#' data(p53_pathways)
#' set.seed(0)
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways,
#'                    group_labels = meso$groups, n_perm = 10)
#' summarize_edges(results[[1]])
summarize_edges <- function(x, alpha = 0.1, monotonized = FALSE, 
                            require_dc_genes = FALSE) {
  if(is(x, "dnapath_list"))
    stop("Summary of edges is not available for `dnapath_list` object. ",
         "Try again using a specific pathway, which can be done by ",
         "indexing the 'dnapath_list' object; for example ",
         deparse(substitute(x)), "[[1]] will access the first pathway.")
  if(!is(x, "dnapath"))
    stop(deparse(substitute(x)), " is not a 'dnapath' object.")
  
  if(is.null(alpha)) {
    alpha <- 0.1
  }
  if(alpha < get_min_alpha(x)) {
    warning("alpha = ", alpha, " is too low given the number of ",
            "permutations. Setting to ", get_min_alpha(x))
    alpha <- get_min_alpha(x)
  }
  
  # Note, d_edges, p_value_edges, nw1, nw2, etc. are lower tri of matrix,
  # and these fill the matrix by column.
  genes <- x$param$gene_names[x$pathway$genes]
  edges <- sapply(combn(length(genes), 2, simplify = FALSE), 
                  function(x) paste(genes[x[1]], genes[x[2]], sep = " - "))
  
  # Subset on x with p-values below alpha
  if(monotonized) {
    p_values <- x$pathway$p_value_edges_mono
    
    if(require_dc_genes) {
      # Calculate the min of the p-values for the gene-level differential
      # connectivity of the two genes corresponding to each edge.
      p_values_gene_min <- 
        apply(expand.grid(x$pathway$p_value_genes_mono, 
                          x$pathway$p_value_genes_mono)[1:length(p_values), ],
              1, min) 
    }
  } else {
    p_values <- x$pathway$p_value_edges
    
    if(require_dc_genes) {
      # Calculate the min of the p-values for the gene-level differential
      # connectivity of the two genes corresponding to each edge.
      p_values_gene_min <- 
        apply(expand.grid(x$pathway$p_value_genes, 
                          x$pathway$p_value_genes)[1:length(p_values), ],
              1, min) 
    }
  }
  
  if(any(is.na(p_values))) {
    index <- NULL
  } else {
    # Determine which edges are significantly differentially connected.
    is_dc_edge <- ((p_values <= alpha))
    if(require_dc_genes) {
      is_dc_edge <- is_dc_edge & (p_values_gene_min <= alpha)
    }
    index <- which(is_dc_edge)
  }
  
  
  
  if(length(index) == 0)
    # If no edges meet the threshold, return an empty tibble.
    return(tibble::tibble(pathway = character(),
                          edges = character(),
                          dc_score = numeric(),
                          p_value = numeric(),
                          nw1 = numeric(),
                          nw2 = numeric()))
  
  pathway <- names(x)
  if(is.null(pathway)) pathway <- "unnamed"
  
  df <- tibble::tibble(pathway = pathway,
                       edges = edges[index],
                       dc_score = x$pathway$d_edges[index],
                       p_value = p_values[index],
                       nw1 = x$pathway$nw1[index],
                       nw2 = x$pathway$nw2[index])
  
  return(df)
}


#' Get the minimum alpha level for the permutation test
#' 
#' This method is used internally by several methods to determine the 
#' minimum significance threshold (alpha value) that can be applied to the 
#' permutation p-values obtained in the differential network analysis. 
#' @param x A 'dnapath_list' or 'dnapath' object from \code{\link{dnapath}}.
#' @return The minimum alpha level that can be used based on the number
#' of permutations performed in the analysis.
#' @export
#' @examples 
#' data(meso)
#' data(p53_pathways)
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways,
#'                    group_labels = meso$groups, n_perm = 5)
#' get_min_alpha(results) # 1 / (5 + 1) = 0.167
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways,
#'                    group_labels = meso$groups, n_perm = 10)
#' get_min_alpha(results) # 1 / (10 + 1) = 0.091
get_min_alpha <- function(x) {
  if(is(x, "dnapath") || is(x, "dnapath_list")) {
    return(1 / (x$param$n_perm + 1))
  } else {
    return(NA)
  }
}

