#' @importFrom utils globalVariables
utils::globalVariables(c("p_value", "dc_score", "mean_expr1", "mean_expr2"))

#' Differential Network Analysis Using Gene Pathways
#' 
#' Integrates pathways into the differential network analysis of gene expression 
#' data \insertCite{grimes19}{dnapath}.
#' 
#' @param x The gene expression data to be analyzed. This can be either (1) a 
#' list of two matrices or data frames that contain the gene expression profile 
#' from each of two populations (groups) -- with rows corresponding to samples
#' and columns to genes -- or (2) a single matrix or data frame 
#' that contains the expression profiles for both groups. For case (2), the 
#' `group_labels` argument must be specified to identify which rows belong to which 
#' group.
#' @param pathway_list A single vector or list of vectors containing gene names 
#' to indicate pathway membership. The vectors are used to subset the columns
#' of the matrices in `x`. A pathway list can be obtained using 
#' \code{\link{get_reactome_pathways}}. If NULL, then the entire expression 
#' dataset is analyzed as a single network (this approach is not recommended 
#' unless there are only a small number of genes).
#' @param group_labels If `x` is a single matrix or data frame, `group_labels` must 
#' be specified to label each row. `group_labels` is a matrix each row 
#' corresponding to a in `x`. This matrix may either (1) have a single column
#' containing the group label for each observation, or (2) individual columns 
#' representing each group with values in `[0, 1]` representing the probability 
#' that the patient in that row is in each group. In the latter case, if the 
#' rows do not sum to 1, then each entry will be divided by its row sum. 
#' @param network_inference A function used to infer the pathway network. It
#' should take in an n by p matrix and return a p by p matrix of association 
#' scores. (Built-in options include: \code{\link{run_aracne}}, 
#' \code{\link{run_bc3net}}, \code{\link{run_c3net}},
#' \code{\link{run_clr}}, \code{\link{run_corr}}, \code{\link{run_genie3}}, 
#' \code{\link{run_glasso}}, \code{\link{run_mrnet}},
#' \code{\link{run_pcor}}, and \code{\link{run_silencer}}.) 
#' Defaults to \code{\link{run_pcor}} for partial correlations.
#' @param n_perm The number of random permutations to perform during 
#' permutation testing. If `n_perm == 1`, the permutation tests are not performed.
#' If `n_perm` is larger than the number of possible permutations, `n_perm` will
#' be set to this value with a warning message.
#' @param lp The lp value used to compute differential connectivity
#' scores. (Note: If a vector is provided, then the results are returned as
#' a list of `dnapath_list` objects, one result for each value of `lp`. This 
#' option is available so that network inference methods only need to be run 
#' once for each pathway when multple values of `lp` are being considered. This
#' may be useful when conducting simulation studies).
#' @param seed (Optional) Used to set.seed prior to permutation test for
#' each pathway. This allows results for individual pathways to be easily 
#' reproduced.
#' @param verbose Set to TRUE to turn on messages.
#' @param mc.cores Used in \code{\link[parallel]{mclapply}} to run the differential
#' network analysis in parallel across pathways. Must be set to 1 if on a Windows
#' machine.
#' @param ... Additional arguments are passed into the network inference function.
#' @return A 'dnapath_list' or 'dnapath' object containing results for each 
#' pathway in `pathway_list`.
#' @references 
#' \insertRef{grimes19}{dnapath}
#' @seealso 
#' \code{\link{filter_pathways}}, \code{\link{summary.dnapath_list}}
#' \code{\link{subset.dnapath_list}}, \code{\link{sort.dnapath_list}},
#' \code{\link{plot.dnapath}}, \code{\link{rename_genes}}
#' @export
#' @examples 
#' data(meso)
#' data(p53_pathways)
#' set.seed(0)
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways,
#'                    group_labels = meso$groups, n_perm = 10)
#' results
#' summary(results) # Summary over all pathways in the pathway list.
#' # Remove results for pathways with p-values above 0.2.
#' top_results <- filter_pathways(results, 0.2)
#' # Sort the top results by the pathway DC score.
#' top_results <- sort(top_results, by = "dc_score")
#' top_results
#' summary(top_results[[1]])  # Summary of pathway 1.
#' plot(results[[1]]) # Plot of the differential network for pathway 1.
#' 
#' # Use ... to adjust arguments in the network inference function.
#' # For example, using run_corr() with method = "spearman":
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways,
#'                    group_labels = meso$groups, n_perm = 10,
#'                    network_inference = run_corr,
#'                    method = "spearman")
#' results
dnapath <- function(x, pathway_list, group_labels = NULL,
                network_inference = run_pcor, n_perm = 100, lp = 2, 
                seed = NULL, verbose = FALSE, mc.cores = 1, ...) {
  
  #################################################################
  # Process the input. 
  # - Create a matrix of group probabilities from group_labels
  # - Coerce x into a matrix, if necessary.
  # - If pathway_list is a list, subset columns of x onto these genes.
  # - Check n_perm is not too large for the given sample size.
  #################################################################
  
  #
  # First process the group_labels argument, if provided.
  #
  if(!is.null(group_labels)) {
    # If group_labels is a vector, coerce into a matrix.
    if(is.null(dim(group_labels))) {
      group_labels <- matrix(group_labels, ncol = 1)
    }
    # If group_labels is a data frame, tibble, etc., coerce into matrix.
    group_labels <- tryCatch(as.matrix(group_labels),
                             error = function(e) 
                               stop("'group_labels' must be coercable into a matrix."))
    
    # Create the group probability matrix. Handle each type of input separately.
    if(ncol(group_labels) == 1) {
      if(is.numeric(group_labels)) {
        # If a numeric vector (of probabilities) was provided, then check that
        # the values are in [0, 1] and add the missing column.
        
        if(!all(group_labels >= 0 & group_labels <= 1)) {
          if(length(unique(group_labels)) == 1) {
            stop("The values in `group_labels` appear to be two unique numbers. ",
                 "For discrete group labels, please use characters (such as the",
                 "group names).")
          } else {
            stop("The values in `group_labels` must be in [0, 1].")
          }
        }
        group_prob <- matrix(0, nrow = nrow(group_labels), ncol = 2)
        group_prob[, 1] <- group_labels
        group_prob[, 2] <- 1 - group_labels
        
        # Add the group names, if provided. Otherwise, use "A" and "B".
        if(!is.null(colnames(group_labels))) {
          group_name <- colnames(group_labels)
          colnames(group_prob) <- c(group_name, paste("not", group_name))
        } else {
          colnames(group_prob) <- c("A", "B")
        }
      } else {
        # If a character vector was provided, then check that there are two unique
        # names and fill in the probability matrix with 1's and 0's.
        
        group_names <- as.character(unique(group_labels))
        if(length(unique(group_names)) != 2)
          stop("length(unique(group_labels)) = ", length(unique(group_names)), ", ",
               "expected to equal 2. For example, if the first n rows ",
               'are from group "a" and the last m rows are from group "b", then ',
               "'x' should contain n + m rows and 'group' may be the ",
               'vector c(rep("a", n), rep("b", m)).')
        group_prob <- matrix(0, nrow = nrow(group_labels), ncol = 2)
        group_prob[group_labels == group_names[1], 1] <- 1
        group_prob[group_labels == group_names[2], 2] <- 1
        colnames(group_prob) <- group_names
      }
      #
      # End of case with ncol(group_labels) = 1
      #
    } else if(ncol(group_labels) == 2) {
      if(!is.numeric(group_labels))
        stop("Argument `group_labels` should be numeric when provided as a ",
             "two column matrix.")
      row_totals <- rowSums(group_labels)
      if(any(group_labels < 0)) {
        stop("The values in `group_labels` should be non-negative.")
      }
      if(any(row_totals <= 0)) {
        stop("The rows in `group_labels` do not sum to a positive number.")
      }
      group_prob <- group_labels / row_totals
      
      #
      # End of case with ncol(group_labels) = 2
      #
    } else {
      stop("'group_labels' contains more than two columns.")
    }
  }
  
  #
  # Process x: handle the list and single matrix cases separately.
  #
  if(inherits(x, "list")) {
    # Expecting x to be a list of two matrices.
    if(length(x) != 2) 
      stop("'x' is a list but does not contain two matrices.")
    if(!is.null(group_labels)) 
      warning("`group_labels` ignored when `x` is a list.")
    
    gene_freq <- table(colnames(x[[1]]))
    if(any(gene_freq > 1)) {
      stop('Multiple columns found in `x[[1]]` for the following genes: "', 
           paste0(names(gene_freq)[which(gene_freq > 1)], collapse = '", "'), 
           '".', " The column names should be unique; one column per gene.")
    }
    gene_freq <- table(colnames(x[[2]]))
    if(any(gene_freq > 1)) {
      stop('Multiple columns found in `x[[2]]` for the following genes: "', 
           paste0(names(gene_freq)[which(gene_freq > 1)], collapse = '", "'), 
           '".', " The column names should be unique; one column per gene.")
    }
    
    # Save the group names, if there are any.
    group_names <- names(x)
    if(is.null(group_names)) 
      group_names <- c("1", "2")
    
    # Store the number of samples in each dataset.
    n <- c(nrow(x[[1]]), nrow(x[[2]]))
    
    # Create the group probabilities matrix.
    group_prob <- cbind(c(rep(1, n[1]), rep(0, n[2])), 
                         c(rep(0, n[1]), rep(1, n[2])))
    colnames(group_prob) <- group_names
    
    # Join the two datasets and coerce the output into a matrix.
    x <- as.matrix(merge(x[[1]], x[[2]], all = TRUE, sort = FALSE))

  } else {
    # First check that `group_labels` is provided.
    # The `group_prob` variable will have already been created.
    if(is.null(group_labels)) 
      stop("'group_labels' must be specified when 'x' is a single matrix.")
    
    # `x` is assumed to be a matrix, but allow data.frame, tibble, and other 
    # objects that are coercible to matrix.
    x <- tryCatch(as.matrix(x),
                  error = function(e) 
                    stop("'x' is not a list and as.matrix() failed."))
    
    # `group_labels` are already processed, so `group_prob` has been created.
    if(nrow(group_prob) != nrow(x))
      stop("The length of `group_labels` does not match the number of rows in `x`")

    gene_freq <- table(colnames(x))
    if(any(gene_freq > 1)) {
      stop('Multiple columns found in `x` for the following genes: "', 
           paste0(names(gene_freq)[which(gene_freq > 1)], collapse = '", "'), 
           '".', " The column names should be unique; one column per gene.")
    }
    
    if(all(group_prob %in% c(0, 1))) {
      # Rearrange rows of x with the first n[1] observations of group 1
      # followed by the n[2] observations from group 2.
      index_n1 <- which(group_prob[, 1] == 1)
      index_n2 <- which(group_prob[, 2] == 1)
      x <- x[c(index_n1, index_n2), ]
      group_prob[c(index_n1, index_n2), ]
    } else {
      # Rearrange rows of x in descending order of group 1 probability.
      index_order <- order(group_prob[, 1], decreasing = TRUE)
      x <- x[index_order, ]
      group_prob <- group_prob[index_order, ]
    }
  }
  
  #
  # `x` has been processed into a single matrix and `group_prob` is created.
  #
  
  # Check that `x` is numeric. 
  if(!is.numeric(x)) {
    stop("Non-numeric columns found in 'x'.")
  }
  
  # Fill in missing values with 0
  index_na <- which(is.na(x))
  if(length(index_na) > 0) {
    # Note: these may occur when merging the two datasets from a list.
    x[index_na] <- 0
  }
  index_nan <- which(is.nan(x))
  if(length(index_nan) > 0) {
    warning("Setting ", length(index_nan), " NaN gene expression values to 0.")
    x[index_nan] <- 0
  }
  if(is.null(colnames(x))) {
    stop("The gene expression data do not contain column names (genes names).")
  }
  
  #
  # Initialize the permutations, which is stored as an N by n_perm matrix.
  #
  
  # Set seed for reproducibility, if one is provided.
  if(!is.null(seed))
    set.seed(seed)
    
  #
  # If group labels are provided (i.e. all probabilities are either 1 or 0),
  # then we need to be more careful with the permutation testing to ensure
  # each permutation swaps at least one observation across groups (as opposed
  # to only permuting within groups, which can happen just by chance).
  #
  N <- nrow(x)
  if(all(group_prob[, 1] == 1 | group_prob[, 1] == 0)) {
    n <- colSums(group_prob) # Sample size within each group.
    # Check that n_perm is achievable with given sample size.
    if(n[1] == n[2]) {
      n_perm_max <- choose(N, n[1]) / 2 - 1
    } else {
      n_perm_max <- choose(N, n[1]) - 1
    }
    if(n_perm_max < n_perm) {
      warning("Only ", n_perm_max, " permutations are possible with the given ",
              "sample size. Setting 'n_perm' to this value.")
      n_perm <- n_perm_max
    }
    
    # If group classes are provided, then check sample sizes when
    # creating the permutations. If total number of observations is small, use 
    # exact set of permutations. Order of samples doesn't matter, so keep only 
    # first half if sample sizes are the same. If sample sizes are unequal, all 
    # permutations are used.
    if((n[1] == n[2]) && (choose(N, n[1]) / 2 <= n_perm)) {
      permutations <- combn(1:N, n[1])
      permutations <- permutations[, 1:(ncol(permutations) / 2)] 
      permutations <- permutations[, -1] # The first column is 1:n[1].
    } else if((n[1] != n[2]) && (choose(N, n[1]) <= n_perm)) {
      permutations <- combn(1:N, n[1])
      permutations <- permutations[, -1] # The first column is 1:n[1].
    } else {
      permutations <- cbind(sapply(1:n_perm, function(x) sample(1:N, n[1])))
    }
    permutations <- apply(permutations, 2, function(perm) c(perm, setdiff(1:N, perm)))
    # Obtain the row indices for each group and map those to the permutations.
    index <- c(which(group_prob[, 1] == 1), 
               which(group_prob[, 2] == 1))
    permutation <- apply(permutations, 2, function(i) i[index])
  } else { # `group_prob` contains probabilities.
    n <- nrow(x) # Total sample size.
    n_perm_max <- factorial(N) - 1
    if(n_perm_max < n_perm) {
      warning("Only ", n_perm_max, " permutations are possible with the given ",
              "sample size. Setting 'n_perm' to this value.")
      n_perm <- n_perm_max
    }
    if(n_perm_max == n_perm) {
      # Obtain all the permutations.
      permutations <- t(gtools::permutations(N, N))[, -1]
      permutations <- permutations[, -1] # The first column is 1:N.
    } else {
      # Sample `n_perm` permutations (randomization test).
      permutations <- cbind(sapply(1:n_perm, function(x) sample(1:N, N)))
    }
  }
  
  #################################################################
  # Perform dnapath over each pathway in the pathway list.
  #################################################################
  if(is.null(pathway_list)) {
    # If no pathway list is provided, use all the genes in the dataset.
    pathway_list <- list(colnames(x))
  } else if(!is.list(pathway_list)) {
    # If a single pathway is provided, coerce it into a list.
    pathway_list <- list(pathway_list)
  }
  
  # Make sure there are no duplicate genes in any pathway.
  pathway_list <- lapply(pathway_list, unique)
  
  # Subset `x` onto the genes within the pathway list.
  # Subset the expression data onto those genes contained in pathways.
  genes <- unique(unlist(pathway_list))
  n_total_genes <- ncol(x)
  x <- x[, colnames(x) %in% genes]
  gene_names <- colnames(x)
  
  if(length(gene_names) == 0) 
    stop("None of the genes in `x` are in any pathway in `pathway_list`.")
  
  results_by_pathway <- parallel::mclapply(pathway_list, function(pathway) {
    dna_pathway(x, pathway, group_prob, network_inference, permutations, 
                lp, verbose, ...)
  }, mc.cores = mc.cores)
  
  # Determine which pathways returned results.
  index_null <- which(sapply(results_by_pathway, is.null))
  index_pathways <- setdiff(1:length(pathway_list), index_null) 
  
  # Store the number of pathways in original pathway list.
  n_pathways <- length(pathway_list)
  
  # If all pathways returned NULL results, stop here and return NULL.
  if(length(index_null) == n_pathways) {
    warning("No pathways were present in the gene expression data. ",
            "Note: this may be due to different gene names used in ",
            "the pathway list and the expression data.")
    return(NULL)
  }
  
  # Count the number of pathways containing each gene.
  counts <- rep(0, length(gene_names))
  for(i in index_pathways) {
    index_genes <- match(pathway_list[[i]], gene_names)
    if(any(is.na(index_genes))) {
      index_genes <- index_genes[!is.na(index_genes)]
    }
    counts[index_genes] <- counts[index_genes] + 1
  }
  
  
  # If multiple lp values were used, we will re-arrange the results as a list 
  # of  dnapath_list objects by lp value.
  results_by_lp <- vector("list", length(lp))
  
  if(length(lp) == 1) {
    # lapply() is used later to handle the multiple lp values case.
    # The pathway results are made into a list so that the indexing works later.
    results_by_pathway <- lapply(results_by_pathway, function(results) list(results))
  }
  
  for(i in 1:length(lp)) {
    # Extract the dnapath results for the i'th lp value.
    results_by_lp[[i]] <- list(pathway = NA, param = NA)
    results_by_lp[[i]]$pathway <- 
      lapply(results_by_pathway, function(results) results[[i]]$pathway)
    results_by_lp[[i]]$param <- 
      list(gene_names = gene_names,
           n_pathways = n_pathways,
           n_total_genes = n_total_genes,
           n_pathways_containing_gene = counts,
           n = n,
           groups = colnames(group_prob),
           group_prob = group_prob,
           network_inference = network_inference,
           n_perm = n_perm,
           lp = lp[i],
           seed = seed,
           x = x,
           extra_params = list(...))
    
    # Add pathway name to each result, if pathway_list is named.
    if(!is.null(names(pathway_list))) {
      for(j in index_pathways) {
        results_by_lp[[i]]$pathway[[j]]$name <- names(pathway_list)[j]
      }
    } 
    
    class(results_by_lp[[i]]) <- "dnapath_list"
    if(n_pathways == 1) {
      # Subset on first pathway, which will turn this into a "dnapath" object.
      results_by_lp[[i]] <- results_by_lp[[i]][[1]]
    }
  }
  
  # Remove pathways that returned NULL results.
  if(length(index_null) > 0) {
    results_by_lp <- lapply(results_by_lp, function(results) {
      results <- results[-index_null]
      return(results)
    })
  }
  
  if(length(lp) == 1) {
    results_by_lp <- results_by_lp[[1]]
  } else {
    names(results_by_lp) <- paste0("lp = ", lp)
  }
  
  return(results_by_lp)
}



#' Differential network analysis on a single pathway
#' 
#' This is an internal function used by \code{\link{dnapath}} to conduct
#' the differential network analysis on a single pathway.
#' 
#' @param x The gene expression data to be analyzed. Assumed to already be
#' processed by \code{\link{dnapath}}
#' @param pathway A single vector containing gene names.
#' @param group_prob A matrix with number of rows equal to the 
#' number of rows in `x` and two columns, each containing the probability that
#' a given observation belongs to each group.
#' @param network_inference A function used to infer the pathway network. 
#' @param permutations A matrix containing the permutations to perform during 
#' permutation testing. If `NULL`, the permutation tests are not performed.
#' @param lp The lp value used to compute differential connectivity
#' scores. (Note: If a vector is provided, then the results are returned as
#' a list of `dnapath_list` objects, one result for each value of `lp`. This 
#' option is available so that network inference methods only need to be run 
#' once for each pathway when multple values of `lp` are being considered. This
#' may be useful when conducting simulation studies).
#' @param verbose Set to TRUE to turn on messages.
#' @param ... Additional arguments are passed into the network inference function.
#' @return A list containing the results of the differential network analysis
#' for a single pathway.
#' @keywords internal
dna_pathway <- function(x, pathway, group_prob, network_inference, permutations, 
                        lp = 2, verbose = FALSE, ...) {
  
  #################################################################
  # Run differential network analysis on the pathway
  #################################################################
  
  gene_names <- colnames(x)
  pathway_genes <- which(gene_names %in% pathway)
  if(length(pathway_genes) < 2) {
    if(verbose)
      cat("Fewer than two genes are expressed in a pathway. Returning NULL.\n")
    return(NULL)
  }
  
  x <- x[, pathway_genes]
  # Store the number of genes in the pathway.
  # Note, this may not equal length(pathway_genes).
  n_genes <- length(pathway)
  
  n_perm <- ncol(permutations)
  p <- ncol(x) # Total number of genes in union.
  n_lp <- length(lp)
  
  # Estimate the network for each group using the original data.
  scores_1 <- network_inference(x, weights = group_prob[, 1], ...)
  scores_2 <- network_inference(x, weights = group_prob[, 2], ...)
  
  # Initialize lists to store DC scores and p-values for each lp in lp.
  # Scores are initialized to 0 and p-values to 1.
  d_path_list <- lapply(lp, function(lp) d_pathwayC(scores_1, scores_2, lp = lp))
  d_gene_list <- lapply(lp, function(lp) d_genesC(scores_1, scores_2, lp = lp))
  d_edge_list <- lapply(lp, function(lp) d_edgesC(scores_1, scores_2, lp = lp))
  
  # Obtain ranks of original DC scores for monotonization.
  d_path_ranks <- lapply(d_path_list, function(d_path) order(d_path, decreasing = TRUE))
  d_gene_ranks <- lapply(d_gene_list, function(d_gene) order(d_gene, decreasing = TRUE))
  d_edge_ranks <- lapply(d_edge_list, function(d_edge) order(d_edge, decreasing = TRUE))
  
  pval_path_list <- lapply(lp, function(lp) 1)
  pval_gene_list <- lapply(lp, function(lp) rep(1, p))
  pval_edge_list <- lapply(lp, function(lp) rep(1, p * (p - 1) / 2))
  
  pval_path_list_mono <- lapply(lp, function(lp) 1)
  pval_gene_list_mono <- lapply(lp, function(lp) rep(1, p))
  pval_edge_list_mono <- lapply(lp, function(lp) rep(1, p * (p - 1) / 2))
  
  # Save inferred networks.
  nw1 <- scores_1[lower.tri(scores_1)]
  nw2 <- scores_2[lower.tri(scores_2)]
  
  # If inferred networks are empty, then quit without running permutation test.
  if(all(scores_1 == 0) && all(scores_2 == 0)) {
    if(verbose)
      cat("Inferred network is empty. Permutation test not performed.\n")
    n_perm <- 0 # Only original data were considered.
  } else if(n_perm > 0) {
    # Iterate over each permutation.
    for(i in 1:n_perm) {
      # Permute the rows of the group probabilities and re-estimate the networks.
      group_prob_perm <- group_prob[permutations[, i], ]
      scores_1 <- network_inference(x, weights = group_prob_perm[, 1], ...)
      scores_2 <- network_inference(x, weights = group_prob_perm[, 2], ...)

      for(k in 1:n_lp) {
        pval_path_list[[k]] <- pval_path_list[[k]] +
          (d_pathwayC(scores_1, scores_2, lp = lp[[k]]) >= d_path_list[[k]])
        pval_gene_list[[k]] <- pval_gene_list[[k]] +
          (d_genesC(scores_1, scores_2, lp = lp[[k]]) >= d_gene_list[[k]])
        pval_edge_list[[k]] <- pval_edge_list[[k]] +
          (d_edgesC(scores_1, scores_2, lp = lp[[k]]) >= d_edge_list[[k]])
        
        # Compute step-down p-values.
        # Rank index for original scores:
        index <- d_path_ranks[[k]]
        n_index <- length(index)
        # Calculate scores of permuted data.
        d_path_perm <- d_pathwayC(scores_1, scores_2, lp = lp[[k]])
        # Take the cumulative max, in rank order, starting from the min.
        d_path_perm <- cummax(d_path_perm[rev(index)])[n_index:1][order(index)]
        # Accumulate number of permuted scores that are larger than original.
        pval_path_list_mono[[k]] <- pval_path_list_mono[[k]] +
          (d_path_perm >= d_path_list[[k]])
        
        # Repeat step-down procedure for genes and edges.
        index <- d_gene_ranks[[k]]
        n_index <- length(index)
        d_gene_perm <- d_genesC(scores_1, scores_2, lp = lp[[k]])
        d_gene_perm <- cummax(d_gene_perm[rev(index)])[n_index:1][order(index)]
        pval_gene_list_mono[[k]] <- pval_gene_list_mono[[k]] +
          (d_gene_perm >= d_gene_list[[k]])
        
        index <- d_edge_ranks[[k]]
        n_index <- length(index)
        d_edge_perm <- d_edgesC(scores_1, scores_2, lp = lp[[k]])[index]
        d_edge_perm <- cummax(d_edge_perm[rev(index)])[n_index:1][order(index)]
        pval_edge_list_mono[[k]] <- pval_edge_list_mono[[k]] +
          (d_edge_perm >= d_edge_list[[k]])
      }
    }
  }
  
  # If no permutation testing is done, set p-values to NA.
  if(n_perm == 0) {
    pval_path_list <- lapply(pval_path_list, function(pval) NA)
    pval_gene_list <- lapply(pval_gene_list, function(pval) NA)
    pval_edge_list <- lapply(pval_edge_list, function(pval) NA)
    
    pval_path_list_mono <- lapply(pval_path_list_mono, function(pval) NA)
    pval_gene_list_mono <- lapply(pval_gene_list_mono, function(pval) NA)
    pval_edge_list_mono <- lapply(pval_edge_list_mono, function(pval) NA)
  } else {
    pval_path_list <- lapply(pval_path_list, function(pval) pval / (n_perm + 1))
    pval_gene_list <- lapply(pval_gene_list, function(pval) pval / (n_perm + 1))
    pval_edge_list <- lapply(pval_edge_list, function(pval) pval / (n_perm + 1))
    
    for(k in 1:length(lp)) {
      index <- d_path_ranks[[k]]
      pval_path_list_mono[[k]] <- 
        cummax(pval_path_list_mono[[k]][index] / (n_perm + 1))[order(index)]
      index <- d_gene_ranks[[k]]
      pval_gene_list_mono[[k]] <- 
        cummax(pval_gene_list_mono[[k]][index] / (n_perm + 1))[order(index)]
      index <- d_edge_ranks[[k]]
      pval_edge_list_mono[[k]] <- 
        cummax(pval_edge_list_mono[[k]][index] / (n_perm + 1))[order(index)]
    }
  }
  
  results <- vector("list", n_lp)
  for(i in 1:n_lp) {
    results[[i]] <- 
      list(pathway = list(name = NULL,
                          genes = pathway_genes,
                          n_genes = n_genes,
                          p_value_path = pval_path_list[[i]],
                          p_value_genes = drop(pval_gene_list[[i]]),
                          p_value_edges = drop(pval_edge_list[[i]]),
                          p_value_path_mono = pval_path_list_mono[[i]],
                          p_value_genes_mono = pval_gene_list_mono[[i]],
                          p_value_edges_mono = pval_edge_list_mono[[i]],
                          d_pathway = d_path_list[[i]],
                          d_genes = drop(d_gene_list[[i]]),
                          d_edges = drop(d_edge_list[[i]]),
                          nw1 = nw1,
                          nw2 = nw2),
           param = NULL)
  }
  
  # If only one lp is considered, unlist over lp.
  if(length(results) == 1) {
    results <- results[[1]]
  }
  
  return(results)
}

#' Print function for 'dnapath_list' object.
#' 
#' @param x A 'dnapath_list' object from \code{\link{dnapath}}.
#' @param ... Additional arguments are ignored.
#' @return Prints a summary of the module.
#' @export
print.dnapath_list <- function(x, ...) {
  if(!is(x, "dnapath_list")) 
    stop(paste0("'", deparse(substitute(x)), 
                "' is not a 'dnapath_list' object."))
  
  groups <- x$param$groups
  
  message <- paste0("Differential network analysis results between ", groups[1], 
                    " (group 1) and ", groups[2], " (group 2) over ", length(x), 
                    " out of ", x$param$n_pathways, " pathways analyzed.\n")
  cat(message)
  print(summary(x))
}

#' Print function for 'dnapath' object.
#' 
#' @param x A 'dnapath' object from \code{\link{dnapath}}.
#' @param ... Additional arguments are ignored.
#' @return Prints a summary of the module.
#' @export
print.dnapath <- function(x, ...) {
  if(!is(x, "dnapath")) 
    stop(paste0("'", deparse(substitute(x)), 
                "' is not a 'dnapath' object."))
  
  pathway <- drop(x$pathway$name)
  p <- length(x$pathway$genes)
  n_pathways <- x$param$n_pathways
  groups <- x$param$groups
  
  message <- paste0("Differential network analysis between ", groups[1], 
                    " (group 1) and ", groups[2], " (group 2).\n")
  
  if(is.null(pathway)) {
    message <- paste0(message, "Results for an unnamed ", "pathway", 
                      " (out of ", n_pathways, " pathways analyzed) ",
                      "containing ", p, " genes.\n")
  } else {
    message <- paste0(message, 'Results for the pathway "', pathway, 
                      '" (out of ', n_pathways, " pathways analyzed) ",
                      "containing ", p, " genes.\n")
  }
  
  # Add pathway p-value to the message. 
  p_value <- round(x$pathway$p_value_path, 3)
  if(is.na(p_value)) {
    p_value <- "is NA because the inferred group-specific networks are empty"
  } else if(p_value == 0) {
    p_value <- "< 0.001"
  } else {
    p_value <- paste("=", p_value)
  }
  
  message <- paste0(message, 'Pathway p-value ', p_value, ".")
  
  mean_expr <- get_mean_expr_mat(x)
  
  message <- paste0(message, " The mean expression of genes in the pathway is ",
                    round(mean(mean_expr[1, ]), 1), " in group 1 and ",
                    round(mean(mean_expr[2, ]), 1), " in group 2.\n")
  
  cat(message)
  if(!is.na(x$pathway$p_value_path))
    print(summary(x))
}


#' Summary function for 'dnapath_list' object.
#' 
#' @param object A 'dnapath_list' object from \code{\link{dnapath}}.
#' @param by_gene If TRUE, summarizes the differential network analysis by genes
#' instead of by pathways.
#' @param alpha_pathway Threshold for p-values of pathway DC scores; used 
#' to subset the results. If NULL (or 1), results for all pathways are shown.
#' @param alpha_gene Threshold for p-values of gene DC scores. Used to determine
#' the number of genes that are differentially connected within each pathway.
#' Defaults to 0.1 or the minimum possible threshold for the number
#' of permutations performed, whichever is greater.
#' @param monotonized If TRUE, monotonized p-values are used.
#' @param ... Additional arguments are ignored.
#' @return Summarizes the differential network analysis results.
#' @seealso 
#' \code{\link{summarize_pathways}}, \code{\link{summarize_genes}}
#' @export
#' @examples 
#' data(meso)
#' data(p53_pathways)
#' set.seed(0)
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways,
#'                    group_labels = meso$groups, n_perm = 10)
#' summary(results) # Summary across all pathways in the analysis.
summary.dnapath_list <- function(object, by_gene = FALSE, 
                             alpha_pathway = 1, alpha_gene = 0.1, 
                             monotonized = FALSE, ...) {
  if(is.null(alpha_gene)) {
    alpha_gene <- max(0.1, get_min_alpha(object))
  }
  if(alpha_gene < get_min_alpha(object)) {
    warning("alpha_gene = ", alpha_gene, " is too low given the number of ",
            "permutations. Setting to ", get_min_alpha(object))
    alpha_gene <- get_min_alpha(object)
  }
  
  if(is.null(alpha_pathway)) {
    alpha_pathway <- 1
  }
  if(alpha_pathway < get_min_alpha(object)) {
    warning("alpha_pathway = ", alpha_pathway, " is too low given the number of ",
            "permutations. Setting to ", get_min_alpha(object))
    alpha_pathway <- get_min_alpha(object)
  }
  
  if(by_gene) {
    df <- summarize_genes(object, 
                          alpha = alpha_gene,
                          monotonized = monotonized)
  } else {
    df <- summarize_pathways(object, 
                             alpha = alpha_pathway, 
                             alpha_gene = alpha_gene,
                             monotonized = monotonized)
  }
  
  return(df)
}

#' Summary function for 'dnapath' object.
#' 
#' @param object A 'dnapath' object from \code{\link{dnapath}}.
#' @param by_gene If TRUE, summarizes the differential network analysis by genes;
#' otherwise, summarizes by gene-gene interactions.
#' @param alpha Threshold for p-values to determine significance; defaults 
#' to 1 and returns all results. If 'by_gene'
#' is FALSE, then 'alpha' is used to filter edges. If 'by_gene' is TRUE,
#' then 'alpha' is used to filter genes. 
#' @param monotonized If TRUE, monotonized p-values are used.
#' @param ... Additional arguments are ignored.
#' @return Summarizes the differential network analysis result.
#' @seealso 
#' \code{\link{summarize_genes}}, \code{\link{summarize_edges}}
#' @export
#' @examples
#' data(meso)
#' data(p53_pathways)
#' set.seed(0)
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways,
#'                    group_labels = meso$groups, n_perm = 10)
#' summary(results[[1]]) # Summary of the first pathway in the results.
summary.dnapath <- function(object, by_gene = TRUE, alpha = 1, 
                            monotonized = FALSE, ...) {
  if(is.null(alpha)) {
    alpha <- 1
  }
  if(alpha < get_min_alpha(object)) {
    warning("alpha = ", alpha, " is too low given the number of ",
            "permutations. Setting to ", get_min_alpha(object))
    alpha <- get_min_alpha(object)
  }
  
  if(by_gene) {
    df <- summarize_genes_for_pathway(object, 
                                      alpha = alpha,
                                      monotonized = monotonized)
  } else {
    df <- summarize_edges(object, 
                          alpha = alpha,
                          monotonized = monotonized)
  }
  
  df <- dplyr::arrange(df, p_value, desc(dc_score))
  
  return(df)
}


#' Subset function for 'dnapath_list' object.
#' 
#' @param x A 'dnapath_list' object from \code{\link{dnapath}}.
#' @param pathways A set of pathways to index on. This can be (1) a vector of 
#' character strings, corresponding to 
#' pathway names or regular expressions used to find pathways, (2) a vector of 
#' indices to select pathways, (3) a vector of 
#' negative indices indicating pathways to remove, or (4) a logical (boolean) 
#' vector  that is the same length of current number of pathways in `x`.
#' @param genes A set of gene names to index on; exact matching is used. Only 
#' pathways containing these genes are retained. 
#' @param ... Additional arguments are ignored.
#' @return A subset of the differential network analysis results.
#' @export
#' @examples
#' data(meso)
#' # Obtain a pathway list for this short example:
#' pathway_list <- get_reactome_pathways("human", overlap_limit = NULL,
#'                                       min_size = 13, max_size = 19)
#' # Run the differential network analysis.
#' results <- dnapath(x = meso$gene_expression, pathway_list = pathway_list,
#'                    group_labels = meso$groups, n_perm = 5, seed = 0)
#' summary(results) # Summary over all pathways in the pathway list.
#' 
#' # Subset on pathways that contain "cell cycle" in its name.
#' cell_cycle_pathways <- subset(results, pathways = "cell cycle")
#' summary(cell_cycle_pathways)
#' # Subset on pathways that contain the gene 1026 (Entrezgene ID).
#' pathways_with_1026 <- subset(results, genes = "1026")
#' summary(pathways_with_1026)
#' 
#' # Multiple pathways and/or genes can also be specified. 
#' # Specifying both acts as an "OR" operation. For example, the following subset 
#' # will contain pathways containing the words "acetylation" or "methylation" 
#' # OR pathways that contain the genes "1108" or "11200". 
#' results_OR <- subset(results, 
#'                      pathways = c("acetylation", "methylation"),
#'                      genes = c("1108", "11200"))
#' summary(results_OR)
#' # To subset on pathways that have both a specific pathway name AND
#' # certain genes, call the subset function twice: once specifying the
#' # `pathways` argument, then pass those results back into subset() with the
#' # `genes` argument specified. For example:
#' results_AND <- subset(results, 
#'                       pathways = c("acetylation", "methylation"))
#' results_AND <- subset(results_AND,
#'                       genes = c("1108", "11200"))
#' summary(results_AND)
subset.dnapath_list <- function(x, pathways = NULL, genes = NULL, ...) {
  # Pathways to keep or remove. These are used so that both pathways and genes
  # can be provided, and the union of results is returned.
  index_keep <- rep(FALSE, length(x$pathway))
  index_remove <- rep(FALSE, length(x$pathway)) # Only used for -pathway input.
  
  if(!is.null(pathways)) {
    if(is.numeric(pathways)) {
      if(all(pathways > 0)) {
        index_keep[pathways] <- TRUE
      } else if(all(pathways < 0)) {
        index_remove[pathways] <- TRUE
      } else {
        stop("pathway indices must be all postive or all negative.")
      }
    } else if(is.logical(pathways)) {
      if(length(pathways) == length(x)) {
        index_keep[pathways] <- TRUE
      } else {
        stop("pathway is a logical vector but is not same length as the current",
             "number of pathways in 'x'.")
      }
    } else if(is.character(pathways)) {
      for(pathway in pathways) {
        index <- which(
          grepl(pathway, names(x), ignore.case = TRUE) |
            names(x) == pathway)
        if(length(index) > 0)
          index_keep[index] <- TRUE
      }
    }
  }
  
  gene_names <- x$param$gene_names
  
  if(!is.null(genes)) {
    for(gene in genes) {
      index <- which(sapply(x$pathway, function(path) 
        any(gene_names[path$genes] == gene)))
      if(length(index) > 0)
         index_keep[index] <- TRUE
    }
  }
  
  # If pathways were specified for removal, update index_keep.
  if(sum(index_remove) > 1) {
    if(!is.null(genes)) {
      # If pathways were subset on genes, then update the index_keep array.
      index_keep[index_remove] <- FALSE
    } else {
      # Otherwise, keep all pathways that were not specified for removal.
      index_keep[!index_remove] <- TRUE
    }
  }
  
  x <- x[index_keep]
  return(x)
}


#' Sort function for 'dnapath_list' object.
#' 
#' @param x A 'dnapath_list' object from \code{\link{dnapath}}.
#' @param decreasing Logical. If TRUE (the default), results are sorted in 
#' decreasing order.
#' @param by The variable to sort the results by. Must be one of: "mean_expr", 
#' the mean expression of each pathway across both groups; "mean_expr1"
#' or "mean_expr2", the mean expression of each pathway in group
#' 1 or 2, respectively; "dc_score", the differential connectivity score
#' of the pathway; "p_value", the p-value of the dc score; "n_genes", 
#' the number of genes in each pathway; "pathway", the pathway names; or 
#' "n_dc" the number of significantly differentially conncted genes in each 
#' pathway.
#' @param ... Additional arguments are ignored.
#' @return The differential network analysis results ordered by DC pathway score.
#' @export
#' @examples
#' data(meso)
#' data(p53_pathways)
#' set.seed(0)
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways,
#'                    group_labels = meso$groups, n_perm = 10)
#' # Filter out pathways that have p-values above 0.2.
#' results_sig <- filter_pathways(results, 0.2) 
#' sort(results_sig, by = "dc_score") # Sort by the pathway DC score.
#' sort(results_sig, by = "n_genes") # Sort by the pathway size.
#' sort(results_sig, by = "mean_expr") # Sort by the mean expression.
sort.dnapath_list <- function(x, decreasing = TRUE, by = "dc_score", ...) {
  if(!is(x, "dnapath_list"))
    stop(deparse(substitute(x)), " is not a 'dnapath_list' object.")
  
  by <- tolower(by[1])
  
  if(by == "mean_expr") {
    mean_expr <- get_mean_expr_mat(x)
    index <- order(sapply(x$pathway, function(path) mean(mean_expr[, path$genes])), 
                   decreasing = decreasing)
  } else if(by == "mean_expr1") {
    mean_expr <- get_mean_expr_mat(x)
    index <- order(sapply(x$pathway, function(path) mean(mean_expr[1, path$genes])), 
                   decreasing = decreasing)
  } else if(by == "mean_expr2") {
    mean_expr <- get_mean_expr_mat(x)
    index <- order(sapply(x$pathway, function(path) mean(mean_expr[2, path$genes])), 
                   decreasing = decreasing)
  } else if(by == "dc_score") {
    index <- order(sapply(x$pathway, function(path) path$d_pathway), 
                   decreasing = decreasing)
  } else if(by == "p_value") {
    index <- order(sapply(x$pathway, function(path) path$p_value_path), 
                   decreasing = decreasing)
  } else if(by == "n_genes") {
    index <- order(sapply(x$pathway, function(path) length(path$genes)), 
                   decreasing = decreasing)
  } else if(by == "pathway") {
    index <- order(sapply(x$pathway, function(path) length(path$name)), 
                   decreasing = decreasing)
  } else if(by == "n_dc") {
    alpha <- max(0.05, get_min_alpha(x))
    index <- order(sapply(x$pathway, function(path) sum(path$p_value_genes <= alpha)), 
                   decreasing = decreasing)
  } else {
    stop('`by` must be one of: "mean_expr", the mean expression of ',
         'each pathway across both groups; "mean_expr1" or "mean_expr2", the ',
         'mean expression of each pathway in group 1 or 2, respectively; ',
         '"dc_score", the differential connectivity score of the pathway; ',
         '"p_value", the p-value of the dc score; "n_genes", the number of ',
         'genes in each pathway; "pathway", the pathway name; ', 
         'or "n_dc" the number of significantly ', 
         'differentially conncted genes in each pathway.')
  }

  x <- x[index]
  
  return(x)
}


#' Extract parts of a 'dnapath_list' object.
#' 
#' @param x A 'dnapath_list' object from \code{\link{dnapath}}.
#' @param i The indices of pathways to extract.
#' @param ... Additional arguments are ignored.
#' @return A 'dnapath_list' object containing pathways indexed by 'i'.
#' @export
`[.dnapath_list` <- function(x, i, ...) {
  x$pathway <- x$pathway[i]
  # If all pathways are removed, return the empty list.
  if(length(x$pathway) == 0) {
    return(x)
  }
  
  p <- length(x$param$gene_names)
  remove_genes <- rep(TRUE, p)
  # x$pathway[[j]]$genes contains indices for x$param$gene_names that 
  # correspond to the genes contained in the pathway.
  # First, obtain all indices that are retained in the subset.
  keep <- sort(unique(unlist(lapply(x$pathway, function(x) x$genes))))
  # Check that the selected pathways are not empty.
  if(length(keep) == 0) {
    warning("Selected pathways contained no genes. Returning NULL.")
    return(NULL)
  }
  # Do not remove the gene names that are indexed by the pathways.
  remove_genes[keep] <- FALSE
  # New indices are shifted by the number of genes that were removed
  # before them. For ex. if the first first 3 genes are removed,
  # index 4 will become 4 - 3. If gene 5 is also removed. Index 6
  # would become 6 - (3 + 1) = 2.
  # The cumsum counts the number of genes that were removed prior
  # to each index.
  index_shift <- cumsum(remove_genes)
  # In the above example, newindex would be the vector 
  # (0, 0, 0, 1, 0, 2, ...). 
  # Indices 4 and 5 are retained and newindex[(4, 5)] provides the
  # new, shifted indices.
  newindex <- ((1:p) - index_shift)
  
  # Loop through each pathway and store the new gene indices.
  for(j in 1:length(x)) {
    x$pathway[[j]]$genes <- newindex[x$pathway[[j]]$genes]
  }
  
  # Update the gene names, counts, and x data set.
  x$param$gene_names <- x$param$gene_names[!remove_genes]
  x$param$n_pathways_containing_gene <- x$param$n_pathways_containing_gene[!remove_genes]
  x$param$x <- x$param$x[, !remove_genes]
  
  return(x)
}
 
#' Extract results of a single pathway from a 'dnapath' object.
#' 
#' @param x A 'dnapath' object.
#' @param i The index specifying which pathway to extract.
#' @param ... Additional arguments are ignored.
#' @return The 'dnapath' object unmodified
#' @note In the current implementation, there is nothing to subset on for
#' individual pathway results, so the original object is returned unmodified.
#' @export
#' @examples
#' data(meso)
#' data(p53_pathways)
#' set.seed(0)
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways[[1]],
#'                    group_labels = meso$groups, n_perm = 10)
#' results[1]
`[.dnapath` <- function(x, i, ...) {
  return(x)
}


#' Extract results of a single pathway from a 'dnapath_list' object.
#' 
#' @param x A 'dnapath_list' object from \code{\link{dnapath}}.
#' @param i The index specifying which pathway to extract.
#' @param ... Additional arguments are ignored.
#' @return A 'dnapath' object containing a single pathway result.
#' @export
#' @examples
#' data(meso)
#' data(p53_pathways)
#' set.seed(0)
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways,
#'                    group_labels = meso$groups, n_perm = 10)
#' results[[1]]
`[[.dnapath_list` <- function(x, i, ...) {
  if(length(i) != 1) {
    stop("The index should have length equal to 1.")
  }
  x$pathway <- x$pathway[[i]]
  class(x) <- "dnapath"
  return(x)
}

#' Extract results of a single pathway from a 'dnapath' object.
#' 
#' @param x A 'dnapath' object.
#' @param i The index specifying which pathway to extract.
#' @param ... Additional arguments are ignored.
#' @return The 'dnapath' object unmodified
#' @note In the current implementation, there is nothing to subset on for
#' individual pathway results, so the original object is returned unmodified.
#' @export
#' @examples
#' data(meso)
#' data(p53_pathways)
#' set.seed(0)
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways[[1]],
#'                    group_labels = meso$groups, n_perm = 10)
#' results[[1]]
`[[.dnapath` <- function(x, i, ...) {
  return(x)
}



#' Replace parts of a 'dnapath_list' object.
#' 
#' This functionality is not implemented and will return an error.
#' 
#' @param x A 'dnapath_list' object from \code{\link{dnapath}}.
#' @param ... Additional arguments are ignored.
#' @param value A 'dnapath_list' object.
#' @return Replacement is not defined; an error is generated.
#' @export
`[<-.dnapath_list` <- function(x, ..., value) {
  stop("Replacement undefined for 'dnapath_list' object.")
}

#' Replace parts of a 'dnapath' object.
#' 
#' This functionality is not implemented and will return an error.
#' 
#' @param x A 'dnapath' object from \code{\link{dnapath}}.
#' @param ... Additional arguments are ignored.
#' @param value A 'dnapath' object.
#' @return Replacement is not defined; an error is generated.
#' @export
`[<-.dnapath` <- function(x, ..., value) {
  stop("Replacement undefined for 'dnapath' object.")
}




#' Combine two 'dnapath_list' objects.
#' 
#' This functionality is not implemented and will return an error.
#' 
#' @param ... 'dnapath_list' objects to be concatenated.
#' @return Concatenation is not defined; an error is generated.
#' @export
`c.dnapath_list` <- function(...) {
  stop("Concatenation is undefined for 'dnapath_list' objects.")
}

#' Combine two 'dnapath' objects.
#' 
#' This functionality is not implemented and will return an error.
#' 
#' @param ... 'dnapath' objects to be concatenated.
#' @return Concatenation is not defined; an error is generated.
#' @export
`c.dnapath` <- function(...) {
  stop("Concatenation is undefined for 'dnapath' objects.")
}






#' Reverse the order of pathways in a 'dnapath_list' object.
#' 
#' @param x A 'dnapath_list' object from \code{\link{dnapath}}.
#' @param ... Additional arguments are ignored.
#' @return A 'dnapath_list' object containing the pathways in 'x' in reverse order.
#' @export
#' @examples
#' data(meso)
#' data(p53_pathways)
#' set.seed(0)
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways,
#'                    group_labels = meso$groups, n_perm = 10)
#' # Filter out pathways that have p-values above 0.2.
#' results <- filter_pathways(results, 0.2) 
#' results <- sort(results, by = "dc_score") # Sort by the pathway DC score.
#' results <- rev(results) # Reverse the ordering.
rev.dnapath_list <- function(x, ...) {
  return(x[length(x):1])
}






#' The pathway names in a 'dnapath_list' object.
#' 
#' @param x A 'dnapath_list' object from \code{\link{dnapath}}.
#' @return The pathway names.
#' @export
#' @examples
#' data(meso)
#' data(p53_pathways)
#' set.seed(0)
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways,
#'                    group_labels = meso$groups, n_perm = 10)
#' names(results)
names.dnapath_list <- function(x) {
  pathway_names <- unname(sapply(x$pathway, function(path) path$name))
  return(pathway_names)
}

#' The pathway names in a 'dnapath' object.
#' 
#' @param x A 'dnapath' object from \code{\link{dnapath}} or from subsetting
#' a 'dnapath_list'.
#' @return The pathway's name.
#' @export
#' @examples
#' data(meso)
#' data(p53_pathways)
#' set.seed(0)
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways,
#'                    group_labels = meso$groups, n_perm = 10)
#' names(results[[1]])
names.dnapath <- function(x) {
  pathway <- unname(x$pathway$name)
  return(pathway)
}





#' The number of pathways in a 'dnapath_list' object.
#' 
#' @param x A 'dnapath_list' object from \code{\link{dnapath}}.
#' @return The number of pathways.
#' @export
#' @examples
#' data(meso)
#' data(p53_pathways)
#' set.seed(0)
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways,
#'                    group_labels = meso$groups, n_perm = 10)
#' length(results)
length.dnapath_list <- function(x) {
  return(length(x$pathway))
}





#' Return the first part of the dnapath results.
#' 
#' @param x A 'dnapath_list' object.
#' @param ... Additional paramters are passed into 
#' \code{\link{summary.dnapath_list}}.
#' @return Returns the first five rows of the summary table of the
#' 'dnapath_list' object.
#' @export
#' @examples
#' data(meso)
#' data(p53_pathways)
#' set.seed(0)
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways,
#'                    group_labels = meso$groups, n_perm = 10)
#' head(results)
head.dnapath_list <- function(x, ...) {
  tab <- summary(x, ...)
  if(nrow(tab) <= 5) {
    return(tab)
  } else {
    return(tab[1:5, ])
  }
}

#' Return the last part of the dnapath results.
#' 
#' @param x A 'dnapath_list' object.
#' @param ... Additional paramters are passed into 
#' \code{\link{summary.dnapath_list}}.
#' @return Returns the last five rows of the summary table of the
#' 'dnapath_list' object.
#' @export
#' @examples
#' data(meso)
#' data(p53_pathways)
#' set.seed(0)
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways,
#'                    group_labels = meso$groups, n_perm = 10)
#' tail(results)
tail.dnapath_list <- function(x, ...) {
  tab <- summary(x, ...)
  if(nrow(tab) > 5) {
    return(tab[(nrow(tab) - 4):nrow(tab), ])
  } else {
    return(tab)
  }
}





#' Get mean expression of pathway genes in both groups
#'
#' @param x A 'dnapath' or 'dnapath_list' object.
#' @return A matrix of two rows whose columns contain the mean expression of 
#' each gene in the pathway.
#' @keywords internal
get_mean_expr_mat <- function(x) {
  # If sample sizes for each group aren't available, use weighted mean.
  if(length(x$param$n) == 1) {
    mean_expr <- rbind(
      colSums(x$param$x[, match(get_genes(x), colnames(x$param$x))] * x$param$group_prob[, 1]) / 
        sum(x$param$group_prob[, 1]),
      colSums(x$param$x[, match(get_genes(x), colnames(x$param$x))] * x$param$group_prob[, 2]) / 
        sum(x$param$group_prob[, 2]))
  } else {
    mean_expr <- rbind(
      colMeans(x$param$x[1:x$param$n[1], match(get_genes(x), colnames(x$param$x))]),
      colMeans(x$param$x[-(1:x$param$n[1]), match(get_genes(x),colnames(x$param$x))]))
  }
  
  return(mean_expr)
}
