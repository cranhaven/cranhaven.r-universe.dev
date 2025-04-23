#' Wrapper for ARACNE method
#' 
#' Conducts co-expression analysis using ARACNE \insertCite{margolin06}{dnapath}. 
#' Uses the implementation from the `minet` package \insertCite{meyer08}{dnapath}.
#' Can be used for the `network_inference` argument in \code{\link{dnapath}}.
#' 
#' @param x A n by p matrix of gene expression data (n samples and p genes).
#' @param weights An optional vector of weights. This is used by `dnapath()` to
#' apply the probabilistic group labels to each observation when estimating the
#' group-specific network.
#' @param estimator Argument is passed into \code{\link[minet]{build.mim}}.
#' @param disc Argument is passed into \code{\link[minet]{build.mim}}.
#' @param nbins Argument is passed into \code{\link[minet]{build.mim}}.
#' @param eps Argument is passed into \code{\link[minet]{aracne}}.
#' @param ... Additional arguments are ignored.
#' @return A p by p matrix of association scores.
#' @references 
#' \insertRef{margolin06}{dnapath}
#' 
#' \insertRef{meyer08}{dnapath}
#' @seealso 
#' \code{\link{run_bc3net}}, \code{\link{run_c3net}},
#' \code{\link{run_clr}}, \code{\link{run_corr}}, \code{\link{run_genie3}}, 
#' \code{\link{run_glasso}}, \code{\link{run_mrnet}},
#' \code{\link{run_pcor}}, and \code{\link{run_silencer}}
#' @export
#' @examples 
#' data(meso)
#' data(p53_pathways)
#' 
#' # To create a short example, we subset on two pathways from the p53 pathway list,
#' # and will only run 5 permutations for significance testing.
#' pathway_list <- p53_pathways[c(8, 13)]
#' n_perm <- 5
#' 
#' # Use this method to perform differential network analysis.
#' # The parameters in run_aracne() can be adjusted using the ... argument.
#' # For example, the 'estimator' parameter can be specified as shown here.
#' results <- dnapath(x = meso$gene_expression,
#'                    pathway_list = pathway_list,
#'                    group_labels = meso$groups,
#'                    n_perm = n_perm,
#'                    network_inference = run_aracne,
#'                    estimator = "spearman")
#' summary(results)
#' 
#' # The group-specific association matrices can be extracted using get_networks().
#' nw_list <- get_networks(results[[1]]) # Get networks for pathway 1.
#' 
#' \donttest{
#' # nw_list has length 2 and contains the inferred networks for the two groups.
#' # The gene names are the Entrezgene IDs from the original expression dataset.
#' # Renaming the genes in the dnapath results to rename those in the networks.
#' # NOTE: The temporary directory, tempdir(), is used in this example. In practice,
#' #       this argument can be removed or changed to an existing directory
#' results <- rename_genes(results, to = "symbol", species = "human",
#'                         dir_save = tempdir())
#' nw_list <- get_networks(results[[1]]) # The genes (columns) will have new names.
#' 
#' # (Optional) Plot the network using SeqNet package (based on igraph plotting).
#' # First rename entrezgene IDs into gene symbols.
#' SeqNet::plot_network(nw_list[[1]])
#' }
run_aracne <- function(x, weights = NULL, estimator = "spearman", disc = "none", 
                       nbins = NULL, eps = 0, ...) {
  if(!requireNamespace("minet", quietly = TRUE)) {
    message("Warning: The `minet` package must be installed to use run_aracne(). ",
            "Using run_corr() instead.")
    return(run_corr(x, weights = weights, ...))
  } else {
    p <- ncol(x)
    scores <- matrix(0, nrow = p, ncol = p)
    # Index the genes that have variability in their expression. (Using the max
    # and min as done here is faster than computing var().)
    index <- which(apply(x, 2, function(val) abs(max(val) - min(val)) > 1e-8))
    # If only 1 or fewer genes have variability, no network can be estimated.
    if(length(index) <= 1) 
      return(scores)
    
    index_rows <- 1:nrow(x)
    if(is.null(weights)) {
      # Don't make any changes to x.
    } else if(all(weights == 1 | weights == 0)) {
      # Subset rows onto those with weight of 1.
      index_rows <- (weights == 1)
    } else {
      # Subset rows onto those with weight above 0.5.
      # TODO: this scenario will be updated if aracne allows for weights.
      index_rows <- (weights >= 0.5)
    }
    x <- x[index_rows, ]
    weights <- weights[index_rows]
    
    mim <- minet::build.mim(x[, index], estimator = estimator, disc = disc, nbins = nbins)
    scores[index, index] <- minet::aracne(mim, eps = eps)
    
    colnames(scores) <- colnames(x)
    rownames(scores) <- NULL
    
    return(scores) 
  }
}

#' Wrapper for BC3Net method
#' 
#' Conducts co-expression analysis using BC3Net \insertCite{matos12}{dnapath}. 
#' Uses the implementation from the `bc3net` package \insertCite{bc3net}{dnapath}.
#' Can be used for the `network_inference` argument in \code{\link{dnapath}}.
#' 
#' @param x A n by p matrix of gene expression data (n samples and p genes).
#' @param weights An optional vector of weights. This is used by `dnapath()` to
#' apply the probabilistic group labels to each observation when estimating the
#' group-specific network.
#' @param boot Argument is passed into \code{\link[bc3net]{bc3net}}.
#' @param estimator Argument is passed into \code{\link[bc3net]{bc3net}}.
#' @param disc Argument is passed into \code{\link[bc3net]{bc3net}}.
#' @param mtc1 Argument is passed into \code{\link[bc3net]{bc3net}}.
#' @param adj1 Argument is passed into \code{\link[bc3net]{bc3net}}.
#' @param alpha1 Argument is passed into \code{\link[bc3net]{bc3net}}.
#' @param mtc2 Argument is passed into \code{\link[bc3net]{bc3net}}.
#' @param adj2 Argument is passed into \code{\link[bc3net]{bc3net}}.
#' @param alpha2 Argument is passed into \code{\link[bc3net]{bc3net}}.
#' @param ... Additional arguments are ignored.
#' @return A p by p matrix of association scores.
#' @references 
#' \insertRef{matos12}{dnapath}
#' 
#' \insertRef{bc3net}{dnapath}
#' @seealso 
#' \code{\link{run_aracne}}, \code{\link{run_c3net}},
#' \code{\link{run_clr}}, \code{\link{run_corr}}, \code{\link{run_genie3}}, 
#' \code{\link{run_glasso}}, \code{\link{run_mrnet}},
#' \code{\link{run_pcor}}, and \code{\link{run_silencer}}
#' @export
#' @examples 
#' data(meso)
#' data(p53_pathways)
#' 
#' # To create a short example, we subset on one pathway from the p53 pathway list,
#' # and will only run 1 permutation for significance testing.
#' pathway_list <- p53_pathways[13]
#' n_perm <- 1
#' 
#' # Use this method to perform differential network analysis.
#' # The parameters in run_bc3net() can be adjusted using the ... argument.
#' # For example, the 'estimator' and 'boot' parameter can be specified as shown here.
#' results <- dnapath(x = meso$gene_expression,
#'                    pathway_list = pathway_list,
#'                    group_labels = meso$groups,
#'                    n_perm = n_perm,
#'                    network_inference = run_bc3net,
#'                    boot = 10,
#'                    estimator = "pearson",
#'                    mtc1 = FALSE,
#'                    mtc2 = FALSE)
#' summary(results)
#' 
#' # The group-specific association matrices can be extracted using get_networks().
#' nw_list <- get_networks(results) # Get networks for pathway 1.
#' 
#' \donttest{
#' # nw_list has length 2 and contains the inferred networks for the two groups.
#' # The gene names are the Entrezgene IDs from the original expression dataset.
#' # Renaming the genes in the dnapath results to rename those in the networks.
#' # NOTE: The temporary directory, tempdir(), is used in this example. In practice,
#' #       this argument can be removed or changed to an existing directory
#' results <- rename_genes(results, to = "symbol", species = "human",
#'                         dir_save = tempdir())
#' nw_list <- get_networks(results) # The genes (columns) will have new names.
#' 
#' # (Optional) Plot the network using SeqNet package (based on igraph plotting).
#' # First rename entrezgene IDs into gene symbols.
#' SeqNet::plot_network(nw_list[[1]])
#' }
run_bc3net <- function(x, weights = NULL, boot = 100, estimator = "spearman", 
                       disc = "equalwidth", 
                       mtc1 = TRUE, adj1 = "bonferroni", alpha1 = 0.05, 
                       mtc2 = TRUE, adj2 = "bonferroni", alpha2 = 0.05, ...) {
  if(!requireNamespace("bc3net", quietly = TRUE)) {
    message("Warning: The `bc3net` package must be installed to use run_bc3net(). Using ",
            "run_corr() instead.")
    return(run_corr(x, weights = weights, ...))
  } else {
    p <- ncol(x)
    scores <- matrix(0, nrow = p, ncol = p)
    # Index the genes that have variability in their expression. (Using the max
    # and min as done here is faster than computing var().)
    index <- which(apply(x, 2, function(val) abs(max(val) - min(val)) > 1e-8))
    # If only 1 or fewer genes have variability, no network can be estimated.
    if(length(index) <= 1) 
      return(scores)
    
    index_rows <- 1:nrow(x)
    if(is.null(weights)) {
      # Don't make any changes to x.
    } else if(all(weights == 1 | weights == 0)) {
      # Subset rows onto those with weight of 1.
      index_rows <- (weights == 1)
    } else {
      # Subset rows onto those with weight above 0.5.
      # TODO: this scenario will be updated if bc3net allows for weights.
      index_rows <- (weights >= 0.5)
    }
    x <- x[index_rows, ]
    weights <- weights[index_rows]
    
    scores[index, index] <- bc3net::bc3net(t(x[, index]), 
                                           boot = boot,
                                           estimator = estimator,
                                           disc = disc,
                                           mtc1 = mtc1,
                                           adj1 = adj1,
                                           alpha1 = alpha1,
                                           mtc2 = mtc2,
                                           adj2 = adj2,
                                           alpha2 = alpha2,
                                           igraph = FALSE)
    
    colnames(scores) <- colnames(x)
    rownames(scores) <- NULL
    
    return(scores) 
  }
}


#' Wrapper for C3Net method
#' 
#' Conducts co-expression analysis using C3Net \insertCite{altay10}{dnapath}. 
#' Uses the implementation from the `bc3net` package \insertCite{bc3net}{dnapath}.
#' Can be used for the `network_inference` argument in \code{\link{dnapath}}.
#' 
#' @param x A n by p matrix of gene expression data (n samples and p genes).
#' @param weights An optional vector of weights. This is used by `dnapath()` to
#' apply the probabilistic group labels to each observation when estimating the
#' group-specific network.
#' @param estimator Argument is passed into \code{\link[bc3net]{c3mtc}}.
#' @param disc Argument is passed into \code{\link[bc3net]{c3mtc}}.
#' @param mtc Argument is passed into \code{\link[bc3net]{c3mtc}}.
#' @param adj Argument is passed into \code{\link[bc3net]{c3mtc}}.
#' @param alpha Argument is passed into \code{\link[bc3net]{c3mtc}}.
#' @param ... Additional arguments are ignored.
#' @return A p by p matrix of association scores.
#' @references 
#' \insertRef{altay10}{dnapath}
#' 
#' \insertRef{bc3net}{dnapath}
#' @seealso 
#' \code{\link{run_aracne}}, 
#' \code{\link{run_bc3net}}, 
#' \code{\link{run_clr}}, \code{\link{run_corr}}, \code{\link{run_genie3}}, 
#' \code{\link{run_glasso}}, \code{\link{run_mrnet}},
#' \code{\link{run_pcor}}, and \code{\link{run_silencer}}
#' @export
#' @examples 
#' data(meso)
#' data(p53_pathways)
#' 
#' # To create a short example, we subset on one pathway from the p53 pathway list,
#' # and will only run 1 permutation for significance testing.
#' pathway_list <- p53_pathways[13]
#' n_perm <- 1
#' 
#' # Use this method to perform differential network analysis.
#' # The parameters in run_c3net() can be adjusted using the ... argument.
#' # For example, the 'estimator' parameter can be specified as shown here.
#' results <- dnapath(x = meso$gene_expression,
#'                    pathway_list = pathway_list,
#'                    group_labels = meso$groups,
#'                    n_perm = n_perm,
#'                    network_inference = run_c3net,
#'                    estimator = "pearson",
#'                    mtc = FALSE)
#' summary(results)
#' 
#' # The group-specific association matrices can be extracted using get_networks().
#' nw_list <- get_networks(results) # Get networks for the pathway.
#' 
#' \donttest{
#' # nw_list has length 2 and contains the inferred networks for the two groups.
#' # The gene names are the Entrezgene IDs from the original expression dataset.
#' # Renaming the genes in the dnapath results to rename those in the networks.
#' # NOTE: The temporary directory, tempdir(), is used in this example. In practice,
#' #       this argument can be removed or changed to an existing directory
#' results <- rename_genes(results, to = "symbol", species = "human",
#'                         dir_save = tempdir())
#' nw_list <- get_networks(results) # The genes (columns) will have new names.
#' 
#' # (Optional) Plot the network using SeqNet package (based on igraph plotting).
#' # First rename entrezgene IDs into gene symbols.
#' SeqNet::plot_network(nw_list[[1]])
#' }
run_c3net <- function(x, weights = NULL, estimator = "spearman",  
                      disc = "equalwidth", mtc = TRUE, 
                      adj = "bonferroni", alpha = 0.05, ...) {
  if(!requireNamespace("bc3net", quietly = TRUE)) {
    message("Warning: The `bc3net` package must be installed to use run_c3net(). Using ",
            "run_corr() instead.")
    return(run_corr(x, weights = weights, ...))
  } else {
    p <- ncol(x)
    scores <- matrix(0, nrow = p, ncol = p)
    # Index the genes that have variability in their expression. (Using the max
    # and min as done here is faster than computing var().)
    index <- which(apply(x, 2, function(val) abs(max(val) - min(val)) > 1e-8))
    # If only 1 or fewer genes have variability, no network can be estimated.
    if(length(index) <= 1) 
      return(scores)
    
    index_rows <- 1:nrow(x)
    if(is.null(weights)) {
      # Don't make any changes to x.
    } else if(all(weights == 1 | weights == 0)) {
      # Subset rows onto those with weight of 1.
      index_rows <- (weights == 1)
    } else {
      # Subset rows onto those with weight above 0.5.
      # TODO: this scenario will be updated if c3mtc allows for weights.
      index_rows <- (weights >= 0.5)
    }
    x <- x[index_rows, ]
    weights <- weights[index_rows]
    
    scores[index, index] <- bc3net::c3mtc(t(x[, index]), 
                                          mtc = mtc,
                                          adj = adj,
                                          alpha = alpha, 
                                          estimator = estimator, 
                                          disc = disc,
                                          igraph = FALSE)
    
    colnames(scores) <- colnames(x)
    rownames(scores) <- NULL
    
    return(scores) 
  }
}


#' Wrapper for CLR method
#' 
#' Conducts co-expression analysis using CLR \insertCite{faith07}{dnapath}. 
#' Uses the implementation from the `minet` package \insertCite{meyer08}{dnapath}.
#' Can be used for the `network_inference` argument in \code{\link{dnapath}}.
#' 
#' @param x A n by p matrix of gene expression data (n samples and p genes).
#' @param weights An optional vector of weights. This is used by `dnapath()` to
#' apply the probabilistic group labels to each observation when estimating the
#' group-specific network.
#' @param estimator Argument is passed into \code{\link[minet]{build.mim}}.
#' @param ... Additional arguments are ignored.
#' @return A p by p matrix of association scores.
#' @references 
#' \insertRef{faith07}{dnapath}
#' 
#' \insertRef{meyer08}{dnapath}
#' @seealso 
#' \code{\link{run_aracne}}, 
#' \code{\link{run_bc3net}}, \code{\link{run_c3net}}, \code{\link{run_corr}}, 
#' \code{\link{run_genie3}}, \code{\link{run_glasso}}, \code{\link{run_mrnet}},
#' \code{\link{run_pcor}}, and \code{\link{run_silencer}}
#' @export
#' @examples 
#' data(meso)
#' data(p53_pathways)
#' 
#' # To create a short example, we subset on two pathways from the p53 pathway list,
#' # and will only run 5 permutations for significance testing.
#' pathway_list <- p53_pathways[c(8, 13)]
#' n_perm <- 5
#' 
#' # Use this method to perform differential network analysis.
#' # The parameters in run_clr() can be adjusted using the ... argument.
#' # For example, the 'estimator' paramter can be specified as shown here.
#' results <- dnapath(x = meso$gene_expression,
#'                    pathway_list = pathway_list,
#'                    group_labels = meso$groups,
#'                    n_perm = n_perm,
#'                    network_inference = run_clr,
#'                    estimator = "spearman")
#' summary(results)
#' 
#' # The group-specific association matrices can be extracted using get_networks().
#' nw_list <- get_networks(results[[1]]) # Get networks for pathway 1.
#' 
#' \donttest{
#' # nw_list has length 2 and contains the inferred networks for the two groups.
#' # The gene names are the Entrezgene IDs from the original expression dataset.
#' # Renaming the genes in the dnapath results to rename those in the networks.
#' # NOTE: The temporary directory, tempdir(), is used in this example. In practice,
#' #       this argument can be removed or changed to an existing directory
#' results <- rename_genes(results, to = "symbol", species = "human",
#'                         dir_save = tempdir())
#' nw_list <- get_networks(results[[1]]) # The genes (columns) will have new names.
#' 
#' # (Optional) Plot the network using SeqNet package (based on igraph plotting).
#' # First rename entrezgene IDs into gene symbols.
#' SeqNet::plot_network(nw_list[[1]])
#' }
run_clr <- function(x, weights = NULL, estimator = "spearman", ...) {
  if(!requireNamespace("minet", quietly = TRUE)) {
    message("Warning: The `minet` package must be installed to use run_clr(). Using ",
            "run_corr() instead.")
    return(run_corr(x, weights = weights, ...))
  } else {
    p <- ncol(x)
    scores <- matrix(0, nrow = p, ncol = p)
    # Index the genes that have variability in their expression. (Using the max
    # and min as done here is faster than computing var().)
    index <- which(apply(x, 2, function(val) abs(max(val) - min(val)) > 1e-8))
    # If only 1 or fewer genes have variability, no network can be estimated.
    if(length(index) <= 1) 
      return(scores)
    
    index_rows <- 1:nrow(x)
    if(is.null(weights)) {
      # Don't make any changes to x.
    } else if(all(weights == 1 | weights == 0)) {
      # Subset rows onto those with weight of 1.
      index_rows <- (weights == 1)
    } else {
      # Subset rows onto those with weight above 0.5.
      # TODO: this scenario will be updated if clr allows for weights.
      index_rows <- (weights >= 0.5)
    }
    x <- x[index_rows, ]
    weights <- weights[index_rows]
    
    mim <- minet::build.mim(x[, index], estimator = estimator)
    scores[index, index] <- minet::clr(mim)
    
    colnames(scores) <- colnames(x)
    rownames(scores) <- NULL
    
    return(scores) 
  }
}


#' Wrapper for correlation co-expression
#' 
#' Conducts co-expression analysis using correlation for association measure.
#' Can be used for the `network_inference` argument in \code{\link{dnapath}}.
#' 
#' @param x A n by p matrix of gene expression data (n samples and p genes).
#' @param weights An optional vector of weights. This is used by `dnapath()` to
#' apply the probabilistic group labels to each observation when estimating the
#' group-specific network.
#' @param threshold Cutoff for significant associations. If NULL, all correlations
#' are returned. Otherwise, correlations of magnitude at or below this threshold are 
#' set to zero.
#' @param method Argument is passed into \code{\link[stats]{cor}}. Should be one
#' of "pearson" or "spearman".
#' @param ... Additional arguments are ignored.
#' @return A p by p matrix of association scores.
#' @seealso 
#' \code{\link{run_aracne}}, 
#' \code{\link{run_bc3net}}, \code{\link{run_c3net}},
#' \code{\link{run_clr}}, \code{\link{run_genie3}}, 
#' \code{\link{run_glasso}}, \code{\link{run_mrnet}},
#' \code{\link{run_pcor}}, and \code{\link{run_silencer}}
#' @export
#' @examples 
#' data(meso)
#' data(p53_pathways)
#' 
#' # To create a short example, we subset on two pathways from the p53 pathway list,
#' # and will only run 5 permutations for significance testing.
#' pathway_list <- p53_pathways[c(8, 13)]
#' n_perm <- 5
#' 
#' # Use this method to perform differential network analysis.
#' # The parameters in run_corr() can be adjusted using the ... argument.
#' # For example, the 'method' parameter can be specified as shown here.
#' results <- dnapath(x = meso$gene_expression,
#'                    pathway_list = pathway_list,
#'                    group_labels = meso$groups,
#'                    n_perm = n_perm,
#'                    network_inference = run_corr,
#'                    method = "spearman")
#' summary(results)
#' 
#' # The group-specific association matrices can be extracted using get_networks().
#' nw_list <- get_networks(results[[1]]) # Get networks for pathway 1.
#' 
#' \donttest{
#' # nw_list has length 2 and contains the inferred networks for the two groups.
#' # The gene names are the Entrezgene IDs from the original expression dataset.
#' # Renaming the genes in the dnapath results to rename those in the networks.
#' # NOTE: The temporary directory, tempdir(), is used in this example. In practice,
#' #       this argument can be removed or changed to an existing directory
#' results <- rename_genes(results, to = "symbol", species = "human",
#'                         dir_save = tempdir())
#' nw_list <- get_networks(results[[1]]) # The genes (columns) will have new names.
#' 
#' # (Optional) Plot the network using SeqNet package (based on igraph plotting).
#' # First rename entrezgene IDs into gene symbols.
#' SeqNet::plot_network(nw_list[[1]])
#' }
run_corr <- function(x, weights = NULL, threshold = NULL, 
                     method = c("pearson", "spearman"), ...) {
  method <- tolower(method[1])
  if(method == "pearson") {
    # Capitalized Pearson or Spearman for use with wCorr::weightedCorr.
    method <- "Pearson" 
  } else if(method == "spearman") {
    method  <- "Spearman"
  } else {
    stop('method should be one of c("pearson", "spearman").')
  }
  p <- ncol(x)
  scores <- matrix(0, nrow = p, ncol = p)
  # Index the genes that have variability in their expression. (Using the max
  # and min as done here is faster than computing var().)
  index <- which(apply(x, 2, function(val) abs(max(val) - min(val)) > 1e-8))
  # If only 1 or fewer genes have variability, no network can be estimated.
  if(length(index) <= 1) 
    return(scores)
  
  if(is.integer(x[1, 1])) {
    x <- x + 0.0
  }
  
  if(is.null(weights)) {
    weights <- rep(1, nrow(x))
  }
  
  index_rows <- 1:nrow(x)
  if(all(weights == 1 | weights == 0))
    index_rows <- (weights == 1) # Subset rows onto those with weight of 1.
  x <- x[index_rows, ]
  weights <- weights[index_rows]
  
  index_set <- combn(length(index), 2)
  scores[upper.tri(scores)] <- sapply(1:ncol(index_set), function(k) {
    i = index[index_set[1, k]]
    j = index[index_set[2, k]]
    wCorr::weightedCorr(x[, i], x[, j], method = method, weights = weights)
  })
  scores <- scores + t(scores)
  diag(scores) <- 0
  
  if(!is.null(threshold)) {
    scores[abs(scores) <= threshold] <- 0
  }
  
  colnames(scores) <- colnames(x)
  
  if(any(is.na(scores))) {
    scores[is.na(scores)] <- 0
  }
  
  return(scores)
}



#' Wrapper for GENIE3 method
#' 
#' Conducts co-expression analysis using GENIE3 \insertCite{huynhthu10}{dnapath}. 
#' Uses the implementation from the `GENIE3` package.
#' Can be used for the `network_inference` argument in \code{\link{dnapath}}.
#' 
#' @param x A n by p matrix of gene expression data (n samples and p genes).
#' @param nTrees Argument is passed into \code{\link[GENIE3]{GENIE3}}. 
#' @param weights An optional vector of weights. This is used by `dnapath()` to
#' apply the probabilistic group labels to each observation when estimating the
#' group-specific network.
#' @param ... Additional arguments are ignored.
#' @return A p by p matrix of association scores.
#' @references 
#' \insertRef{huynhthu10}{dnapath}
#' @seealso 
#' \code{\link{run_aracne}}, 
#' \code{\link{run_bc3net}}, \code{\link{run_c3net}},
#' \code{\link{run_clr}}, \code{\link{run_corr}},
#' \code{\link{run_glasso}}, \code{\link{run_mrnet}},
#' \code{\link{run_pcor}}, and \code{\link{run_silencer}}
#' @export
#' @examples 
#' if(!requireNamespace("GENIE3", quietly = TRUE)) {
#' data(meso)
#' data(p53_pathways)
#' 
#' # To create a short example, we subset on two pathways from the p53 pathway list,
#' # and will only run 5 permutations for significance testing.
#' pathway_list <- p53_pathways[c(8, 13)]
#' n_perm <- 5
#' 
#' # Use this method to perform differential network analysis.
#' # The parameters in run_genie3() can be adjusted using the ... argument.
#' # For example, the 'nTrees' parameter can be specified as shown here.
#' results <- dnapath(x = meso$gene_expression,
#'                    pathway_list = pathway_list,
#'                    group_labels = meso$groups,
#'                    n_perm = n_perm,
#'                    network_inference = run_genie3,
#'                    nTrees = 100)
#' summary(results)
#' 
#' # The group-specific association matrices can be extracted using get_networks().
#' nw_list <- get_networks(results[[1]]) # Get networks for pathway 1.
#' 
#' # nw_list has length 2 and contains the inferred networks for the two groups.
#' # The gene names are the Entrezgene IDs from the original expression dataset.
#' # Renaming the genes in the dnapath results to rename those in the networks.
#' # NOTE: The temporary directory, tempdir(), is used in this example. In practice,
#' #       this argument can be removed or changed to an existing directory
#' results <- rename_genes(results, to = "symbol", species = "human",
#'                         dir_save = tempdir())
#' nw_list <- get_networks(results[[1]]) # The genes (columns) will have new names.
#' 
#' # (Optional) Plot the network using SeqNet package (based on igraph plotting).
#' # First rename entrezgene IDs into gene symbols.
#' SeqNet::plot_network(nw_list[[1]])
#' }
run_genie3 <- function(x, weights = NULL, nTrees = 200, ...) {
  if(!requireNamespace("GENIE3", quietly = TRUE)) {
    message("Warning: The `GENIE3` package must be installed to use run_genie3(). ", 
            "Using run_corr() instead.")
    return(run_corr(x, weights = weights, ...))
  } else {
    p <- ncol(x)
    scores <- matrix(0, nrow = p, ncol = p)
    # Index the genes that have variability in their expression. (Using the max
    # and min as done here is faster than computing var().)
    index <- which(apply(x, 2, function(val) abs(max(val) - min(val)) > 1e-8))
    # If only 1 or fewer genes have variability, no network can be estimated.
    if(length(index) <= 1) 
      return(scores)
    
    index_rows <- 1:nrow(x)
    if(is.null(weights)) {
      # Don't make any changes to x.
    } else if(all(weights == 1 | weights == 0)) {
      # Subset rows onto those with weight of 1.
      index_rows <- (weights == 1)
    } else {
      # Subset rows onto those with weight above 0.5.
      # TODO: this scenario will be updated if GENIE3 allows for weights.
      index_rows <- (weights >= 0.5)
    }
    x <- x[index_rows, ]
    weights <- weights[index_rows]
    
    mat <- GENIE3::GENIE3(t(x[, index]), nTrees = nTrees)
    
    # GENIE3 rearranges columns of x; need to put back in orginal order.
    genes <- colnames(mat)
    orig_index <- sapply(colnames(x[, index]), function(k) which(genes == k))
    mat <- mat[orig_index, orig_index]
    
    scores[index, index] <- mat
    
    # Symmetrize the associations.
    scores <- 0.5 * (scores + t(scores))
    
    colnames(scores) <- colnames(x)
    rownames(scores) <- NULL
    
    return(scores)
  }
}



#' Wrapper for glasso method
#' 
#' Conducts co-expression analysis using glasso \insertCite{friedman18}{dnapath}. 
#' Uses the implementation from the `huge` package \insertCite{huge}{dnapath}.
#' Can be used for the `network_inference` argument in \code{\link{dnapath}}.
#' 
#' @param x A n by p matrix of gene expression data (n samples and p genes).
#' @param method Argument is passed into \code{\link[huge]{huge}}.
#' @param criterion Argument is passed into \code{\link[huge]{huge.select}}.
#' @param verbose Argument is passed into \code{\link[huge]{huge}} and 
#' \code{\link[huge]{huge.select}}
#' @param weights An optional vector of weights. This is used by `dnapath()` to
#' apply the probabilistic group labels to each observation when estimating the
#' group-specific network.
#' @param ... Additional arguments are ignored.
#' @return A p by p matrix of association scores.
#' @references 
#' \insertRef{friedman18}{dnapath}
#' 
#' \insertRef{huge}{dnapath}
#' @seealso 
#' \code{\link{run_aracne}}, 
#' \code{\link{run_bc3net}}, \code{\link{run_c3net}},
#' \code{\link{run_clr}}, \code{\link{run_corr}}, 
#' \code{\link{run_genie3}}, \code{\link{run_mrnet}},
#' \code{\link{run_pcor}}, and \code{\link{run_silencer}}
#' @export
#' @examples 
#' data(meso)
#' data(p53_pathways)
#' 
#' # To create a short example, we subset on one pathway from the p53 pathway list,
#' # and will only run 1 permutation for significance testing.
#' pathway_list <- p53_pathways[13]
#' n_perm <- 1
#' 
#' # Use this method to perform differential network analysis.
#' # The parameters in run_glasso() can be adjusted using the ... argument.
#' # For example, the 'criterion' parameter can be specified as shown here.
#' results <- dnapath(x = meso$gene_expression,
#'                    pathway_list = pathway_list,
#'                    group_labels = meso$groups,
#'                    n_perm = n_perm,
#'                    network_inference = run_glasso,
#'                    criterion = "ric")
#' summary(results)
#' 
#' # The group-specific association matrices can be extracted using get_networks().
#' nw_list <- get_networks(results) # Get networks for pathway 1.
#' 
#' \donttest{
#' # nw_list has length 2 and contains the inferred networks for the two groups.
#' # The gene names are the Entrezgene IDs from the original expression dataset.
#' # Renaming the genes in the dnapath results to rename those in the networks.
#' # NOTE: The temporary directory, tempdir(), is used in this example. In practice,
#' #       this argument can be removed or changed to an existing directory
#' results <- rename_genes(results, to = "symbol", species = "human",
#'                         dir_save = tempdir())
#' nw_list <- get_networks(results) # The genes (columns) will have new names.
#' 
#' # (Optional) Plot the network using SeqNet package (based on igraph plotting).
#' # First rename entrezgene IDs into gene symbols.
#' SeqNet::plot_network(nw_list[[1]])
#' }
run_glasso <- function(x, 
                       method = c("glasso", "mb", "ct"), 
                       criterion = c("ric", "stars"), 
                       verbose = FALSE, weights = NULL, ...) {
  if(!requireNamespace("huge", quietly = TRUE)) {
    message("Warning: The `huge` package must be installed to use run_glasso(). Using ",
            "run_corr(x, method = 'pearson') instead.")
    return(run_corr(x, method = "pearson", ...))
  } else {
    method <- tolower(method[1])
    criterion <- tolower(criterion[1])
    if(!(method %in% c("mb", "glasso", "ct"))) {
      stop('method should be one of "mb", "glasso", or "ct".')
    }
    if(criterion != "ric" & criterion != "stars") {
      stop("criterion must be either \"ric\" or \"stars\".")
    }
    
    p <- ncol(x)
    scores <- matrix(0, nrow = p, ncol = p)
    # Index the genes that have variability in their expression. (Using the max
    # and min as done here is faster than computing var().)
    index <- which(apply(x, 2, function(val) abs(max(val) - min(val)) > 1e-8))
    # If only 1 or fewer genes have variability, no network can be estimated.
    if(length(index) <= 1) 
      return(scores)
    
    index_rows <- 1:nrow(x)
    if(is.null(weights)) {
      # Don't make any changes to x.
    } else if(all(weights == 1 | weights == 0)) {
      # Subset rows onto those with weight of 1.
      index_rows <- (weights == 1)
    } else {
      # Subset rows onto those with weight above 0.5.
      # TODO: this scenario will be updated if huge allows for weights.
      index_rows <- (weights >= 0.5)
    }
    x <- x[index_rows, ]
    weights <- weights[index_rows]
    
    # Use unsigned to treat positive and negative associations equally.
    x_huge <- huge::huge(x[, index], method = method, verbose = verbose)
    result <- huge::huge.select(x_huge, criterion = criterion, verbose = verbose)
    
    # Choose largest lambda at or below the optimal value.
    index_lambda <- which(result$lambda <= result$opt.lambda)[1]
    if(length(index) == 0) {
      index_lambda <- 10 # = length(x_huge$icov)
    } else if(is.na(index_lambda)) {
      # If no lambdas were smaller than optimal value, choose the smallest.
      index_lambda <- 1
    }
    
    if(verbose) {
      cat("Optimal lambda:", result$opt.lambda, "\n")
    }
    
    mat <- as.matrix(x_huge$icov[[index_lambda]])
    mat <- cov2cor(mat)
    diag(mat) <- 0
    mat <- (mat + t(mat)) / 2
    
    scores[index, index] <- mat
    colnames(scores) <- colnames(x)
    
    return(scores)
  }
}


#' Wrapper for MRNET method
#' 
#' Conducts co-expression analysis using MRNET \insertCite{meyer07}{dnapath}. 
#' Uses the implementation from the `minet` package \insertCite{meyer08}{dnapath}.
#' Can be used for the `network_inference` argument in \code{\link{dnapath}}.
#' 
#' @param x A n by p matrix of gene expression data (n samples and p genes).
#' @param estimator Argument is passed into \code{\link[minet]{build.mim}}.
#' @param weights An optional vector of weights. This is used by `dnapath()` to
#' apply the probabilistic group labels to each observation when estimating the
#' group-specific network.
#' @param ... Additional arguments are ignored.
#' @return A p by p matrix of association scores.
#' @references 
#' \insertRef{meyer07}{dnapath}
#' 
#' \insertRef{meyer08}{dnapath}
#' @seealso 
#' \code{\link{run_aracne}}, 
#' \code{\link{run_bc3net}}, \code{\link{run_c3net}},
#' \code{\link{run_clr}}, \code{\link{run_corr}}, 
#' \code{\link{run_genie3}}, \code{\link{run_glasso}}, 
#' \code{\link{run_pcor}}, and \code{\link{run_silencer}}
#' @export
#' @examples 
#' data(meso)
#' data(p53_pathways)
#' 
#' # To create a short example, we subset on two pathways from the p53 pathway list,
#' # and will only run 3 permutations for significance testing.
#' pathway_list <- p53_pathways[c(8, 13)]
#' n_perm <- 3
#' 
#' # Use this method to perform differential network analysis.
#' # The parameters in run_mrnet() can be adjusted using the ... argument.
#' # For example, the 'estimator' parameter can be specified as shown here.
#' results <- dnapath(x = meso$gene_expression,
#'                    pathway_list = pathway_list,
#'                    group_labels = meso$groups,
#'                    n_perm = n_perm,
#'                    network_inference = run_mrnet,
#'                    estimator = "spearman")
#' summary(results)
#' 
#' # The group-specific association matrices can be extracted using get_networks().
#' nw_list <- get_networks(results[[1]]) # Get networks for pathway 1.
#' 
#' \donttest{
#' # nw_list has length 2 and contains the inferred networks for the two groups.
#' # The gene names are the Entrezgene IDs from the original expression dataset.
#' # Renaming the genes in the dnapath results to rename those in the networks.
#' # NOTE: The temporary directory, tempdir(), is used in this example. In practice,
#' #       this argument can be removed or changed to an existing directory
#' results <- rename_genes(results, to = "symbol", species = "human",
#'                         dir_save = tempdir())
#' nw_list <- get_networks(results[[1]]) # The genes (columns) will have new names.
#' 
#' # (Optional) Plot the network using SeqNet package (based on igraph plotting).
#' # First rename entrezgene IDs into gene symbols.
#' SeqNet::plot_network(nw_list[[1]])
#' }
run_mrnet <- function(x, estimator = "spearman", weights = NULL, ...) {
  if(!requireNamespace("minet", quietly = TRUE)) {
    message("Warning: The `minet` package must be installed to use run_mrnet(). Using ",
            "run_corr() instead.")
    return(run_corr(x, ...))
  } else {
    p <- ncol(x)
    scores <- matrix(0, nrow = p, ncol = p)
    # Index the genes that have variability in their expression. (Using the max
    # and min as done here is faster than computing var().)
    index <- which(apply(x, 2, function(val) abs(max(val) - min(val)) > 1e-8))
    # If only 1 or fewer genes have variability, no network can be estimated.
    if(length(index) <= 1) 
      return(scores)
    
    index_rows <- 1:nrow(x)
    if(is.null(weights)) {
      # Don't make any changes to x.
    } else if(all(weights == 1 | weights == 0)) {
      # Subset rows onto those with weight of 1.
      index_rows <- (weights == 1)
    } else {
      # Subset rows onto those with weight above 0.5.
      # TODO: this scenario will be updated if mrnet allows for weights.
      index_rows <- (weights >= 0.5)
    }
    x <- x[index_rows, ]
    weights <- weights[index_rows]
    
    mim <- minet::build.mim(x[, index], estimator = estimator)
    scores[index, index] <- minet::mrnet(mim)
    
    colnames(scores) <- colnames(x)
    rownames(scores) <- NULL
    
    return(scores)
  }
}

#' Wrapper for partial correlations from corpcor
#' 
#' Conducts co-expression analysis using full partial correlations; these are
#' computed using the shrinkage approach for covariance estimation 
#' \insertCite{schafer05}{dnapath} from the 
#' `corpcor` package \insertCite{corpcor}{dnapath}.
#' Can be used for the `network_inference` argument in \code{\link{dnapath}}.
#' 
#' @param x A n by p matrix of gene expression data (n samples and p genes).
#' @param ranks If TRUE, the gene expression values will be converted to ranks
#' (across samples) prior to covariance estimation.
#' @param verbose Argument is passed into \code{\link[corpcor]{pcor.shrink}}.
#' @param weights An optional vector of weights. This is used by `dnapath()` to
#' apply the probabilistic group labels to each observation when estimating the
#' group-specific network.
#' @param ... Additional arguments are ignored.
#' @return A p by p matrix of association scores.
#' @references 
#' \insertRef{schafer05}{dnapath}
#' 
#' \insertRef{corpcor}{dnapath}
#' @seealso 
#' \code{\link{run_aracne}}, 
#' \code{\link{run_bc3net}}, \code{\link{run_c3net}},
#' \code{\link{run_clr}}, \code{\link{run_corr}}, \code{\link{run_genie3}}, 
#' \code{\link{run_glasso}}, \code{\link{run_mrnet}}, and \code{\link{run_silencer}}
#' @export
#' @examples 
#' data(meso)
#' data(p53_pathways)
#' 
#' # To create a short example, we subset on two pathways from the p53 pathway list,
#' # and will only run 3 permutations for significance testing.
#' pathway_list <- p53_pathways[c(8, 13)]
#' n_perm <- 3
#' 
#' # Use this method to perform differential network analysis.
#' results <- dnapath(x = meso$gene_expression,
#'                    pathway_list = pathway_list,
#'                    group_labels = meso$groups,
#'                    n_perm = n_perm,
#'                    network_inference = run_pcor)
#' summary(results)
#' 
#' # The group-specific association matrices can be extracted using get_networks().
#' nw_list <- get_networks(results[[1]]) # Get networks for pathway 1.
#' 
#' \donttest{
#' # nw_list has length 2 and contains the inferred networks for the two groups.
#' # The gene names are the Entrezgene IDs from the original expression dataset.
#' # Renaming the genes in the dnapath results to rename those in the networks.
#' # NOTE: The temporary directory, tempdir(), is used in this example. In practice,
#' #       this argument can be removed or changed to an existing directory
#' results <- rename_genes(results, to = "symbol", species = "human",
#'                         dir_save = tempdir())
#' nw_list <- get_networks(results[[1]]) # The genes (columns) will have new names.
#' 
#' # (Optional) Plot the network using SeqNet package (based on igraph plotting).
#' # First rename entrezgene IDs into gene symbols.
#' SeqNet::plot_network(nw_list[[1]])
#' }
run_pcor <- function(x, weights = NULL, ranks = FALSE, verbose = FALSE, ...) {
  p <- ncol(x)
  scores <- matrix(0, nrow = p, ncol = p)
  # Index the genes that have variability in their expression. (Using the max
  # and min as done here is faster than computing var().)
  index <- which(apply(x, 2, function(val) abs(max(val) - min(val)) > 1e-8))
  # If only 1 or fewer genes have variability, no network can be estimated.
  if(length(index) <= 1) 
    return(scores)
  
  index_rows <- 1:nrow(x)
  if(is.null(weights)) {
    # Don't make any changes to x.
  } else if(all(weights == 1 | weights == 0)) {
    # Subset rows onto those with weight of 1.
    index_rows <- (weights == 1)
  } else {
    # Keep weights unchanged and all rows of x.
  }
  x <- x[index_rows, ]
  weights <- weights[index_rows]
  
  if(ranks) {
    x[, index] <- apply(x[, index], 2, rank)
  }
  
  if(is.null(weights)) {
    mat <- corpcor::pcor.shrink(x[, index], verbose = verbose)
  } else {
    mat <- corpcor::pcor.shrink(x[, index], w = weights, verbose = verbose)
  }
  
  mat <- matrix(as.numeric(mat), nrow = nrow(mat), ncol = ncol(mat))
  diag(mat) <- 0
  
  scores[index, index] <- mat
  
  colnames(scores) <- colnames(x)
  
  return(scores)
}


#' Wrapper for silencer method
#' 
#' Conducts co-expression analysis using the matrix silencer method 
#' \insertCite{barzel13}{dnapath}. 
#' Can be used for the `network_inference` argument in \code{\link{dnapath}}.
#' 
#' @param x A n by p matrix of gene expression data (n samples and p genes).
#' @param weights An optional vector of weights. This is used by `dnapath()` to
#' apply the probabilistic group labels to each observation when estimating the
#' group-specific network.
#' @param method Argument is passed into \code{\link[stats]{cor}}.
#' @param verbose If TRUE, updates are printed during the estimation process.
#' @param ... Additional arguments are ignored.
#' @return A p by p matrix of association scores.
#' @references 
#' \insertRef{barzel13}{dnapath}
#' @seealso 
#' \code{\link{run_aracne}}, 
#' \code{\link{run_bc3net}}, \code{\link{run_c3net}},
#' \code{\link{run_clr}}, \code{\link{run_corr}}, \code{\link{run_genie3}}, 
#' \code{\link{run_glasso}}, \code{\link{run_mrnet}}, and
#' \code{\link{run_pcor}}
#' @export
#' @examples
#' data(meso)
#' data(p53_pathways)
#' 
#' # To create a short example, we subset on two pathways from the p53 pathway list,
#' # and will only run 1 permutation for significance testing.
#' pathway_list <- p53_pathways[13]
#' n_perm <- 1
#' 
#' # Use this method to perform differential network analysis.
#' # The parameters in run_silencer() can be adjusted using the ... argument.
#' # For example, the 'method' parameter can be specified as shown here.
#' results <- dnapath(x = meso$gene_expression,
#'                    pathway_list = pathway_list,
#'                    group_labels = meso$groups,
#'                    n_perm = n_perm,
#'                    network_inference = run_silencer,
#'                    method = "spearman")
#' summary(results)
#' 
#' # The group-specific association matrices can be extracted using get_networks().
#' nw_list <- get_networks(results) # Get networks for the pathway
#' 
#' \donttest{
#' # nw_list has length 2 and contains the inferred networks for the two groups.
#' # The gene names are the Entrezgene IDs from the original expression dataset.
#' # Renaming the genes in the dnapath results to rename those in the networks.
#' # NOTE: The temporary directory, tempdir(), is used in this example. In practice,
#' #       this argument can be removed or changed to an existing directory
#' results <- rename_genes(results, to = "symbol", species = "human",
#'                         dir_save = tempdir())
#' nw_list <- get_networks(results) # The genes (columns) will have new names.
#' 
#' # (Optional) Plot the network using SeqNet package (based on igraph plotting).
#' # First rename entrezgene IDs into gene symbols.
#' SeqNet::plot_network(nw_list[[1]])
#' }
run_silencer <- function(x, weights = NULL, method = "spearman", 
                         verbose = FALSE, ...) {
  p <- ncol(x)
  scores <- matrix(0, nrow = p, ncol = p)
  # Index the genes that have variability in their expression. (Using the max
  # and min as done here is faster than computing var().)
  index <- which(apply(x, 2, function(val) abs(max(val) - min(val)) > 1e-8))
  # If only 1 or fewer genes have variability, no network can be estimated.
  if(length(index) <= 1) 
    return(scores)
  
  index_rows <- 1:nrow(x)
  if(is.null(weights)) {
    # Don't make any changes to x.
  } else if(all(weights == 1 | weights == 0)) {
    # Subset rows onto those with weight of 1.
    index_rows <- (weights == 1)
  } else {
    # Subset rows onto those with weight above 0.5.
    # TODO: this scenario will be updated if silencer allows for weights.
    index_rows <- (weights >= 0.5)
  }
  x <- x[index_rows, ]
  weights <- weights[index_rows]
  
  Norm <-  0.5
  # First calculate correlation matrix.
  G <- cor(x[, index], method = method)
  G[is.na(G) | is.nan(G)] <- 0
  # diag(G) <- 0
  
  # Preprocessing: check singularity of G.
  N <- nrow(G)
  R <- qr(G)$rank
  
  alpha <- 1
  
  if(R < N) {
    G <- G * 0.5 # Initial Norm = 0.5.
    diag(G) <- 1
    alpha <- alpha * Norm
  }
  
  MaxValue <- 2
  Maximum <- 0.9
  Minimum <- 0.5
  
  Go <- G - diag(N)
  S <- (Go + diag(diag(Go %*% G))) %*% solve(G)
  S <- Go
  
  n <- 0
  while((MaxValue <= Minimum || MaxValue >= Maximum) && n < 20) {
    Go <- G - diag(N)
    
    # Perform the transormation.
    S <- (Go + diag(diag(Go %*% G))) %*% solve(G)
    MaxValue <- max(abs(eigen(S)$values))
    
    if(MaxValue <= Minimum || MaxValue >= Maximum) {
      Norm <- 1 / (sqrt(MaxValue / 0.5 * (Maximum - Minimum)))
      # If Norm is close to 1, calculate again.
      if(abs(1 - Norm) < 0.01) {
        Norm <- 1 / (sqrt(MaxValue / 0.5 * (Maximum + Minimum)))
      }
      G <- G * Norm
      diag(G) <- 1
      alpha <- alpha * Norm
    }
    
    n <- n + 1
    
    if(verbose) {
      cat(paste("n =", n, 
                "- alpha =", round(alpha, 4), 
                "MaxValue =", round(MaxValue, 4),
                "Norm =", round(Norm, 4), "\n"))
    }
  }
  
  # S <- cov2cor(S)
  diag(S) <- 0
  S <- S / max(abs(S))
  S <- 0.5 * (S + t(S))
  
  scores[index, index] <- S
  colnames(scores) <- colnames(x)
  
  return(scores)
}


#' Wrapper for partial correlations with Empirical Bayes FDR correction
#' 
#' Conducts co-expression analysis using full partial correlations; these are
#' computed using the shrinkage approach for covariance estimation 
#' \insertCite{schafer05}{dnapath} from the 
#' `corpcor` package \insertCite{corpcor}{dnapath}.
#' Can be used for the `network_inference` argument in \code{\link{dnapath}}.
#' This method will use Empirical Bayes FDR to set some estimates to zero. 
#' 
#' @param x A n by p matrix of gene expression data (n samples and p genes).
#' @param weights An optional vector of weights. This is used by `dnapath()` to
#' apply the probabilistic group labels to each observation when estimating the
#' group-specific network.
#' @param ranks If TRUE, the gene expression values will be converted to ranks
#' (across samples) prior to covariance estimation.
#' @param thrsh A positive value (defaults to 1.5). This is used as
#' the cutoff for the likelihood ratio of the estimate local FDR.
#' @param verbose Argument is passed into \code{\link[corpcor]{pcor.shrink}}.
#' @param ... Additional arguments are ignored.
#' @return A p by p matrix of association scores.
#' @references 
#' \insertRef{schafer05}{dnapath}
#' 
#' \insertRef{corpcor}{dnapath}
#' @seealso 
#' \code{\link{run_aracne}}, 
#' \code{\link{run_bc3net}}, \code{\link{run_c3net}},
#' \code{\link{run_clr}}, \code{\link{run_corr}}, \code{\link{run_genie3}}, 
#' \code{\link{run_glasso}}, \code{\link{run_mrnet}}, and \code{\link{run_silencer}}
#' @export
#' @examples 
#' data(meso)
#' data(p53_pathways)
#' 
#' # To create a short example, we subset on two pathways from the p53 pathway list,
#' # and will only run 3 permutations for significance testing.
#' pathway_list <- p53_pathways[c(8, 13)]
#' n_perm <- 3
#' 
#' # Use this method to perform differential network analysis.
#' results <- dnapath(x = meso$gene_expression,
#'                    pathway_list = pathway_list,
#'                    group_labels = meso$groups,
#'                    n_perm = n_perm,
#'                    network_inference = run_pcor)
#' summary(results)
#' 
#' # The group-specific association matrices can be extracted using get_networks().
#' nw_list <- get_networks(results[[1]]) # Get networks for pathway 1.
#' 
#' \donttest{
#' # nw_list has length 2 and contains the inferred networks for the two groups.
#' # The gene names are the Entrezgene IDs from the original expression dataset.
#' # Renaming the genes in the dnapath results to rename those in the networks.
#' # NOTE: The temporary directory, tempdir(), is used in this example. In practice,
#' #       this argument can be removed or changed to an existing directory
#' results <- rename_genes(results, to = "symbol", species = "human",
#'                         dir_save = tempdir())
#' nw_list <- get_networks(results[[1]]) # The genes (columns) will have new names.
#' 
#' # (Optional) Plot the network using SeqNet package (based on igraph plotting).
#' # First rename entrezgene IDs into gene symbols.
#' SeqNet::plot_network(nw_list[[1]])
#' }
run_pcor_fdr <- function(x, weights = NULL, ranks = TRUE, thrsh = 1.5, 
                         verbose = FALSE, ...) {
  p <- ncol(x)
  
  # First, estimate the network using the partial correlation method.
  scores <- run_pcor(x, weights = weights, ranks = ranks, verbose = verbose, ...)
  
  # Extract the individual partial correlation estimates.
  vals <- scores[lower.tri(scores)]
  # Estimate the mean and standard deviation of the null distribution.
  mu.f0 <- median(vals) # Use median since distribution of scores may be skewed.
  sigma.f0 <- 1.4826 * median(abs(vals - median(vals))) # Use 1.4826 * MAD.
    
  # Emperical Bayes FDR. Save likelihood ratios.
  numerator <- dnorm(vals, mu.f0, sigma.f0)
  denominator <- approx(density(vals), xout = vals)$y
  
  likelihood <- numerator / denominator
  
  # # Note: The estimated null distribution and empirical distribution can
  # # be plotted using these lines of code:
  # ordered_vals <- order(vals)
  # plot(vals[ordered_vals], numerator[ordered_vals], col = "blue", type = "l",
  #      main = "Empirical Bayes fdr")
  # lines(vals[ordered_vals], denominator[ordered_vals], 
  #       col = "orange")
  # lines(vals[ordered_vals], numerator[ordered_vals] / denominator[ordered_vals], 
  #       col = "red")

  is_significant <- likelihood > thrsh
  
  # Find significant edges.
  sig_scores <- matrix(0, nrow = p, ncol = p)
  sig_scores[lower.tri(sig_scores)] <- vals * is_significant
  sig_scores <- sig_scores + t(sig_scores)
  
  return(sig_scores)
}
