#' Plot function for 'dnapath' object.
#' 
#' Uses the plotting functions for networks from the `SeqNet` package
#' \insertCite{seqnet}{dnapath}
#' @param x A 'dnapath' object from \code{\link{dnapath}}.
#' @param alpha Threshold for p-values to infer differentially connected edges.
#' If NULL (the default) then no edges are removed from the plot.
#' @param monotonized If TRUE, monotonized (i.e. step-down) p-values from the 
#' permutation test will be used.
#' @param only_dc If TRUE, only differentially connected edges will be shown; 
#' any edges that are present in both groups are hidden. If FALSE, the edges
#' shared by both groups are shown. If a non-sparse estimator for network
#' edges is used, then the graph may be dense and setting this argument to TRUE 
#' will be useful for highlighting the DC edges.
#' @param require_dc_genes If TRUE, the gene-level differential connectivity
#' p-value of the two genes for a given edge are also considered when deciding
#' whether an edge is differentially connected. If neither gene is significantly
#' differentially connected, then the edge between them will not be either.
#' @param scale_edges (Optional) multiplier for edge widths.
#' @param scale_nodes (Optional) multiplier for node radius
#' @param ... Additional arguments are passed into the plotting function
#' \code{\link[SeqNet]{plot_network}}.
#' @return Plots the differential network and returns the graph object. 
#' See \code{\link[SeqNet]{plot_network}} for details.
#' @references 
#' \insertRef{seqnet}{dnapath}
#' @export
#' @examples 
#' data(meso)
#' data(p53_pathways)
#' set.seed(0)
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways,
#'                    group_labels = meso$groups, n_perm = 10)
#' # Plot of the differential network for pathway 1.
#' plot(results[[1]]) 
#' # Plot of the differential network for pathway 1; remove any edges from
#' # the plot that have p-values above 0.1.
#' plot(results[[1]], alpha = 0.1) 
plot.dnapath <- function(x, alpha = NULL, monotonized = FALSE, 
                         only_dc = FALSE, require_dc_genes = FALSE,
                         scale_edges = 1, scale_nodes = 1,
                         ...) {
  if(!is(x, "dnapath")) 
    stop(paste0("'", deparse(substitute(x)), 
                "' is not a 'dnapath' object."))
  
  mean_expr <- get_mean_expr_mat(x)
  # If there are any NA mean expression values, set them to 0.
  if(any(is.na(mean_expr))) {
    mean_expr[which(is.na(mean_expr))] <- 0
  }
  # If any mean expression values are negative, set all values equal
  # to make fold-changes equal to 1.
  if(any(mean_expr < 0)) {
    warning("Some expression values are negative. Node sizes cannot be scaled.")
    mean_expr[] <- 10
  }
  
  genes_in_dnw <- get_genes(x)
  p <- length(genes_in_dnw)
  nw1 <- x$pathway$nw1
  nw2 <- x$pathway$nw2
  # If nw1 and nw2 are opposite sign, use max of |x - 0| or |0 - y|. 
  # Otherwise, use |x + y|/2 for the dc value.
  nw_vals <- ifelse(nw2 * nw1 < 0, 
                    pmax(abs(nw2), abs(nw1)),
                    abs(nw2 + nw1) / 2) # Use x - 0, 0 - x, or x - y.
  
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
  
  # If alpha is provided, make sure it is not too small relative to the
  # size of the permutation test.
  if(!is.null(alpha)) {
    alpha <- max(alpha, get_min_alpha(x))
  } else {
    alpha <- 1
  }
  
  # Determine which edges are significantly differentially connected.
  is_dc_edge <- ((p_values <= alpha))
  if(require_dc_genes) {
    is_dc_edge <- is_dc_edge & (p_values_gene_min <= alpha)
  }
  
  # If `only_dc` is TRUE, then set any non-significant edges to zero.
  if(only_dc) {
    nw_vals[!is_dc_edge] <- 0
  }
  index_edges <- which(abs(nw_vals) > 10^-10)
  index_dc_edges <- which(is_dc_edge[index_edges])
  
  edge_weights <- 8 * abs(nw_vals) / max(c(abs(x$pathway$nw1), abs(x$pathway$nw2)))
  edge_weights <- edge_weights[index_edges]
  
  if(any(is.nan(edge_weights))) {
    # 0 / 0 association scores will result in NaN values. Set these to 0.
    edge_weights[is.nan(edge_weights)] <- 0
  }
  
  fold_change <- log2(mean_expr[2, ] / mean_expr[1, ])
  if(any(is.nan(fold_change))) {
    # 0 / 0 expression will result in NaN values. Set these to 0.
    fold_change[is.nan(fold_change)] <- 0
  }
  mean_expr <- apply(mean_expr, 2, max)
  mean_expr <- mean_expr / max(mean_expr)
  node_weights <- log2(1 + mean_expr) * 26 + 1
  
  
  # Positive values indicate increase in association from group 1 to group 2.
  # nw2 - nw1
  dnw <- matrix(0, p, p)
  dnw[lower.tri(dnw)] <- nw_vals 
  dnw <- dnw + t(dnw)
  colnames(dnw) <- genes_in_dnw
  
  # TODO: set color based on group with higher magnitude of association.
  # what if going from -0.3 to 0.3? No change in magnitude, but largest difference.
  # Maybe just set color = to sign in group 1.
  g0 <- SeqNet::plot_network(dnw != 0, display_plot = FALSE, as_subgraph = FALSE)
  g <- SeqNet::plot_network(dnw != 0, display_plot = FALSE, ...)
  v <- igraph::V(g$graph)
  # If as_subgraph argument is used, the vertices and edges need to be subset.
  index_genes <- match(names(igraph::V(g$graph)), colnames(dnw))
  m <- match(attr(igraph::E(g$graph), "vnames"),
             attr(igraph::E(g0$graph), "vnames"))
  
  g$vertex.size <- (node_weights * scale_nodes)[index_genes]
  g$edge.width <- (edge_weights * scale_edges)[m]
  g$vertex.color <- (ifelse(fold_change > 0, 
                            rgb(1, 0.19, 0.19, 
                                pmax(0, pmin(1, abs(fold_change) / 2))),
                            rgb(0.31, 0.58, 0.8, 
                                pmax(0, pmin(1, abs(fold_change) / 2)))))[index_genes]
  g$vertex.frame.color <- rgb(0.3, 0.3, 0.3, 0.5)
  # Rescale edge weights to be used for color transparency.
  edge_weights <- ((log(pmin(1, pmax(alpha, p_values[index_edges][index_dc_edges]) - alpha + 0.05)) / log(0.05))^2)
  if(any(is.nan(edge_weights))) edge_weights <- 0 # No edges are significantly DC.
  
  g$edge.color <- rep(rgb(0, 0, 0, 1), length(index_edges))
  g$edge.color[index_dc_edges] <- 
    (ifelse(abs(nw2)[index_edges][index_dc_edges] > abs(nw1)[index_edges][index_dc_edges], 
            rgb(1, 0.19, 0.19, edge_weights),
            rgb(0.31, 0.58, 0.8, edge_weights)))
  g$vertex.label.cex <- 1
  
  mar <- par("mar")
  on.exit(par(mar = mar))
  par(mar = rep(0, 4))
  plot(g) # Calls the function SeqNet:::plot.network_plot.
  
  invisible(g)
}


#' Plot the expression values of two genes
#' 
#' Inspired by the \code{plotCors} function from the DGCA package,
#' this function is used to plot the expression values of two genes contained
#' in the differential network analysis results. This is useful for comparing
#' the marginal relationship between two genes. Note, however, that this 
#' visualization is not able to show conditional associations. 
#' @param x A 'dnapath' or 'dnapath_list' object from \code{\link{dnapath}}.
#' @param gene_A The name of the first gene to plot. Must be one of the names
#' in \code{\link{get_genes}}(x).
#' @param gene_B The name of the second gene to plot. Must be one of the names
#' in \code{\link{get_genes}}(x).
#' @param method A charater string, either "lm" or "loess" (the default) 
#' used by \code{\link[ggplot2]{geom_smooth}} to summarize the marginal
#' gene-gene association. For no line, set method = NULL.
#' @param alpha Sets the transparancy of the points, used to set alpha in
#' \code{\link[ggplot2]{geom_point}}.
#' @param se_alpha Sets the transparancy of the confidence band around 
#' the association trend line. Set to 0 to remove the band.
#' @param use_facet If TRUE, the groups are plotted in separate graphs
#' using the \code{link[ggplot2]{facet_wrap}} method.
#' @param scales Only used if do_facet_wrap is TRUE. See 
#' \code{link[ggplot2]{facet_wrap}} for details.
#' @param colors A vector of length two containing the rgb colors used for the 
#' two groups.
#' @return Plots the differential network and returns the ggplot object. 
#' Additional modifications can be applied to this object just
#' like any other ggplot.
#' @references 
#' \insertRef{seqnet}{dnapath}
#' @export
#' @examples 
#' data(meso)
#' data(p53_pathways)
#' set.seed(0)
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways,
#'                    group_labels = meso$groups, n_perm = 10)
#' # Plot of the marginal association between the first two genes.
#' genes <- get_genes(results)[1:2]
#' g <- plot_pair(results, genes[1], genes[2])
#' # The ggplot object, g, can be further modified.
#' # Here we move the legend and use a log scale for the expression values
#' # (the log scale doesn't help with these data but is shown for demonstration).
#' g <- g +
#'   ggplot2::theme(legend.position = "bottom") +
#'   ggplot2::scale_x_log10() +
#'   ggplot2::scale_y_log10()
#' g
plot_pair <- function(x, gene_A, gene_B, method = "loess", alpha = 0.5, 
                      se_alpha = 0.1, use_facet = FALSE, scales = "fixed",
                      colors = c(rgb(0.31, 0.58, 0.8, 0.9),
                                 rgb(1, 0.19, 0.19, 0.9))) {
  if(!is(x, "dnapath") && !is(x, "dnapath_list")) {
    stop('x must be a "dnapath" or "dnapath_list" object.')
  }
  
  # Extract probabilistic group labels from the dnapath results.
  group_prob = x$param$group_prob
  # Initialize all subjects as the first group.
  groups <- rep(colnames(group_prob)[1], nrow(group_prob))
  # Re-assign subjects that have greater than 50% probability of being in 
  # the second group as such.
  groups[group_prob[, 2] > 0.5] <- colnames(group_prob)[2]
  
  index_A <- match(gene_A, colnames(x$param$x))
  index_B <- match(gene_B, colnames(x$param$x))
  
  expr_A <- x$param$x[, index_A]
  expr_B <- x$param$x[, index_B]
  
  df <- tibble::tibble(group = groups,
                       A = expr_A, 
                       B = expr_B)
  g <- ggplot2::ggplot(df, ggplot2::aes(x = .data$A, y = .data$B, 
                                        color = .data$group))
  if (use_facet) {
    g <- g + ggplot2::facet_wrap(. ~ .data$group, scales = scales)
  }
  g <- g + ggplot2::geom_point(alpha = alpha)
  if (!is.null(method) && !is.na(method)) {
    g <- g + ggplot2::geom_smooth(method = method, alpha = se_alpha)
  }
  g <- g + 
    ggplot2::theme_bw() + 
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(x = paste("Expression of", gene_A), 
                  y = paste("Expression of", gene_B), color = "Group")
  
  return(g)
}



#' Get the two association networks
#' 
#' Extracts the estimated association network for each group from the
#' differential network analysis results.
#' @param x A 'dnapath' object from \code{\link{dnapath}}.
#' @return A list of two association matrices.
#' @note The two matrices can be plotted using the 
#' \code{\link[SeqNet]{plot_network}} function from the SeqNet package, as
#' illustrated in the examples below.
#' @export
#' @examples 
#' data(meso)
#' data(p53_pathways)
#' set.seed(0)
#' results <- dnapath(x = meso$gene_expression, pathway_list = p53_pathways,
#'                    group_labels = meso$groups, n_perm = 10)
#' # Extract the two estimated association networks for the first pathway
#' nw <- get_networks(results[[1]])
#' # Plot the networks using the SeqNet::plot_network function.
#' # Note that the `compare_graph` argument is used so that the same node layout
#' # is used across all of the plots.
#' # Plot the two networks (in separate plots)
#' g <- SeqNet::plot_network(nw[[1]])
#' SeqNet::plot_network(nw[[1]], compare_graph = g)
#' # Plot of the differential network for pathway 1.
#' # Again, the `compare_graph` argument is used to maintain the same layout.
#' plot(results[[1]], compare_graph = g) 
#' # We see that genes 51230 and 7311 show strong differential connectivity.
#' # The plot_pair() function can be used to investigate these two genes further.
#' plot_pair(results[[1]], "51230", "7311")
get_networks <- function(x) {
  if(!is(x, "dnapath")) 
    stop(paste0("'", deparse(substitute(x)), "' is not a 'dnapath' object."))
  
  gene_names <- get_genes(x)
  p <- length(gene_names)
  
  nw_list <- list(nw1 = matrix(0, p, p),
                  nw2 = matrix(0, p, p))
  nw_list[[1]][lower.tri(nw_list[[1]])] <- x$pathway$nw1
  nw_list[[1]] <- nw_list[[1]] + t(nw_list[[1]])
  
  nw_list[[2]][lower.tri(nw_list[[2]])] <- x$pathway$nw2
  nw_list[[2]] <- nw_list[[2]] + t(nw_list[[2]])
  
  colnames(nw_list[[1]]) <- gene_names
  colnames(nw_list[[2]]) <- gene_names
  names(nw_list) <- x$param$groups
  
  return(nw_list)
}
