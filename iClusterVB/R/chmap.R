#' Generates a heat map based on an iClusterVB object
#'
#' @param fit A fitted iClusterVB object.
#' @param rho The minimum probability of inclusion for features shown on the
#'   heatmap. Default is 0.5. 0 would show all features. Only useful for
#'   VS_method = 1.
#' @param title A character vector or a single value. Title of the heat map. The
#'   default is "View 1 - Distribution 1", ..., "View R - Distribution R".
#' @param cols A vector of colors to use for the clusters. The default is a
#'   random selection of colors.
#' @param ... Additional arguments to be passed down to
#'   \code{\link[pheatmap]{pheatmap}}
#'
#' @return Returns a heat map for each data view.
#' @examples
#' # Setting up the data
#' dat1 <- list(
#'   gauss_1 = sim_data$continuous1_data[c(1:20, 61:80, 121:140, 181:200), 1:75],
#'   gauss_2 = sim_data$continuous2_data[c(1:20, 61:80, 121:140, 181:200), 1:75],
#'   poisson_1 = sim_data$count_data[c(1:20, 61:80, 121:140, 181:200), 1:75],
#'   multinomial_1 = sim_data$binary_data[c(1:20, 61:80, 121:140, 181:200), 1:75]
#' )
#'
#' # Recoding `0`s to `2`s
#' dat1$multinomial_1[dat1$multinomial_1 == 0] <- 2
#'
#' dist <- c(
#'   "gaussian", "gaussian",
#'   "poisson", "multinomial"
#' )
#'
#' fit_iClusterVB <- iClusterVB(
#'   mydata = dat1,
#'   dist = dist,
#'   K = 4,
#'   initial_method = "VarSelLCM",
#'   VS_method = 1,
#'   max_iter = 25
#' )
#'
#'
#' # We can set the colors, turn off scaling and set titles
#'
#' chmap(fit_iClusterVB,
#'   cols = c("red", "blue", "green", "purple"),
#'   title = c("Gene Expression", "DNA Methylation", "Copy Number", "Mutation Status"),
#'   scale = "none"
#' )
#' @export chmap
#' @import pheatmap
#' @useDynLib iClusterVB, .registration=TRUE


chmap <- function(fit, rho = 0.5, cols = NULL, title = NULL, ...) {
  if (is.null(cols)) {
    cols <- colors()[sample(1:600, size = length(unique(fit$cluster)))]
  }

  ifelse(is.null(title), title <- paste(
    "View", 1:length(fit$mydata), "-",
    tools::toTitleCase(fit$dist[1:length(fit$mydata)])
  ),
  title
  )

  formals(pheatmap)[c(
    "cluster_rows", "cluster_cols", "color", "treeheight_row", "treeheight_col", "scale",
    "show_colnames", "show_rownames", "annotation_names_row", "annotation_names_col", "annotation_colors"
  )] <- list(
    cluster_rows = FALSE,
    cluster_cols = FALSE,
    color = colorRampPalette(c("navy", "white", "firebrick3"))(50), treeheight_row = 0,
    treeheight_col = 0, scale = "row", show_colnames = FALSE,
    show_rownames = FALSE, annotation_names_row = FALSE,
    annotation_names_col = FALSE,
    annotation_colors = list(Clusters = setNames(cols, paste("Cluster", sort(unique(fit$cluster)))))
  )


  if (is.null(fit$model_parameters$rho)) {
    dfs <- mapply(function(fit) as.data.frame(t(data.matrix(fit))),
      fit = fit$mydata, SIMPLIFY = FALSE
    )

    mat_col <- data.frame(Clusters = paste("Cluster", as.numeric(fit$cluster)))
    rownames(mat_col) <- colnames(dfs[[1]])

    dfs <- lapply(X = dfs, FUN = function(dfs) dfs[, order(as.numeric(fit$cluster))])

    plot_list <- mapply(
      FUN = pheatmap, dfs, main = as.character(title),
      MoreArgs = list(annotation_col = mat_col),
      ...,
      SIMPLIFY = FALSE
    )

    plot_list <- sapply(plot_list, "[", 4)
  } else if (!is.null(fit$model_parameters$rho)) {
    names <- lapply(fit$model_parameters$rho, function(fit) which(fit > rho))

    dfs <- mapply(function(fit, names) as.data.frame(t(data.matrix(fit[, names]))),
      fit = fit$mydata, names = names, SIMPLIFY = FALSE
    )

    mat_col <- data.frame(Clusters = paste("Cluster", as.numeric(fit$cluster)))
    rownames(mat_col) <- colnames(dfs[[1]])

    dfs <- lapply(X = dfs, FUN = function(dfs) dfs[, order(as.numeric(fit$cluster))])

    plot_list <- mapply(
      FUN = pheatmap, dfs, main = as.character(title),
      MoreArgs = list(annotation_col = mat_col),
      ..., SIMPLIFY = FALSE
    )

    plot_list <- sapply(plot_list, "[", 4)
  }

  return(invisible(plot_list))
}
