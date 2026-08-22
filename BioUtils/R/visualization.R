#' Plot Gene Expression with Automatic Statistical Analysis
#'
#' Generates an annotated visualization of gene expression between two groups.
#' For a single-gene data frame, the plot is annotated with statistical results
#' from \code{analyze.gene()}. For a multi-gene data frame, the plot is
#' faceted by gene and annotations are omitted, use \code{analyze.gene()} on
#' each gene subset for per-gene statistics.
#'
#' @param df A data frame as produced by \code{build.analysis.df()}, containing:
#' \describe{
#'   \item{expression}{Numeric vector of gene expression values.}
#'   \item{group}{Character or factor vector with exactly two group labels.}
#'   \item{gene}{Character vector of gene names. Optional; when present and
#'     containing more than one unique value, enables faceted multi-gene
#'     plotting.}
#' }
#'
#' @param alpha Numeric. Significance level passed to \code{analyze.gene()}.
#'   Default is \code{0.05}. Only used in single-gene mode.
#' @param n.boot Integer. Number of bootstrap resamples for the confidence
#'   interval, passed to \code{analyze.gene()}. Default is \code{1000}.
#'   Only used in single-gene mode.
#' @param show.points Logical. Whether to overlay jittered individual sample
#'   points on the violin/boxplot. Default is \code{TRUE}.
#'
#' @return A \code{ggplot} object. In single-gene mode the plot includes a
#'   text annotation with p-value, Cohen's d, and bootstrapped CI, and a
#'   subtitle with the full interpretation string. In multi-gene mode the
#'   plot is faceted by gene with no statistical annotation.
#'
#' @details
#' Single-gene mode is triggered when the \code{gene} column is absent or
#' contains exactly one unique value. Multi-gene mode is triggered when
#' \code{gene} contains more than one unique value, producing a
#' \code{facet_wrap(~ gene)} layout. The plot theme is consistent across
#' both modes and matches the style conventions of the BioUtils package.
#'
#' @examples
#' \donttest{
#' geo <- extract.expression(load.geo.soft(accession = "GDS3268", log.transform = TRUE))
#'
#' # Single-gene plot with statistical annotation
#' probe <- find.probe.by.gene(geo$gene, "mucin 20, cell surface associated")
#' expr <- get.gene.expression(geo$expression, probe)
#' df <- build.analysis.df(expr, geo$phenotype, geo$gene)
#' gene.analysis.plot(df)
#'
#' # Multi-gene faceted plot
#' probes <- find.probe.by.gene(geo$gene, c(
#'   "mucin 20, cell surface associated",
#'   "alcohol dehydrogenase 1A (class I), alpha polypeptide"
#' ))
#' expr.multi <- get.gene.expression(geo$expression, probes)
#' df.multi <- build.analysis.df(expr.multi, geo$phenotype, geo$gene)
#' gene.analysis.plot(df.multi)
#' }
#'
#' @export
gene.analysis.plot <- function(df, alpha=0.05, n.boot=1000, show.points=TRUE)
{
  multi.gene <- "gene" %in% colnames(df) && length(unique(df$gene)) > 1

  # Base plot shared by both modes
  p <- ggplot2::ggplot(df, ggplot2::aes(x=group, y=expression)) +
    ggplot2::geom_violin(trim=FALSE, alpha=0.6) +
    ggplot2::geom_boxplot(width=0.1, outlier.shape=NA)

  if(show.points && !multi.gene)
  {
    p <- p + ggplot2::geom_jitter(width=0.15, alpha=0.4)
  }

  if(multi.gene)
  {
    p <- p +
      ggplot2::facet_wrap(~ gene, scales="free_y") +
      ggplot2::labs(
        title = "Gene Expression Comparison",
        x = "Group",
        y = "Expression"
      )
  }
  else
  {
    # Single-gene mode: run full statistical analysis and annotate
    result <- analyze.gene(df, alpha=alpha, n.boot=n.boot)
    ci <- result$confidence.interval

    label <- paste(
      "p = ", signif(result$p.value, 3),
      "\nd = ", round(result$effect.size, 2),
      " (", result$effect.size.class, ")",
      "\n", 1 - alpha, "% CI: [", signif(ci$lower, 2), ", ", signif(ci$upper, 2), "]"
    )

    p <- p +
      ggplot2::annotate(
        "text",
        x = 1.5,
        y = Inf,
        label = label,
        hjust = 0.5,
        vjust = 1.5,
        size = 4
      ) +
      ggplot2::labs(
        title = "Gene Expression Comparison",
        subtitle = result$interpretation,
        x = "Group",
        y = "Expression"
      )
  }

  p <- p + ggplot2::theme(
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color="black"),
    panel.grid.major = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(color="black", linewidth=1.2),
    title = ggplot2::element_text(size=20),
    axis.title = ggplot2::element_text(size=16),
    plot.title = ggplot2::element_text(hjust=0.5)
  )

  return(p)
}

#' Plot PCA of Sample Expression Profiles
#'
#' Reduces the dimensionality of the full expression matrix using Principal
#' Component Analysis and plots samples in the first two principal component
#' axes, colored by a phenotype variable. Useful for quality control,
#' batch effect detection, and exploratory assessment of sample clustering.
#'
#' @param expression.matrix Matrix of gene expression values. Rows are probes,
#'   columns are samples.
#' @param phenotype Data frame of sample metadata, as returned by
#'   \code{extract.expression()$phenotype}.
#' @param color.by Character. Column name in \code{phenotype} to use for
#'   point coloring. Default is \code{"disease.state"}.
#' @param scale Logical. Whether to scale probes to unit variance before PCA.
#'   Default is \code{TRUE}.
#'
#' @return A ggplot object showing samples as points in PC1 vs PC2 space,
#'   colored by the specified phenotype variable. The percentage of variance
#'   explained is shown on each axis label.
#'
#' @details
#' PCA is typically applied at the whole-dataset level before any gene
#' filtering, making it a useful first diagnostic after \code{load.geo.soft()}
#' and \code{extract.expression()}. Tight clustering of samples by disease
#' state in PC1/PC2 space suggests a strong and detectable expression signal.
#' Unexpected clustering by batch, sex, or other covariates can indicate the
#' need for covariate adjustment in downstream \code{run.limma.de()} models.
#'
#' @examples
#' \donttest{
#' # Create a synthetic expression matrix (50 probes x 12 samples)
#' set.seed(42)
#' expr.mat <- matrix(rnorm(600), nrow = 50, ncol = 12)
#' rownames(expr.mat) <- paste0("probe", 1:50)
#' colnames(expr.mat) <- paste0("sample", 1:12)
#'
#' # Create a matching phenotype data frame
#' phenotype <- data.frame(
#'   disease.state = rep(c("normal", "RCC"), each = 6),
#'   row.names = paste0("sample", 1:12)
#' )
#'
#' pca.plot(expr.mat, phenotype, color.by = "disease.state")
#' }
#'
#' @export
pca.plot <- function(expression.matrix, phenotype, color.by="disease.state", scale=TRUE)
{
  pca <- prcomp(t(expression.matrix), scale.=scale)
  var.explained <- round(summary(pca)$importance[2, 1:2] * 100, 1)

  df <- data.frame(
    PC1 = pca$x[, 1],
    PC2 = pca$x[, 2],
    group = phenotype[[color.by]]
  )

  ggplot2::ggplot(df, ggplot2::aes(x=PC1, y=PC2, color=group)) +
    ggplot2::geom_point(size=3, alpha=0.8) +
    ggplot2::labs(
      title = "PCA of Sample Expression Profiles",
      x = paste0("PC1 (", var.explained[1], "% variance)"),
      y = paste0("PC2 (", var.explained[2], "% variance)"),
      color = color.by
    ) +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color="black"),
      panel.grid.major = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color="black", linewidth=1.2),
      title = ggplot2::element_text(size=20),
      axis.title = ggplot2::element_text(size=16),
      plot.title = ggplot2::element_text(hjust=0.5)
    )
}

#' Plot Volcano Plot from Differential Expression Results
#'
#' Generates a volcano plot visualizing the relationship between statistical
#' significance and fold-change magnitude for all probes in a differential
#' expression analysis. Points are colored by whether they exceed user-defined
#' significance and fold-change thresholds.
#'
#' @param de.results Data frame as returned by \code{run.limma.de()}, containing
#'   at minimum the columns \code{logFC} and \code{adj.P.Val}.
#' @param fc.threshold Numeric. Absolute log2 fold-change cutoff for labeling
#'   genes as differentially expressed. Default is \code{1} (i.e., 2-fold).
#' @param fdr.threshold Numeric. Adjusted p-value cutoff. Default is \code{0.05}.
#'
#' @return A ggplot object showing each probe as a point with \code{logFC} on
#'   the x-axis and \code{-log10(adj.P.Val)} on the y-axis. Points are colored
#'   as upregulated, downregulated, or not significant. Dashed threshold lines
#'   are drawn at the specified cutoffs.
#'
#' @details
#' The volcano plot is the standard summary visualization for a TopTable from
#' \code{run.limma.de()} and serves as the genome-wide complement to
#' \code{gene.analysis.plot()}, which visualizes a single gene. Probes that
#' appear in the upper corners (high fold-change, low FDR) are the strongest
#' candidates for follow-up with \code{analyze.gene()} or inclusion in a gene
#' signature for \code{run.gsea()}.
#'
#' @examples
#' \donttest{
#' \donttest{
#' # Synthesize a minimal TopTable as returned by run.limma.de()
#' set.seed(42)
#' n <- 200
#' de.results <- data.frame(
#'   logFC = rnorm(n, mean = 0, sd = 2),
#'   adj.P.Val = runif(n, min = 0, max = 1),
#'   AveExpr = rnorm(n, mean = 8, sd = 1),
#'   row.names = paste0("probe", 1:n)
#' )
#' volcano.plot(de.results, fc.threshold = 1, fdr.threshold = 0.05)
#' }
#'
#' @export
volcano.plot <- function(de.results, fc.threshold=1, fdr.threshold=0.05)
{
  de.results$neg.log10.fdr <- -log10(de.results$adj.P.Val)
  de.results$status <- "Not Significant"
  de.results$status[de.results$adj.P.Val < fdr.threshold &
                      de.results$logFC > fc.threshold] <- "Upregulated"
  de.results$status[de.results$adj.P.Val < fdr.threshold &
                      de.results$logFC < -fc.threshold] <- "Downregulated"

  ggplot2::ggplot(de.results, ggplot2::aes(x=logFC, y=neg.log10.fdr, color=status)) +
    ggplot2::geom_point(alpha=0.5, size=1.5) +
    ggplot2::geom_vline(xintercept = c(-fc.threshold, fc.threshold), linetype = "dashed") +
    ggplot2::geom_hline(yintercept = -log10(fdr.threshold), linetype = "dashed") +
    ggplot2::scale_color_manual(values = c(
      "Upregulated" = "firebrick",
      "Downregulated" = "steelblue",
      "Not Significant" = "grey60"
    )) +
    ggplot2::labs(
      title = "Volcano Plot of Differential Expression",
      x = "Log2 Fold Change",
      y = "-Log10 Adjusted P-Value",
      color = "Status"
    ) +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color="black"),
      panel.grid.major = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color="black", linewidth=1.2),
      title = ggplot2::element_text(size=20),
      axis.title = ggplot2::element_text(size=16),
      plot.title = ggplot2::element_text(hjust=0.5)
    )
}

#' Plot Gene Co-expression Correlation Heatmap
#'
#' Visualizes a gene-by-gene correlation matrix as a clustered heatmap,
#' revealing groups of co-expressed genes within a candidate gene set.
#'
#' @param cor.matrix Numeric matrix as returned by \code{gene.correlation.matrix()}.
#'   Must be square and symmetric with row/column names corresponding to probe IDs.
#' @param gene.names Character vector of gene names to use as axis labels in
#'   place of probe IDs. Must be the same length and order as \code{cor.matrix}
#'   rows. Default is \code{NULL}, which uses probe IDs.
#'
#' @return A \code{pheatmap} object displaying the correlation matrix with
#'   hierarchical clustering applied to both rows and columns. Color scale
#'   runs from blue (strong negative correlation) through white (no correlation)
#'   to red (strong positive correlation).
#'
#' @details
#' Hierarchical clustering of the correlation matrix groups genes with similar
#' co-expression patterns into visible blocks on the heatmap. These blocks
#' often correspond to genes in the same pathway or under shared regulatory
#' control. This function is a targeted companion to \code{gene.correlation.matrix()}
#' for a pre-selected gene set, and complements the genome-wide network view
#' produced by WGCNA.
#'
#' @examples
#' \donttest{
#' mat <- matrix(
#'   c(1.00, 0.85, 0.62, 0.91,
#'     0.85, 1.00, 0.74, 0.88,
#'     0.62, 0.74, 1.00, 0.69,
#'     0.91, 0.88, 0.69, 1.00),
#'   nrow = 4,
#'   dimnames = list(
#'     c("BRCA1", "TP53", "MYC", "EGFR"),
#'     c("BRCA1", "TP53", "MYC", "EGFR")
#'   )
#' )
#' correlation.heatmap.plot(mat, gene.names = c("BRCA1", "TP53", "MYC", "EGFR"))
#' }
#'
#' @export
correlation.heatmap.plot <- function(cor.matrix, gene.names=NULL)
{
  if (!is.null(gene.names))
  {
    rownames(cor.matrix) <- gene.names
    colnames(cor.matrix) <- gene.names
  }

  pheatmap::pheatmap(
    cor.matrix,
    color = grDevices::colorRampPalette(c("steelblue", "white", "firebrick"))(100),
    breaks = seq(-1, 1, length.out = 101),
    clustering_method = "complete",
    display_numbers = TRUE,
    number_format = "%.2f",
    angle_col = 45,
    angle_row = 0,
    main = "Gene Co-expression Correlation Heatmap"
  )
}
