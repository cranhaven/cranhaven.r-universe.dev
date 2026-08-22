#' Build Analysis Data Frame
#'
#' Combines a multi-gene expression matrix with sample metadata into a
#' long-format, analysis-ready data frame. Each row represents one gene-sample
#' observation, with human-readable gene names resolved from the annotation
#' data frame.
#'
#' @param expr.matrix Numeric matrix of gene expression values as returned by
#'   \code{get.gene.expression()}. Rows are probe IDs, columns are sample IDs.
#' @param phenotype Data frame of sample metadata as returned by
#'   \code{extract.expression()$phenotype}. Row names must correspond to the
#'   column names of \code{expr.matrix}.
#' @param genes Data frame of gene annotations as returned by
#'   \code{extract.expression()$gene}. Row indices must correspond to the probe
#'   IDs (row names) of \code{expr.matrix}. Used to resolve probe IDs to the
#'   human-readable gene names stored in the \code{"Gene symbol"} column.
#' @param group.col Character. Name of the column in \code{phenotype} to use as
#'   the grouping variable. Default is \code{"disease.state"}.
#'
#' @return A long-format data frame with one row per gene-sample pair and three
#'   columns:
#' \describe{
#'   \item{gene}{Character. Human-readable gene name resolved from the gene
#'     annotation data frame.}
#'   \item{expression}{Numeric. Expression value for that gene-sample pair.}
#'   \item{group}{Character or factor. Group label for each sample, renamed
#'     from the \code{group.col} column in \code{phenotype} for consistency
#'     with downstream functions.}
#' }
#' Rows with \code{NaN} or \code{NA} expression values are removed.
#'
#' @details
#' The function pivots \code{expr.matrix} from wide format (probes x samples)
#' to long format, merges sample metadata by sample ID, resolves probe IDs to
#' gene names using the annotation data frame, and selects the three columns
#' needed for analysis. The output is the standard input for
#' \code{analyze.gene()}, \code{gene.analysis.plot()}, and \code{fit.lasso()}.
#'
#' Requires \code{tidyr} for the pivot step. Ensure \code{tidyr} is listed
#' under \code{Imports} in the package DESCRIPTION.
#'
#' @examples
#' \donttest{
#' geo <- extract.expression(load.geo.soft(accession = "GDS3268", log.transform = TRUE))
#' probe <- find.probe.by.gene(geo$gene, c("MUC20", "ADH1A"))
#' expr <- get.gene.expression(geo$expression, probe)
#' df <- build.analysis.df(expr, geo$phenotype, geo$gene)
#' head(df)
#' }
#'
#' @export
build.analysis.df <- function(expr.matrix, phenotype, genes, group.col="disease.state")
{
  if(dim(expr.matrix)[2] == 1)
  {
    expr.matrix <- t(expr.matrix)
  }
  df <- as.data.frame(expr.matrix)
  df$probe <- rownames(df)

  long.df <- tidyr::pivot_longer(
    df,
    cols = -tidyr::all_of("probe"),
    names_to = "sample",
    values_to = "expression"
  )
  long.df <- merge(
    long.df,
    phenotype,
    by.x = "sample",
    by.y = "row.names"
  )

  # Resolve numeric probe IDs to human-readable gene names
  long.df$gene <- genes[genes$ID %in% long.df$probe, ][["Gene symbol"]]

  long.df <- long.df[!is.nan(long.df$expression), ]
  long.df <- na.omit(long.df)

  result <- long.df[, c("gene", "expression", group.col)]
  colnames(result)[3] <- "group"

  return(result)
}
