#' Find Probe IDs by Gene Name
#'
#' Searches the gene annotation data frame to find the probe IDs corresponding
#' to one or more gene names or descriptions.
#'
#' @param genes Data frame of gene annotations as returned by
#'   \code{extract.expression()$gene}. The second column (index 2) is expected
#'   to contain gene titles or descriptions.
#' @param gene.names Character vector of one or more gene names or descriptions
#'   to search for. Matching is exact and case-sensitive.
#'
#' @return Integer vector of probe IDs corresponding to the matched genes.
#'   The length of the vector equals the number of matches found. Returns an
#'   empty integer vector if no matches are found.
#'
#' @details
#' Probe IDs are the integer row names of the \code{genes} annotation data
#' frame. These IDs are used directly to index rows of the expression matrix
#' in \code{get.gene.expression()}. If multiple gene names are supplied, all
#' matching probe IDs are returned as a vector, which is then passed as-is to
#' \code{get.gene.expression()} to retrieve a multi-row expression matrix.
#'
#' @examples
#' \donttest{
#' geo <- extract.expression(load.geo.soft(accession = "GDS3268", log.transform = TRUE))
#'
#' # Single gene
#' probe <- find.probe.by.gene(geo$gene, "mucin 20, cell surface associated")
#'
#' # Multiple genes
#' probes <- find.probe.by.gene(geo$gene, c(
#'   "mucin 20, cell surface associated",
#'   "alcohol dehydrogenase 1A (class I), alpha polypeptide"
#' ))
#' }
#'
#' @export
find.probe.by.gene <- function(genes, gene.names)
{
  return(as.integer(rownames(genes[which(genes[, 2] %in% gene.names), ])))
}

#' Extract Gene Expression by Probe ID
#'
#' Retrieves expression values for one or more genes from the expression matrix
#' using their integer probe IDs.
#'
#' @param expression Numeric matrix of gene expression values as returned by
#'   \code{extract.expression()$expression}. Rows are probe IDs, columns are
#'   sample IDs.
#' @param probe.id Integer vector of one or more probe IDs as returned by
#'   \code{find.probe.by.gene()}.
#'
#' @return A numeric matrix with one row per probe and one column per sample.
#'   Row names are the probe IDs; column names are the sample IDs. When a
#'   single probe ID is supplied the result is still a matrix (not a vector),
#'   ensuring that \code{build.analysis.df()} receives a consistent input type.
#'
#' @details
#' The returned matrix is the direct input for \code{build.analysis.df()}.
#' Using \code{as.matrix()} ensures the single-probe case returns a one-row
#' matrix rather than a named vector, which keeps the \code{pivot_longer}
#' step in \code{build.analysis.df()} consistent regardless of how many genes
#' are queried.
#'
#' @examples
#' \donttest{
#' geo <- extract.expression(load.geo.soft(accession = "GDS3268", log.transform = TRUE))
#' probes <- find.probe.by.gene(geo$gene, c(
#'   "mucin 20, cell surface associated",
#'   "alcohol dehydrogenase 1A (class I), alpha polypeptide"
#' ))
#' expr <- get.gene.expression(geo$expression, probes)
#' dim(expr)  # probes x samples
#' }
#'
#' @export
get.gene.expression <- function(expression, probe.id)
{
  return(as.matrix(expression[probe.id, ]))
}

#' Resolve Probe IDs to Gene Names
#'
#' Converts one or more integer probe IDs back to their human-readable gene
#' title strings or short gene symbols using the gene annotation data frame.
#' This is the inverse operation of \code{find.probe.by.gene()}.
#'
#' @param genes Data frame of gene annotations as returned by
#'   \code{extract.expression()$gene}. Must contain columns \code{"ID"},
#'   \code{"Gene title"}, and \code{"Gene symbol"}.
#' @param probe.id Integer or character vector of one or more probe IDs to
#'   resolve, as returned by \code{find.probe.by.gene()}.
#' @param use.symbols Logical. If \code{FALSE} (default), returns full
#'   descriptive gene titles from the \code{"Gene title"} column, suitable
#'   for interpretation and reporting. If \code{TRUE}, returns short gene
#'   symbols from the \code{"Gene symbol"} column, suitable for plot axis
#'   labels and any downstream tool that expects standard gene symbols such
#'   as \code{run.gsea()}.
#'
#' @return Character vector of gene names corresponding to the supplied probe
#'   IDs. Returns empty strings \code{""} for probes with no annotation, which
#'   should be filtered with \code{which(result != "")}.
#'
#' @examples
#' \donttest{
#' geo <- extract.expression(load.geo.soft(accession = "GDS507",
#'                                         log.transform = TRUE))
#' de.results <- run.limma.de(geo)
#' top.probes <- rownames(head(de.results, 10))
#'
#' # Full titles for reporting
#' top.titles  <- get.gene.name(geo$gene, top.probes)
#'
#' # Short symbols for plot labels and GSEA
#' top.symbols <- get.gene.name(geo$gene, top.probes, use.symbols = TRUE)
#' }
#'
#' @export
get.gene.name <- function(genes, probe.id, use.symbols=FALSE)
{
  col <- if(use.symbols) "Gene symbol" else "Gene title"
  return(genes[which(genes$ID %in% probe.id), ][[col]])
}
