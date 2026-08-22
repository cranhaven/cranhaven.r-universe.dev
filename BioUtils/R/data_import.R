#' Load GEO Dataset from SOFT File
#'
#' Imports a GEO dataset and converts it into a structured ExpressionSet object.
#' If a local SOFT file is found at \code{file.path} it is loaded directly.
#' Otherwise the dataset is downloaded from NCBI GEO using \code{accession} and
#' cached in \code{tempdir()}.
#'
#' @param file.path Character. Path to a local GEO SOFT file. If the file does
#'   not exist and \code{accession} is provided, the dataset is downloaded
#'   automatically.
#' @param accession Character. GEO accession ID (e.g., \code{"GDS507"}). Only
#'   used when \code{file.path} does not exist. Default is \code{NULL}.
#' @param log.transform Logical. Whether to apply a log2 transformation to the
#'   expression matrix during import. Default is \code{FALSE}.
#'
#' @return An \code{ExpressionSet} object for use with \code{extract.expression()}.
#'
#' @examples
#' \donttest{
#' # Download automatically if not found locally
#' eset <- load.geo.soft("GDS507.soft", accession = "GDS507", log.transform = TRUE)
#'
#' # Download without a local file at all
#' eset <- load.geo.soft(NULL, accession = "GDS507", log.transform = TRUE)
#' }
#'
#' @export
load.geo.soft <- function(file.path=NULL, accession=NULL, log.transform=FALSE)
{
  if(!is.null(file.path) && file.exists(file.path))
  {
    return(GEOquery::GDS2eSet(GEOquery::getGEO(filename=file.path),
                              do.log2=log.transform))
  }

  if(is.null(accession))
  {
    stop("File not found and no accession provided. ",
         "Supply an accession (e.g., \"GDS507\") to download automatically.")
  }

  message(accession, " not found locally, downloading from NCBI GEO...")
  gds <- GEOquery::getGEO(accession, destdir=tempdir(), GSEMatrix=FALSE)
  return(GEOquery::GDS2eSet(gds, do.log2=log.transform))
}

#' Extract Expression and Metadata from GEO Dataset
#'
#' Decomposes an \code{ExpressionSet} object into its core components,
#' returning a named list that is used as the primary data structure
#' throughout BioUtils.
#'
#' @param eset An \code{ExpressionSet} object as returned by
#'   \code{load.geo.soft()}.
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{expression}{Numeric matrix of gene expression values. Rows are
#'     probe IDs, columns are sample IDs.}
#'   \item{phenotype}{Data frame of sample metadata (e.g., disease state,
#'     age, tissue). Row names are sample IDs matching the column names of
#'     \code{expression}.}
#'   \item{gene}{Data frame of probe-level gene annotations (e.g., gene
#'     symbol, gene title, chromosomal location). Row names are probe IDs
#'     matching the row names of \code{expression}.}
#' }
#'
#' @details
#' The three returned components form the basis of all downstream BioUtils
#' workflows. The \code{gene} data frame is passed to
#' \code{find.probe.by.gene()} for probe lookup, \code{expression} is
#' passed to \code{get.gene.expression()}, and \code{phenotype} is passed
#' to \code{build.analysis.df()} for group labeling.
#'
#' @examples
#' \donttest{
#' eset <- load.geo.soft(accession = "GDS507", log.transform = TRUE)
#' geo <- extract.expression(eset)
#' head(geo$phenotype)
#' dim(geo$expression)
#' }
#'
#' @export
extract.expression <- function(eset)
{
  if(!inherits(eset, "ExpressionSet"))
  {
    stop("Invalid argument: expected ExpressionSet, got ", class(eset))
  }

  gene.expression <- Biobase::exprs(eset)
  phenotype <- Biobase::pData(eset)
  genes <- Biobase::fData(eset)

  return(list(
    expression = gene.expression,
    phenotype = phenotype,
    gene = genes
  ))
}
