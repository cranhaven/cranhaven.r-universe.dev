#' Get mapping between transcripts and genes
#'
#' This function uses the
#' [biomaRt package](https://doi.org/doi:10.18129/B9.bioc.biomaRt).
#'
#' @param organism String used to pass `paste0(organism, "_gene_ensembl")` as the
#'   `dataset` argument to [biomaRt::useEnsembl()]. To see available datasets,
#'   do `mart = biomaRt::useEnsembl("genes"); biomaRt::listDatasets(mart)`.
#' @param version Passed to [biomaRt::useEnsembl()]. `NULL` indicates the latest
#'   version. To see available versions, do `biomaRt::listEnsemblArchives()`.
#' @param outputDir Directory in which to save the result, a file named
#'   "tx2gene.csv.gz". If `NULL`, no file is saved.
#' @param checkArgsOnly Logical indicating whether to only check function
#'   arguments. Used for testing.
#'
#' @return If `checkArgsOnly` is `FALSE`, a data.table based on the result from
#'   [biomaRt::getBM()], with an attribute "version". Otherwise `0`.
#'
#' @seealso [seeker()], [tximport()]
#'
#' @export
getTx2gene = function(
  organism = 'mmusculus', version = NULL, outputDir = 'data',
  checkArgsOnly = FALSE) {

  assertString(organism)
  assertNumber(version, null.ok = TRUE)
  assertString(outputDir, null.ok = TRUE)
  withr::local_options(timeout = 600)

  if (is.null(version)) { # let's be strict
    arch = setDT(biomaRt::listEnsemblArchives())
    version = as.integer(arch[arch$current_release == '*']$version)}

  if (!is.null(outputDir)) {
    assertPathForOutput(outputDir, overwrite = TRUE)}

  assertLogical(checkArgsOnly)
  if (checkArgsOnly) return(0)

  dataset = paste0(organism, '_gene_ensembl')
  mart = biomaRt::useEnsembl('genes', dataset, version = version)
  attribs = c('ensembl_transcript_id', 'ensembl_gene_id')
  t2g = setDT(biomaRt::getBM(attributes = attribs, mart = mart))
  data.table::setattr(t2g, 'version', version)

  if (!is.null(outputDir)) {
    if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE)
    fwrite(t2g, file.path(outputDir, 'tx2gene.csv.gz'))}
  return(t2g)}


#' Run tximport on RNA-seq quantifications
#'
#' This function uses the
#' [tximport package](https://doi.org/doi:10.18129/B9.bioc.tximport).
#'
#' @param inputDir Directory that contains the quantification directories.
#' @param tx2gene `NULL` or data.frame of mapping between transcripts and
#'   genes, as returned by [getTx2gene()], passed to [tximport::tximport()].
#' @param samples Names of quantification directories to include. `NULL`
#'   indicates all.
#' @param outputDir Directory in which to save the result, a file named
#'   "tximport_output.qs", using [qs::qsave()]. If `NULL`, no file is saved.
#' @param type Passed to [tximport::tximport()].
#' @param countsFromAbundance Passed to [tximport::tximport()].
#' @param ignoreTxVersion Passed to [tximport::tximport()].
#' @param ... Additional arguments passed to [tximport::tximport()].
#'
#' @return A list, as returned by [tximport::tximport()], invisibly.
#'
#' @seealso [seeker()], [getTx2gene()]
#'
#' @export
tximport = function(
  inputDir, tx2gene, samples = NULL, outputDir = 'data',
  type = c('salmon', 'kallisto'), countsFromAbundance = 'lengthScaledTPM',
  ignoreTxVersion = TRUE, ...) {

  path = .N = . = NULL

  assertString(inputDir)
  assertDirectoryExists(inputDir)
  assertDataFrame(tx2gene, null.ok = TRUE)
  assertCharacter(samples, null.ok = TRUE)
  assertString(outputDir, null.ok = TRUE)
  if (!is.null(outputDir)) {
    assertPathForOutput(outputDir, overwrite = TRUE)
    if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE)}

  txOut = is.null(tx2gene)

  type = match.arg(type)
  pat = switch(
    type, salmon = 'quant\\.sf(\\.gz)?$', kallisto = 'abundance\\.h5$')

  d = data.table(path = dir(inputDir, pat, full.names = TRUE, recursive = TRUE))
  set(d, j = 'sample', value = basename(dirname(d$path)))
  d = d[, .(path = path[.N]), by = 'sample'] # in case quant.sf and quant.sf.gz
  paths = d$path
  names(paths) = d$sample

  if (!is.null(samples)) {
    paths = paths[names(paths) %in% samples]} # ok if samples has duplicates

  txi = tximport::tximport(
    paths, txOut = txOut, tx2gene = tx2gene, type = type,
    countsFromAbundance = countsFromAbundance,
    ignoreTxVersion = ignoreTxVersion, ...)

  if (!is.null(outputDir)) {
    qs::qsave(txi, file.path(outputDir, 'tximport_output.qs'))}
  invisible(txi)}
