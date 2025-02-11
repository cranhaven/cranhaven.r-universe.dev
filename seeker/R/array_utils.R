#' Get supported microarray platforms
#'
#' @param type String indicating whether to get supported platforms for
#'   processing raw Affymetrix data using custom CDF or for mapping already
#'   processed data from probes to genes.
#'
#' @return A `data.table`.
#'
#' @export
getPlatforms = function(type = c('cdf', 'mapping')) {
  type = match.arg(type)
  f = if (type == 'cdf') 'platform_cdf.csv' else 'platform_mapping.csv'
  path = system.file('extdata', f, package = 'seeker')
  d = unique(fread(path, na.strings = ''))

  if (type == 'cdf') {
    set(d, j = 'ensembl', value = paste0(d$custom_cdf_prefix, 'ensgcdf'))
    set(d, j = 'entrez', value = paste0(d$custom_cdf_prefix, 'entrezgcdf'))}
  return(d)}


getCdfname = function(anno, geneIdType) {
  d = getPlatforms('cdf')
  cdfname = d[d$platform == anno][[geneIdType]]
  return(cdfname)}


#' Install custom CDF packages
#'
#' Install Brainarray custom CDFs for processing raw Affymetrix data. See
#' <http://brainarray.mbni.med.umich.edu/Brainarray/Database/CustomCDF/CDF_download.asp>.
#'
#' @param pkgs Character vector of package names, e.g., "hgu133ahsentrezgcdf".
#' @param ver Integer version number (25 as of 5 Jan 2021).
#' @param dryRun Logical indicating whether to actually install the packages.
#'
#' @return A character vector of URLs, invisibly.
#'
#' @export
installCustomCdfPackages = function(pkgs, ver = 25, dryRun = FALSE) {
  assertCharacter(pkgs, any.missing = FALSE)
  assertIntegerish(ver, len = 1L, any.missing = FALSE)
  assertFlag(dryRun)
  urlPre = glue('http://mbni.org/customcdf/{ver}.0.0')
  pkg = NULL

  urls = foreach(pkg = unique(pkgs), .combine = c) %do% {
    hint = substr(pkg, nchar(pkg) - 6, nchar(pkg) - 3)
    urlMid = switch(hint, ensg = 'ensg', rezg = 'entrezg')
    if (is.null(urlMid)) {
      warning(glue('Cannot install {pkg}, since it doesn\'t correspond ',
                   'to gene IDs from Ensembl or Entrez.'))
      NA_character_
    } else {
      glue('{urlPre}/{urlMid}.download/{pkg}_{ver}.0.0.tar.gz')}}

  names(urls) = pkgs
  if (!dryRun) utils::install.packages(urls[!is.na(urls)], repos = NULL)
  invisible(urls)}


getNaiveEsetGeo = function(
    study, outputDir, rawDir, platform = NULL, metadataOnly = FALSE) {

  gq = GEOquery::getGEO(study, destdir = outputDir, getGPL = FALSE)
  idx = if (length(gq) == 1L) {
    1L
  } else if (is.null(platform)) {
    return(list(rmaOk = glue('{study} uses multiple platforms, but params does ',
                             'not specify which one to use for processing.')))
  } else {
    which(sapply(gq, function(x) x@annotation) == platform)}

  if (length(idx) == 0) {
    return(list(rmaOk = glue(
      '{study} uses multiple platforms, none of which is {platform}.')))}
  eset = gq[[idx]]

  rmaOk = eset@annotation %in% getPlatforms('cdf')$platform
  procOk = eset@annotation %in% getPlatforms('mapping')$platform

  dSuppTmp = GEOquery::getGEOSuppFiles(
    study, makeDirectory = FALSE, baseDir = outputDir, fetch_files = FALSE)
  isRaw = grepl('_RAW\\.tar$', dSuppTmp$fname)

  if (metadataOnly) return(list(eset = eset))

  if (rmaOk && any(isRaw)) {
    dSupp = GEOquery::getGEOSuppFiles(
      study, makeDirectory = FALSE, baseDir = outputDir)
    paths = rownames(dSupp)[isRaw]
    for (path in paths) {
      utils::untar(path, exdir = rawDir)}
    unlink(paths)

  } else if (procOk) {
    rmaOk = FALSE
  } else {
    rmaOk = glue('{study} uses platform {eset@annotation}, ',
                 'which is not supported with the available data.')}

  return(list(eset = eset, rmaOk = rmaOk))}


getAeMetadata = function(study, type = c('search', 'studies')) {
  type = match.arg(type)
  # urlBase = 'https://www.ebi.ac.uk/arrayexpress/json/v3'
  # url = glue('{urlBase}/{type}/{study}')
  urlBase = 'https://www.ebi.ac.uk/biostudies/api/v1'
  url = if (type == 'search') {
    glue('{urlBase}/search?accession={study}')
  } else {
    glue('{urlBase}/studies/{study}')}

  raw = curl::curl_fetch_memory(url)
  x = jsonlite::fromJSON(rawToChar(raw$content))
  # x = if (type == 'experiments') {
  #   setDT(x$experiments$experiment)
  # } else {
  #   x$files$experiment$file}
  return(x)}


getNaiveEsetAe = function(study, outputDir, rawDir) {
  withr::local_options(timeout = max(300, getOption('timeout')))

  metaList = getAeMetadata(study, 'search')
  if (metaList$totalHits != 1L) {
    return(list(rmaOk = glue(
    "'{study}' does not match exactly one study, cannot proceed.")))}

  meta1 = getAeMetadata(study, 'studies')$section$subsections
  idx = sapply(meta1, function(y) identical(y$type, 'Assays and Data'))
  meta2 = meta1[[which(idx)]]$subsections
  idx = meta2$type == 'Array Designs'
  meta3 = meta2$links[[which(idx)]]

  if (length(meta3) != 1L) {
    return(list(rmaOk = glue(
      '{study} does not use exactly one platform, cannot proceed.')))}

  platform = meta3[[1L]]$url
  idx = sapply(meta1, function(y) identical(y$type, 'MIAME Score'))
  meta4 = meta1[[which(idx)]]$attributes
  hasRaw = meta4$value[meta4$name == 'Raw'] == '*'
  hasProc = meta4$value[meta4$name == 'Processed'] == '*'

  # arrays = expers[1L]$arraydesign
  # if (length(arrays) != 1L) {
  #   return(list(rmaOk = glue(
  #     '{study} does not use exactly one platform, cannot proceed.')))}
  # platform = arrays[[1L]]$accession
  #
  # files = getAeMetadata(study, 'files') # for one study, should be a data.frame
  # hasRaw = any(files$kind == 'raw')
  # hasProc = any(sapply(files$kind, function(k) any(k == 'processed', na.rm = TRUE)))
  #
  # if (metadataOnly) return(list(eset = eset))

  if ((platform %in% getPlatforms('cdf')$ae_accession) && hasRaw) {
    mage = ArrayExpress::getAE(study, path = outputDir, type = 'full')
    eset = ArrayExpress::ae2bioc(mage)
    rmaOk = TRUE
  } else {
    type = if (hasRaw && hasProc) 'full' else if (hasRaw) 'raw' else 'processed'
    mage = ArrayExpress::getAE(
      study, path = outputDir, type = type, extract = FALSE)
    eset = NULL
    rmaOk = glue('{study} does not have raw data from a supported ',
                 'Affymetrix platform. You take it from here.')}

  if (!is.null(mage$rawArchive)) {
    unlink(file.path(outputDir, mage$rawArchive))}

  if (!dir.exists(rawDir)) dir.create(rawDir)

  . = file.rename(file.path(outputDir, basename(mage$rawFiles)),
                  file.path(rawDir, basename(mage$rawFiles)))

  return(list(eset = eset, rmaOk = rmaOk))}


methods::setClass('Eset', slots = c(annotation = 'character'))


getNaiveEsetLocal = function(study, platform) {
  rmaOk = platform %in% getPlatforms('cdf')$platform
  # procOk = platform %in% getPlatforms('mapping')$platform
  # TODO: allow processed data?
  if (rmaOk) {
    eset = methods::new('Eset', annotation = platform)
  } else {
    eset = NULL
    rmaOk = glue(
      '{study} uses platform {platform}, which is not supported for local data.')}
  return(list(eset = eset, rmaOk = rmaOk))}


seekerRma = function(inputDir, cdfname) {
  withr::local_dir(inputDir)
  eset = affy::justRMA(cdfname = cdfname)
  emat = eset@assayData$exprs
  rownames(emat) = gsub('_at$', '', rownames(emat))
  return(emat)}


stripFileExt = function(x) {
  y = gsub('\\.cel(\\.gz)?$', '', x, ignore.case = TRUE)
  return(y)}


getNewEmatColnames = function(old, repo) {
  if (repo == 'geo') {
    r = regexpr('^GSM\\d+', old)
    new = substr(old, r, attr(r, 'match.length'))
  } else {
    new = stripFileExt(old)}
  return(new)}


getProbeGeneMappingDirect = function(featureDt, geneColname, probeColname = 'ID') {
  mapping = featureDt[, c(probeColname, geneColname), with = FALSE]
  mapping = mapping[apply(mapping, 1, function(x) all(!is.na(x) & x != '')), ]
  setnames(mapping, c(probeColname, geneColname), c('probe_set', 'gene_id'))

  idx = attr(regexpr('^[0-9]+', mapping$gene_id), 'match.length')
  mapping = mapping[idx > 0]
  set(mapping, j = 'gene_id', value = substr(mapping$gene_id, 1, idx[idx > 0]))
  return(mapping)}


getProbeGeneMappingAnno = function(featureDt, dbName, interName) {
  mappingProbeTmp = featureDt[
    !is.na(featureDt[[interName]]) & featureDt[[interName]] != '',
    c('ID', interName), with = FALSE]
  setnames(mappingProbeTmp, c('ID', interName), c('probe_set', 'geneInter'))

  pkgName = paste0(substr(dbName, 1, 9), '.db')
  if (!requireNamespace(pkgName, quietly = TRUE)) {
    BiocManager::install(pkgName)}

  mapTmp1 = eval(parse(text = glue('{pkgName}::{dbName}')))
  mapTmp2 = AnnotationDbi::mappedkeys(mapTmp1)
  mapTmp3 = as.list(mapTmp1[mapTmp2])

  mappingIdInter = data.table(
    geneInter = rep.int(names(mapTmp3), lengths(mapTmp3)),
    gene_id = unlist(mapTmp3, use.names = FALSE))

  if (dbName == 'org.Hs.egUNIGENE2EG') {
    set(mappingIdInter, j = 'geneInter',
        value = gsub('^Hs\\.', '', mappingIdInter$geneInter))}

  mapping = merge(mappingIdInter, mappingProbeTmp, by = 'geneInter', sort = FALSE)
  mapping = mapping[, c('probe_set', 'gene_id')]
  return(mapping)}


getEntrezEnsemblMapping = function(organism) {
  pkgName = glue('org.{organism}.eg.db')
  if (!requireNamespace(pkgName, quietly = TRUE)) {
    BiocManager::install(pkgName)}

  mapTmp1 = eval(parse(text = glue('{pkgName}::org.{organism}.egENSEMBL')))
  mapTmp2 = AnnotationDbi::mappedkeys(mapTmp1)
  mapTmp3 = as.list(mapTmp1[mapTmp2])

  m = data.table(entrez = rep.int(names(mapTmp3), lengths(mapTmp3)),
                 ensembl = unlist(mapTmp3, use.names = FALSE))
  return(m)}


getProbeGeneMapping = function(featureDt, platformDt, geneIdType) {
  . = probe_set = gene_id = NULL

  if (platformDt$mappingFunction == 'Direct') {
    mapping = getProbeGeneMappingDirect(featureDt, platformDt$geneColname)
  } else {
    if (!is.na(platformDt$splitColumn)) {
      set(featureDt, j = platformDt$interName,
          value = gsub('\\.\\d+$', '', featureDt[[platformDt$splitColumn]]))}
    mapping = getProbeGeneMappingAnno(
      featureDt, platformDt$dbName, platformDt$interName)}

  for (col in c('probe_set', 'gene_id')) {
    set(mapping, j = col, value = as.character(mapping[[col]]))}

  if (geneIdType == 'ensembl') {
    m = getEntrezEnsemblMapping(platformDt$organism)
    mapping = merge(mapping[, .(probe_set, entrez = gene_id)], m, by = 'entrez')
    set(mapping, j = 'entrez', value = NULL)
    setnames(mapping, 'ensembl', 'gene_id')
    data.table::setorderv(mapping, 'gene_id')}

  return(mapping)}


getLogTransformedEmat = function(emat) {
  # adapted from GEO2R
  qx = stats::quantile(emat, c(0, 0.25, 0.99, 1.0), na.rm = TRUE)
  needsLog = (qx[3L] > 100) || (qx[4L] - qx[1L] > 50 && qx[2L] > 0)
  if (needsLog) {
    idx = emat > 0
    minVal = min(emat[idx], na.rm = TRUE)
    emat[!idx & !is.na(emat)] = minVal
    emat = log2(emat)}
  return(emat)}


getEmatGene = function(ematProbe, mapping) {
  .SD = NULL
  idx = rowSums(is.na(ematProbe)) == 0
  dProbe = data.table(ematProbe[idx, , drop = FALSE], keep.rownames = 'probe_set')
  dProbe = merge(mapping, dProbe, by = 'probe_set', sort = FALSE)
  dGene = dProbe[, lapply(.SD, stats::median),
                 by = 'gene_id', .SDcols = !'probe_set']
  ematGene = as.matrix(dGene[, !'gene_id'])
  rownames(ematGene) = dGene$gene_id
  return(ematGene)}
