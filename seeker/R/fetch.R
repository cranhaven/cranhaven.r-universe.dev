#' Fetch metadata for a genomic study
#'
#' This function can use the API of the European Nucleotide Archive
#' (recommended) or the Sequence Read Archive.
#'
#' @param bioproject String indicating bioproject accession.
#' @param host String indicating from where to fetch the metadata.
#' @param fields Character vector indicating which fields to fetch, if `host`
#'   is "ena".
#' @param file String indicating output file path, if not `NULL`.
#'
#' @return A `data.table`.
#'
#' @seealso [seeker()], [fetch()]
#'
#' @export
fetchMetadata = function(
  bioproject, host = c('ena', 'sra'),
  fields = c(
    'study_accession', 'sample_accession', 'secondary_sample_accession',
    'sample_alias', 'sample_title', 'experiment_accession', 'run_accession',
    'fastq_md5', 'fastq_ftp', 'fastq_aspera'), file = NULL) {

  assertString(bioproject)
  host = match.arg(host)
  assertCharacter(fields, any.missing = FALSE)
  assertString(file, null.ok = TRUE)
  if (!is.null(file)) assertPathForOutput(file, overwrite = TRUE)

  if (host == 'ena') {
    url = paste0(
      'https://www.ebi.ac.uk/ena/portal/api/filereport?accession=',
      bioproject, '&result=read_run&format=tsv&download=true&fields=',
      paste0(fields, collapse = ','))
    sep = '\t'
  } else {
    url = paste0(
      'http://trace.ncbi.nlm.nih.gov/Traces/sra/sra.cgi',
      '?save=efetch&db=sra&rettype=runinfo&term=', bioproject)
    sep = ','}

  raw = tryCatch(curl::curl_fetch_memory(url), error = \(e) e)
  metadata = if (inherits(raw, 'error')) {
    message('Unable to fetch metadata. Host may be temporarily unavailable.')
    return(data.table())
  }

  metadata = fread(text = rawToChar(raw$content), sep = sep, na.strings = '')
  data.table::setorder(metadata)
  if (!is.null(file)) fwrite(metadata, file)
  return(metadata)}


#' Fetch files
#'
#' This function uses the NCBI SRA Toolkit via [system2()] to download files
#' from SRA and convert them to fastq.gz. To process files in parallel, register
#' a parallel backend, e.g., using [doParallel::registerDoParallel()]. Beware
#' that intermediate files created by fasterq-dump are uncompressed and could
#' require hundreds of gigabytes if files are processed in parallel.
#'
#' @param accessions Character vector of SRA run accessions.
#' @param outputDir String indicating the local directory in which to save the
#'   files. Will be created if it doesn't exist.
#' @param overwrite Logical indicating whether to overwrite files that already
#'   exist in `outputDir`.
#' @param keepSra Logical indicating whether to keep the ".sra" files.
#' @param prefetchCmd String indicating command for prefetch, which downloads
#'   ".sra" files.
#' @param prefetchArgs Character vector indicating arguments to pass to
#'   prefetch.
#' @param fasterqdumpCmd String indicating command for fasterq-dump, which
#'   uses ".sra" files to create ".fastq" files.
#' @param fasterqdumpArgs Character vector indicating arguments to pass to
#'   fasterq-dump.
#' @param pigzCmd String indicating command for pigz, which converts ".fastq"
#'   files to ".fastq.gz" files.
#' @param pigzArgs Character vector indicating arguments to pass to pigz.
#'
#' @return A list. As the function runs, it updates a tab-delimited log file in
#'   `outputDir` called "progress.tsv".
#'
#' @seealso [seeker()], [fetchMetadata()]
#'
#' @export
fetch = function(
  accessions, outputDir, overwrite = FALSE, keepSra = FALSE,
  prefetchCmd = 'prefetch', prefetchArgs = NULL,
  fasterqdumpCmd = 'fasterq-dump', fasterqdumpArgs = NULL,
  pigzCmd = 'pigz', pigzArgs = NULL) {

  acc = i = NULL
  assertCharacter(accessions, any.missing = FALSE)
  assertString(outputDir)
  assertPathForOutput(outputDir, overwrite = TRUE)
  assertFlag(overwrite)
  assertFlag(keepSra)
  assertString(prefetchCmd)
  assertCharacter(prefetchArgs, any.missing = FALSE, null.ok = TRUE)
  assertString(fasterqdumpCmd)
  assertCharacter(fasterqdumpArgs, any.missing = FALSE, null.ok = TRUE)
  assertString(pigzCmd)
  assertCharacter(pigzArgs, any.missing = FALSE, null.ok = TRUE)

  reg = checkParallel()
  doOp = if (reg) `%dopar%` else `%do%`

  if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE)

  logPath = getLogPath(outputDir)
  writeLogFile(logPath, n = length(accessions))

  feo = foreach(
    acc = accessions, i = seq_len(length(accessions)),
    .options.future = list(scheduling = Inf))

  result = doOp(feo, {
    pat = '^{acc}(_1|_2|)\\.fastq'
    paths = dir(outputDir, glue(pat, '\\.gz$'), full.names = TRUE)

    if (length(paths) > 0 && isFALSE(overwrite)) {
      r = 0
    } else {
      Sys.sleep(stats::runif(1L, 0, foreach::getDoParWorkers() / 4))
      # above line to avoid possible rate limiting if dopar

      args = c(prefetchArgs, '-O', safe(outputDir), acc)
      r = system3(path.expand(prefetchCmd), args)

      args = c(fasterqdumpArgs, '-e', 1, '-O', safe(outputDir), '-f', acc)
      r = system3(path.expand(fasterqdumpCmd), args)

      args = c(
        pigzArgs, '-p', 1,
        '-f', safe(dir(outputDir, glue(pat, '$'), full.names = TRUE)))
      r = system3(path.expand(pigzCmd), args)
      paths = dir(outputDir, glue(pat, '\\.gz$'), full.names = TRUE)

      if (isFALSE(keepSra)) {
        unlink(file.path(outputDir, acc), recursive = TRUE)}}

    writeLogFile(logPath, acc, i, r)
    list(paths, r)})

  writeLogFile(logPath, n = -length(accessions))
  d = list(localFilepaths = getFileVec(lapply(result, `[[`, 1L)),
           statuses = sapply(result, `[[`, 2L))
  return(d)}
