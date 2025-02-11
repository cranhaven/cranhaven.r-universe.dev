#' Run Salmon
#'
#' This function calls
#' [salmon](https://combine-lab.github.io/salmon/)
#' using [system2()]. To run in parallel, register a parallel backend, e.g.,
#' using [doParallel::registerDoParallel()].
#'
#' @param filepaths Paths to fastq files. For single-end reads, each element
#'   should be a single filepath. For paired-end reads, each element should be
#'   two filepaths separated by ";".
#' @param samples Corresponding sample names for fastq files.
#' @param indexDir Directory that contains salmon index.
#' @param outputDir Directory in which to store output. Will be created if it
#'   doesn't exist.
#' @param cmd Name or path of the command-line interface.
#' @param args Additional arguments to pass to the command-line interface.
#' @param compress Logical indicating whether to gzip the quantification file
#'   (quant.sf) from salmon. Does not affect downstream analysis.
#'
#' @return A vector of exit codes, invisibly.
#'
#' @seealso [seeker()], [getSalmonMetadata()]
#'
#' @export
salmon = function(
  filepaths, samples, indexDir, outputDir = 'salmon_output', cmd = 'salmon',
  args = c('-l A -q --seqBias --gcBias --no-version-check -p',
           foreach::getDoParWorkers()), compress = TRUE) {

  i = NULL
  assertCharacter(filepaths, any.missing = FALSE)
  assertCharacter(samples, any.missing = FALSE)
  assertTRUE(length(filepaths) == length(samples))

  filepaths = getFileList(filepaths)

  assertFileExists(unlist(filepaths))
  assertString(indexDir)
  assertDirectoryExists(indexDir)
  assertString(outputDir)
  assertPathForOutput(outputDir, overwrite = TRUE)
  assertString(cmd)
  assertCharacter(args, any.missing = FALSE, null.ok = TRUE)
  assertFlag(compress)
  checkParallel()

  if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE)
  argsBase = c('quant', args, '-i', safe(indexDir))

  samplesUnique = sort(unique(samples))
  logPath = getLogPath(outputDir)
  writeLogFile(logPath, n = length(samplesUnique))

  feo = foreach(
    i = seq_len(length(samplesUnique)), .combine = c,
    .options.future = list(scheduling = Inf))

  result = feo %do% {
    samp = samplesUnique[i]
    f = filepaths[samp == samples]
    args1 = c(argsBase, '-o', safe(file.path(outputDir, samp)))

    if (length(f[[1L]]) > 1) {
      f1 = sapply(f, function(f) f[1L])
      f2 = sapply(f, function(f) f[2L])
      args2 = c('-1', safe(f1), '-2', safe(f2))
    } else {
      args2 = c('-r', safe(unlist(f)))}

    r = system3(path.expand(cmd), c(args1, args2))

    auxPath = file.path(outputDir, samp, 'aux_info', 'ambig_info.tsv')
    if (file.exists(auxPath)) R.utils::gzip(auxPath, overwrite = TRUE)

    if (compress) {
      R.utils::gzip(file.path(outputDir, samp, 'quant.sf'), overwrite = TRUE)}
    writeLogFile(logPath, samp, i, r)
    r}

  writeLogFile(logPath, n = -length(samplesUnique))
  invisible(result)}


#' Aggregrate metadata from salmon quantifications
#'
#' @param inputDir Directory that contains output from salmon.
#' @param outputDir Directory in which to save the result, a file named
#'   "salmon_meta_info.csv". If `NULL`, no file is saved.
#'
#' @return A data.table, invisibly.
#'
#'#' @seealso [seeker()], [salmon()]
#'
#' @export
getSalmonMetadata = function(inputDir, outputDir = 'data') {

  outputFilename = 'salmon_meta_info.csv'

  assertString(inputDir)
  assertDirectoryExists(inputDir)
  assertString(outputDir, null.ok = TRUE)
  if (!is.null(outputDir)) {
    assertPathForOutput(outputDir, overwrite = TRUE)
    if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE)}

  filepaths = list.files(
    inputDir, 'meta_info.json', full.names = TRUE, recursive = TRUE)
  sampleNames = sapply(
    strsplit(filepaths, .Platform$file.sep), function(x) x[length(x) - 2L])

  metaList = lapply(filepaths, jsonlite::fromJSON)
  names(metaList) = sampleNames

  fieldNames = c('eq_class_properties', 'length_classes')
  metaSpecial = list()
  for (fieldName in fieldNames) {
    fieldSpecial = lapply(metaList, function(x) x[[fieldName]])
    if (!all(lapply(fieldSpecial, length) == 0)) {
      metaSpecial[[fieldName]] = lapply(metaList, function(x) x[[fieldName]])}}

  metaList = lapply(metaList, function(x) {
    idx = !(names(x) %in% c('quant_errors', fieldNames))
    x[idx]})
  metadata = data.table::rbindlist(metaList, fill = TRUE, idcol = 'sample_name')

  for (fieldName in names(metaSpecial)) {
    data.table::set(metadata, j = fieldName, value = metaSpecial[[fieldName]])}

  if (!is.null(outputDir)) {
    fwrite(metadata, file.path(outputDir, outputFilename))}
  invisible(metadata)}
