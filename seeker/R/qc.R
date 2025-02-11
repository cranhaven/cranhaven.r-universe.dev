#' Run FastQC
#'
#' This function calls
#' [fastqc](https://www.bioinformatics.babraham.ac.uk/projects/fastqc/) using
#' [system2()]. To run in parallel, register a parallel backend, e.g., using
#' [doParallel::registerDoParallel()].
#'
#' @param filepaths Paths to fastq files. For single-end reads, each element
#'   should be a single filepath. For paired-end reads, each element can be two
#'   filepaths separated by ";".
#' @param outputDir Directory in which to store output. Will be created if it
#'   doesn't exist.
#' @param cmd Name or path of the command-line interface.
#' @param args Additional arguments to pass to the command-line interface.
#'
#' @return A vector of exit codes, invisibly.
#'
#' @seealso [seeker()]
#'
#' @export
fastqc = function(
  filepaths, outputDir = 'fastqc_output', cmd = 'fastqc', args = NULL) {

  f = i = NULL
  assertCharacter(filepaths, any.missing = FALSE)
  filepaths = getFileList(filepaths)
  fs = unlist(filepaths)

  assertFileExists(fs)
  assertString(outputDir)
  assertPathForOutput(outputDir, overwrite = TRUE)
  assertString(cmd)
  assertCharacter(args, any.missing = FALSE, null.ok = TRUE)

  reg = checkParallel()
  doOp = if (reg) `%dopar%` else `%do%`

  if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE)
  logPath = getLogPath(outputDir)
  writeLogFile(logPath, n = length(fs))

  feo = foreach(
    f = fs, i = seq_len(length(fs)), .combine = c,
    .options.future = list(scheduling = Inf))

  result = doOp(feo, {
    argsNow = c(args, '-o', safe(outputDir), safe(f))
    r = system3(path.expand(cmd), argsNow)
    writeLogFile(logPath, f, i, r)
    r})

  writeLogFile(logPath, n = -length(fs))
  invisible(result)}


#' Run FastQ Screen
#'
#' This function calls
#' [fastq_screen](https://www.bioinformatics.babraham.ac.uk/projects/fastq_screen/)
#' using [system2()]. To run in parallel, register a parallel backend, e.g.,
#' using [doParallel::registerDoParallel()].
#'
#' @param filepaths Paths to fastq files. For single-end reads, each element
#'   should be a single filepath. For paired-end reads, each element can be two
#'   filepaths separated by ";".
#' @param outputDir Directory in which to store output. Will be created if it
#'   doesn't exist.
#' @param cmd Name or path of the command-line interface.
#' @param args Additional arguments to pass to the command-line interface.
#'
#' @return A vector of exit codes, invisibly.
#'
#' @seealso [seeker()]
#'
#' @export
fastqscreen = function(
  filepaths, outputDir = 'fastqscreen_output', cmd = 'fastq_screen',
  args = c('--threads', foreach::getDoParWorkers(), '--conf',
           '~/FastQ_Screen_Genomes/fastq_screen.conf')) {

  f = i = NULL
  assertCharacter(filepaths, any.missing = FALSE)
  filepaths = getFileList(filepaths)
  fs = unlist(filepaths)

  assertFileExists(fs)
  assertString(outputDir)
  assertPathForOutput(outputDir, overwrite = TRUE)
  assertString(cmd)
  assertCharacter(args, any.missing = FALSE, null.ok = TRUE)
  checkParallel()

  if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE)
  logPath = getLogPath(outputDir)
  writeLogFile(logPath, n = length(fs))

  result = foreach(f = fs, i = seq_len(length(fs)), .combine = c) %do% {
    argsNow = c(args, '--outdir', safe(outputDir), safe(f))
    r = system3(path.expand(cmd), argsNow)
    writeLogFile(logPath, f, i, r)
    r}

  writeLogFile(logPath, n = -length(fs))
  invisible(result)}


#' Run Trim Galore!
#'
#' This function calls
#' [trim_galore](https://www.bioinformatics.babraham.ac.uk/projects/trim_galore/)
#' using [system2()], and is only designed to handle standard adapter/quality
#' trimming. To run in parallel, register a parallel backend, e.g., using
#' [doParallel::registerDoParallel()].
#'
#' @param filepaths Paths to fastq files. For single-end reads, each element
#'   should be a single filepath. For paired-end reads, each element should be
#'   two filepaths separated by ";".
#' @param outputDir Directory in which to store output. Will be created if it
#'   doesn't exist.
#' @param cmd Name or path of the command-line interface.
#' @param args Additional arguments to pass to the command-line interface.
#'   Output files will always be compressed. Arguments "--gzip", "--cores",
#'   "-j", and "--basename" are not allowed. Arguments "-o" and "--paired"
#'   should not be specified here.
#' @param pigzCmd String for pigz command, which will gzip the output files.
#'
#' @return A vector of exit codes, invisibly.
#'
#' @seealso [seeker()]
#'
#' @export
trimgalore = function(
  filepaths, outputDir = 'trimgalore_output', cmd = 'trim_galore', args = NULL,
  pigzCmd = 'pigz') {

  f = i = NULL
  assertCharacter(filepaths, any.missing = FALSE)
  filepaths = getFileList(filepaths)

  assertFileExists(unlist(filepaths))
  assertString(outputDir)
  assertPathForOutput(outputDir, overwrite = TRUE)
  assertString(cmd)
  assertCharacter(args, any.missing = FALSE, null.ok = TRUE)

  if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE)
  logPath = getLogPath(outputDir)
  writeLogFile(logPath, n = length(filepaths))

  # trim_galore stopped being able to gzip files if the paths contain spaces
  # so now have to pigz afterwards
  args = paste(c(args, '--dont_gzip'), collapse = ' ')
  if (grepl('(-j|--cores|--basename|--gzip|-o|--paired)', args)) {
    stop('args contains unallowed arguments.')}

  reg = checkParallel()
  doOp = if (reg) `%dopar%` else `%do%`

  feo = foreach(
    f = filepaths, i = seq_len(length(filepaths)), .combine = rbind,
    .options.future = list(scheduling = Inf))

  result = doOp(feo, {
    argsNow = c(
      args, '-o', safe(outputDir), if (length(f) > 1) '--paired', safe(f))
    r = system3(path.expand(cmd), argsNow)
    writeLogFile(logPath, paste(f, collapse = ';'), i, r)

    paths = file.path(outputDir, basename(getTrimmedFilenames(f)))
    . = system3(path.expand(pigzCmd), c('-p', 1, '-f', safe(paths)))
    data.table(
      fastq_trimmed = paste(paste0(paths, '.gz'), collapse = ';'), status = r)})

  writeLogFile(logPath, n = -length(filepaths))
  invisible(result)}


#' Run MultiQC
#'
#' This function calls [multiqc](https://multiqc.info/) using [system2()].
#'
#' @param parentDir Directory that contains output to be aggregated.
#' @param outputDir Directory in which to store output. Will be created if it
#'   doesn't exist.
#' @param cmd Name or path of the command-line interface.
#' @param args Additional arguments to pass to the command-line interface.
#'
#' @return An exit code, invisibly.
#'
#' @seealso [seeker()]
#'
#' @export
multiqc = function(
  parentDir = '.', outputDir = 'multiqc_output', cmd = 'multiqc', args = NULL) {

  assertString(parentDir)
  assertDirectoryExists(parentDir)
  assertString(outputDir)
  assertPathForOutput(outputDir, overwrite = TRUE)
  assertString(cmd)
  assertCharacter(args, any.missing = FALSE, null.ok = TRUE)

  argsNow = c(args, '-o', safe(outputDir), safe(parentDir))
  r = system3(path.expand(cmd), argsNow)
  invisible(r)}
