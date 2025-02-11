getLogPath = function(outputDir, filename = 'progress.tsv') {
  return(file.path(outputDir, filename))}


writeLogFile = function(path, task, idx, status, n = NULL) {
  d = data.table(datetime = Sys.time())
  append = TRUE
  if (is.null(n)) {
    d = data.table(d, task = task, idx = idx, status = status)
  } else {
    if (n > 0) {
      x = 'started'
      append = FALSE
    } else {
      x = 'finished'}
    d = data.table(d, task = glue('{x} {abs(n)} task(s)'), idx = 0, status = 0)}
  fwrite(d, path, sep = '\t', append = append, logical01 = TRUE)
  invisible(d)}


getFileList = function(fileVec) {
  if (is.list(fileVec)) return(fileVec)
  return(strsplit(fileVec, ';'))}


getFileVec = function(fileList) {
  return(sapply(fileList, function(f) paste0(f, collapse = ';')))}


# #' Get ascp command
# #'
# #' This function returns the default path to the aspera ascp command-line
# #' interface, based on the operating system. Windows is not supported.
# #'
# #' @return A string.
# #'
# #' @seealso [getAscpArgs()], [fetch()]
# #'
# #' @export
# getAscpCmd = function() {
#   os = Sys.info()[['sysname']]
#   cmd = if (os == 'Linux') {
#     '~/.aspera/connect/bin/ascp'
#   } else if (os == 'Darwin') {
#     appDir = '~/Applications/Aspera Connect.app/Contents/Resources'
#     if (!dir.exists(appDir)) appDir = gsub('~', '', appDir)
#     file.path(appDir, 'ascp')
#   } else {
#     NULL}
#   return(cmd)}


# #' Get ascp arguments
# #'
# #' This function returns the default arguments to pass to the aspera ascp
# #' command-line interface, based on the operating system. Windows is not
# #' supported.
# #'
# #' @return A character vector.
# #'
# #' @seealso [getAscpCmd()], [fetch()]
# #'
# #' @export
# getAscpArgs = function() {
#   a = c('-QT -l 300m -P33001 -i')
#   f = 'asperaweb_id_dsa.openssh'
#   os = Sys.info()[['sysname']]
#   rgs = if (os == 'Linux') {
#     c(a, safe(file.path('~/.aspera/connect/etc', f)))
#   } else if (os == 'Darwin') {
#     appDir = '~/Applications/Aspera Connect.app/Contents/Resources'
#     if (!dir.exists(appDir)) appDir = gsub('~', '', appDir)
#     c(a, safe(file.path(appDir, f)))
#   } else {
#     NULL}
#   return(rgs)}


getTrimmedFilenames = function(x) {
  # for one read or one pair of reads at a time
  # https://github.com/FelixKrueger/TrimGalore/blob/master/trim_galore#L574
  # https://github.com/FelixKrueger/TrimGalore/blob/master/trim_galore#L866
  # https://github.com/FelixKrueger/TrimGalore/blob/master/trim_galore#L1744

  y = x
  for (i in seq_len(length(y))) {
    pat = if (grepl('\\.fastq$', x[i])) {
      '\\.fastq$'
    } else if (grepl('\\.fastq\\.gz$', x[i])) {
      '\\.fastq\\.gz$'
    } else if (grepl('\\.fq$', x[i])) {
      '\\.fq$'
    } else if (grepl('\\.fq\\.gz$', x[i])) {
      '\\.fq\\.gz$'
    } else {
      '$'}
    # trim_galore stopped being able to gzip files if the paths contain spaces
    # y[i] = gsub(pat, '_trimmed.fq.gz', x[i])
    y[i] = gsub(pat, '_trimmed.fq', x[i])

    if (length(y) > 1) {
      y[i] = gsub('trimmed\\.fq', glue('val_{i}.fq'), y[i])}}
  # y[i] = gsub('trimmed\\.fq\\.gz', glue('val_{i}.fq.gz'), y[i])}}

  return(y)}


getFastqcFilenames = function(fastqFilepaths) {
  x = basename(unlist(getFileList(fastqFilepaths)))
  y = gsub('\\.(f(ast)?q(\\.gz)?)$', '', x, ignore.case = TRUE)
  z = c(paste0(y, '_fastqc.html'), paste0(y, '_fastqc.zip'))
  return(z)}


getRCondaInfo = function(outputDir = '.') {
  sessioninfo::session_info(
    info = 'auto', to_file = file.path(outputDir, 'session.log'))

  mc = getOption('seeker.miniconda')
  if (is.null(mc)) {
    envName = 'seeker'
    condaPre = '~/miniconda3'
  } else if (basename(dirname(mc)) == 'envs') {
    envName = basename(mc)
    condaPre = dirname(dirname(mc))
  } else {
    envName = 'base'
    condaPre = mc}
  condaCmd = file.path(condaPre, 'condabin', 'conda')

  args = c('env', 'export', '--name', safe(envName), '>',
           safe(file.path(outputDir, 'environment.yml')))
  system2(path.expand(condaCmd), args)
  invisible()}


system3 = function(...) {
  mc = getOption('seeker.miniconda', '~/miniconda3/envs/seeker')
  p = path.expand(file.path(mc, c('bin/scripts', 'bin')))
  withr::local_path(p)
  system2(...)}


safe = function(x) {
  y = glue("'{path.expand(x)}'")
  return(y)}


validateCommand = function(cmd) {
  # if cmd doesn't exist, system2('command', ...) seems to
  # give warning on mac and error on linux
  suppressWarnings({
    path = tryCatch({system3('command', c('-v', safe(cmd)), stdout = TRUE)},
                    error = function(e) NA_character_)})
  if (length(path) == 0) path = NA_character_
  return(path)}


getCommandVersion = function(cmd, idx) {
  ver = system3(path.expand(cmd), '--version', stdout = TRUE)[idx]
  ver = trimws(gsub('\\"', '', ver))
  return(ver)}


#' Check for presence of command-line interfaces
#'
#' This function checks whether the command-line tools used by seeker are
#' accessible in the expected places.
#'
#' @param keepIdx Logical indicating whether to keep the `idx` column of the
#'   resulting data.table. For internal use only.
#'
#' @return A data.table with columns for command, path, and version.
#'
#' @seealso [installSysDeps()]
#'
#' @export
checkDefaultCommands = function(keepIdx = FALSE) {
  d = data.table(
    # cmd = c('ascp', 'wget', 'fastqc', 'fastq_screen', 'trim_galore', 'cutadapt',
    #         'multiqc', 'salmon'),
    # idx = c(2, 1, 1, 1, 4, 1, 1, 1))
    cmd = c('prefetch', 'fasterq-dump', 'pigz', 'fastqc', 'fastq_screen',
            'trim_galore', 'cutadapt', 'multiqc', 'salmon'),
    idx = c(2, 2, 1, 1, 1, 4, 1, 1, 1))

  i = NULL
  r = foreach(i = seq_len(nrow(d)), .combine = rbind) %do% {
    # cmd = if (d$cmd[i] == 'ascp') getAscpCmd() else d$cmd[i]
    cmd = d$cmd[i]
    path = validateCommand(cmd)
    version = if (is.na(path)) NA_character_ else
      getCommandVersion(cmd, d$idx[i])
    rNow = data.table(command = d$cmd[i], path = path, version = version)
    if (isTRUE(keepIdx)) set(rNow, j = 'idx', value = d$idx[i])
    rNow}

  return(r)}


checkCommand = function(cmd, cmdName, defaultPath) {
  if (is.null(cmd)) {
    if (is.na(defaultPath)) {
      return(glue(
        '{cmdName} is not available at the default location.',
        ' Have you run `installSysDeps`?'))}
  } else {
    path = validateCommand(cmd)
    if (is.na(path)) {
      return(glue("'{cmd}' is not a valid command."))}}
  return(TRUE)}


assertCommand = checkmate::makeAssertionFunction(checkCommand)


checkParallel = function() {
  reg = foreach::getDoParRegistered()
  if (!reg) {
    w = paste('No parallel backend registered.',
              'Running on a single core, this could take a while.')
    warning(w, call. = FALSE)}
  invisible(reg)}
