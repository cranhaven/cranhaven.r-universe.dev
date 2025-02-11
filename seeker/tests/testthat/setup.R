library('data.table')
library('qs')
library('foreach')

withr::local_options(list(timeout = 300), .local_envir = teardown_env())
onCran = !identical(Sys.getenv('NOT_CRAN'), 'true')

snapshot = function(xObs, path) {
  if (file.exists(path)) {
    xExp = qread(path)
  } else {
    qsave(xObs, path)
    xExp = xObs}
  return(xExp)}

getCommandsCheck = function(params, cmds = checkDefaultCommands()) {
  result = foreach(i = seq_len(nrow(cmds)), .combine = rbind) %do% {
    cmdRow = cmds[i]
    # cmd = if (cmdRow$command == 'ascp') {
    #   params$fetch$ascpCmd
    # } else if (cmdRow$command == 'trim_galore') {
    cmd = if (cmdRow$command == 'trim_galore') {
      params$trimgalore$cmd
    } else if (cmdRow$command %in% names(params)) {
      params[[cmdRow$command]]$cmd
    } else {
      NULL}

    cmdExists = !((is.null(cmd) && is.na(cmdRow$path)) ||
                  (!is.null(cmd) && is.na(validateCommand(cmd))))
    data.table(filename = cmdRow$command, exists = cmdExists)}
  return(result)}

registerDoSEQ()
dataDir = 'data'
params = yaml::read_yaml(file.path(dataDir, 'GSE143524.yml'))
os = Sys.info()['sysname']

if (os == 'Darwin') {
  params$salmon$indexDir = gsub('/home/', '/Users/', params$salmon$indexDir)}
if (Sys.info()['user'] != 'runner') {
  params$salmon$indexDir = gsub(
    '/runner/', paste0('/', Sys.info()['user'], '/'), params$salmon$indexDir)}

commandsDt = rbind(
  getCommandsCheck(params),
  data.table(filename = 'salmon_index', exists = file.exists(params$salmon$indexDir)))
anyMissing = any(!commandsDt[['exists']])

params$fetch$run = FALSE
parentDir = file.path(dataDir, 'staging')
dir.create(parentDir)
withr::local_file(parentDir, .local_envir = teardown_env())
file.copy(file.path(dataDir, 'GSE143524'), parentDir, recursive = TRUE)

metadata = qread(file.path(dataDir, 'metadata.qs'))

outputDir = file.path(parentDir, 'GSE143524')
fetchDir = file.path(outputDir, 'fetch_output')
# remoteColname = 'fastq_aspera'
remoteColname = 'run_accession'
fetchColname = 'fastq_fetched'
trimDir = file.path(outputDir, 'trimgalore_output')
trimColname = 'fastq_trimmed'
fastqcDir = file.path(outputDir, 'fastqc_output')
fileColname = if (params$trimgalore$run) trimColname else fetchColname
salmonDir = file.path(outputDir, 'salmon_output')
sampleColname = 'sample_accession'
fileColname = 'fastq_fetched'
multiqcDir = file.path(outputDir, 'multiqc_output')
