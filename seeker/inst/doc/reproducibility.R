## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = '#>')

## ----eval = FALSE-------------------------------------------------------------
#  urlBase = 'https://raw.githubusercontent.com/hugheylab/seeker/master/inst/extdata/'
#  for (filename in c('PRJNA600892.yml', 'run_seeker.R', 'run_seeker.sh')) {
#    download.file(paste0(urlBase, filename), filename)}

## ----code = readLines(system.file('extdata', 'PRJNA600892.yml', package = 'seeker')), eval = FALSE----
#  study: 'PRJNA600892' # [string]
#  metadata:
#    run: TRUE # [logical]
#    bioproject: 'PRJNA600892' # [string]
#    include:
#      # [named list or NULL]
#      colname: 'run_accession' # [string]
#      values: ['SRR10876945', 'SRR10876946'] # [vector]
#    # exclude # [named list or NULL]
#      # colname # [string]
#      # values # [vector]
#  fetch:
#    run: TRUE # [logical]
#    # keep # [logical or NULL]
#    # overwrite # [logical or NULL]
#    # keepSra # [logical or NULL]
#    # prefetchCmd # [string or NULL]
#    # prefetchArgs # [character vector or NULL]
#    # fasterqdumpCmd # [string or NULL]
#    # fasterqdumpArgs # [character vector or NULL]
#    # pigzCmd # [string or NULL]
#    # pigzArgs # [character vector or NULL]
#  trimgalore:
#    run: TRUE # [logical]
#    # keep # [logical or NULL]
#    # cmd # [string or NULL]
#    # args # [character vector or NULL]
#    # pigzCmd # [string or NULL]
#  fastqc:
#    run: TRUE # [logical]
#    # keep # [logical or NULL]
#    # cmd # [string or NULL]
#    # args # [character vector or NULL]
#  salmon:
#    run: TRUE # [logical]
#    indexDir: '~/refgenie_genomes/alias/mm10/salmon_partial_sa_index/default' # [string]
#    # sampleColname # [string or NULL]
#    # keep # [logical or NULL]
#    # cmd # [string or NULL]
#    # args # [character vector or NULL]
#  multiqc:
#    run: TRUE # [logical]
#    # cmd # [string or NULL]
#    # args # [character vector or NULL]
#  tximport:
#    run: TRUE # [logical]
#    tx2gene:
#      # [named list or NULL]
#      organism: 'mmusculus' # [string]
#      # version # [number or NULL]
#      # filename # [string or NULL]
#    countsFromAbundance: 'lengthScaledTPM' # [string]
#    # ignoreTxVersion # [logical or NULL]

## ----code = readLines(system.file('extdata', 'run_seeker.R', package = 'seeker')), eval = FALSE----
#  doParallel::registerDoParallel()
#  
#  cArgs = commandArgs(TRUE)
#  yamlPath = cArgs[1L]
#  parentDir = cArgs[2L]
#  
#  params = yaml::read_yaml(yamlPath)
#  seeker::seeker(params, parentDir)

## ----code = readLines(system.file('extdata', 'run_seeker.sh', package = 'seeker')), eval = FALSE----
#  #!/bin/sh
#  
#  docker run \
#    --mount type=bind,src=`pwd`,dst=/home/rstudio/projects \
#    -w /home/rstudio/projects \
#    --rm \
#    ghcr.io/hugheylab/socker \
#    bash -c \
#      "source /home/rstudio/miniconda3/etc/profile.d/conda.sh \
#        && conda activate seeker \
#        && refgenie pull mm10/salmon_partial_sa_index \
#        && Rscript run_seeker.R PRJNA600892.yml ." \
#    &> PRJNA600892_progress.log

## ----eval = FALSE-------------------------------------------------------------
#  urlBase = 'https://raw.githubusercontent.com/hugheylab/seeker/master/inst/extdata/'
#  for (filename in c('GSE25585.yml', 'run_seeker_array.R', 'run_seeker_array.sh')) {
#    download.file(paste0(urlBase, filename), filename)}

## ----code = readLines(system.file('extdata', 'GSE25585.yml', package = 'seeker')), eval = FALSE----
#  study: 'GSE25585'
#  geneIdType: 'entrez'

## ----code = readLines(system.file('extdata', 'run_seeker_array.R', package = 'seeker')), eval = FALSE----
#  cArgs = commandArgs(TRUE)
#  
#  params = yaml::read_yaml(cArgs[1L])
#  parentDir = cArgs[2L]
#  
#  seeker::seekerArray(
#    study = params$study, geneIdType = params$geneIdType,
#    platform = params$platform, parentDir)

## ----code = readLines(system.file('extdata', 'run_seeker_array.sh', package = 'seeker')), eval = FALSE----
#  #!/bin/sh
#  
#  docker run \
#    --mount type=bind,src=`pwd`,dst=/home/rstudio/projects \
#    -w /home/rstudio/projects \
#    --rm \
#    ghcr.io/hugheylab/socker \
#    bash -c "Rscript run_seeker_array.R GSE25585.yml ." \
#    &> GSE25585_progress.log

