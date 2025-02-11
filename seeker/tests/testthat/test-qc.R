test_that('fastqc', {
  skip_on_cran()
  skip_if(!commandsDt[filename == 'fastqc']$exists, 'Missing fastqc command, skipping.')
  skip_on_os('windows', arch = NULL)
  step = 'fastqc'
  paramsNow = params[[step]]

  paramsNow[c('run', 'keep')] = NULL
  result = do.call(fastqc, c(
    list(filepaths = metadata[[fileColname]], outputDir = fastqcDir),
    paramsNow))

  fastqcFilesObs = list.files(fastqcDir, recursive = TRUE)
  fastqcFilesExp = snapshot(fastqcFilesObs, file.path(dataDir, 'fastqc_output.qs'))

  expect_equal(fastqcFilesObs, fastqcFilesExp)

})

test_that('trimgalore', {
  skip_on_cran()
  skip_if(!commandsDt[filename == 'trim_galore']$exists, 'Missing trim_galore command, skipping.')
  skip_on_os('windows', arch = NULL)
  step = 'trimgalore'
  paramsNow = params[[step]]

  paramsNow[c('run', 'keep')] = NULL

  result = do.call(trimgalore, c(
    list(filepaths = metadata[[fetchColname]], outputDir = trimDir),
    paramsNow))

  trimFilesObs = list.files(trimDir, recursive = TRUE)
  trimFilesExp = snapshot(trimFilesObs, file.path(dataDir, 'trimgalore_output.qs'))

  expect_equal(trimFilesObs, trimFilesExp)
})

test_that('multiqc', {
  skip_on_cran()
  skip_if(!commandsDt[filename == 'multiqc']$exists, 'Missing multiqc command, skipping.')
  skip_on_os('windows', arch = NULL)
  step = 'multiqc'

  paramsNow = params[[step]]
  paramsNow$run = NULL

  result = do.call(multiqc, c(
    list(parentDir = outputDir, outputDir = multiqcDir), paramsNow))

  multiqcFilesObs = list.files(multiqcDir, recursive = TRUE)
  multiqcFilesExp = snapshot(multiqcFilesObs, file.path(dataDir, 'multiqc_output.qs'))

  expect_equal(multiqcFilesObs, multiqcFilesExp)
})
