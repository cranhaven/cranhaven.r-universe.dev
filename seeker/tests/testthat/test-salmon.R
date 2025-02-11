test_that('salmon and getSalmonMetadata', {
  skip_on_cran()
  skip_if(
    (!commandsDt[filename == 'salmon']$exists),
    'Missing salmon command, skipping.')
  skip_if(
    (!commandsDt[filename == 'salmon_index']$exists),
    'Missing salmon index, skipping.')
  skip_on_os('windows', arch = NULL)
  step = 'salmon'
  paramsNow = params[[step]]

  paramsNow[c('run', 'keep')] = NULL
  result = do.call(salmon, c(
    list(filepaths = metadata[[fileColname]],
         samples = metadata[[sampleColname]], outputDir = salmonDir),
    paramsNow))
  salmonOutputObs = list.files(salmonDir, recursive = TRUE)
  salmonOutputExp = snapshot(salmonOutputObs, file.path(dataDir, 'salmon_output.qs'))
  expect_equal(salmonOutputObs, salmonOutputExp)

  excludeColumns = c('frag_length_mean', 'frag_length_sd', 'start_time',
                     'end_time', 'eq_class_properties', 'length_classes',
                     'salmon_version')
  salmonMetaObs = getSalmonMetadata(salmonDir, outputDir)[, .SD, .SDcols = !excludeColumns]
  salmonMetaExp = snapshot(salmonMetaObs, file.path(dataDir, 'salmon_meta_info.qs'))
  expect_equal(salmonMetaObs, salmonMetaExp)
})
