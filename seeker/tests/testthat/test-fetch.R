test_that('fetchMetadata', {
  skip_on_os('windows', arch = NULL)
  step = 'metadata'
  paramsNow = params[[step]]

  metadataObs = fetchMetadata(paramsNow$bioproject)
  skip_if(nrow(metadataObs) == 0L)

  idx = metadataObs[[paramsNow$include$colname]] %in% paramsNow$include$values
  metadataObs = metadataObs[idx]

  skip_if(isTRUE(all.equal(names(metadataObs), '<!doctype html>')),
          'Skipping, as ENA temporarily unavailable.')

  # metadataExp = snapshot(
  #   metadataObs, file.path(dataDir, 'fetch_metadata_output.qs'))
  # expect_equal(metadataObs, metadataExp)
  expect_data_table(metadataObs, min.rows = 1L, min.cols = 1L)

  # Test on another bioproject
  params2 = yaml::read_yaml(file.path(dataDir, 'GSE159135.yml'))
  paramsNow2 = params2[[step]]
  metadataObs2 = fetchMetadata(paramsNow2$bioproject)
  idx2 = metadataObs2[[paramsNow2$include$colname]] %in%
    paramsNow2$include$values
  metadataObs2 = metadataObs2[idx2]

  # metadataExp2 = snapshot(
  #   metadataObs2, file.path(dataDir, 'fetch_metadata_output_2.qs'))
  # expect_equal(metadataObs2, metadataExp2)
  expect_data_table(metadataObs2, min.rows = 1L, min.cols = 1L)
})

test_that('fetch', {
  skip_on_cran()
  skip_if(
    !commandsDt[filename == 'prefetch']$exists,
    'Missing prefetch command, skipping.')
  skip_if(
    !commandsDt[filename == 'pigz']$exists, 'Missing pigz command, skipping.')
  skip_on_os('windows', arch = NULL)
  outputDirFetchTest = file.path(parentDir, 'GSM5694054')
  if (!dir.exists(outputDirFetchTest)) dir.create(outputDirFetchTest)
  step = 'metadata'

  paramsFetch = yaml::read_yaml(file.path(dataDir, 'GSM5694054.yml'))
  paramsFetchNow = paramsFetch[[step]]
  metadataGSM = fetchMetadata(paramsFetchNow$bioproject, host = 'ena')
  idx = metadataGSM[[paramsFetchNow$include$colname]] %in%
    paramsFetchNow$include$values
  metadataGSM = metadataGSM[idx]

  step = 'fetch'
  paramsFetchNow = paramsFetch[[step]]

  paramsFetchNow[c('run', 'keep')] = NULL

  resultObs = do.call(fetch, c(
    list(accessions = metadataGSM[[remoteColname]],
         outputDir = outputDirFetchTest),
    paramsFetchNow))

  resultExp = snapshot(resultObs, file.path(dataDir, 'fetch_output.qs'))

  expect_equal(resultObs, resultExp)

  for (file in strsplit(resultExp$localFilepaths, ';')[[1]]) {
    expect_true(file.exists(file))
  }
})
