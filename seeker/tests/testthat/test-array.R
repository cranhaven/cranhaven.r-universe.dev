paramsArray = yaml::read_yaml(file.path(dataDir, 'GSE25585.yml'))
parentDirArr = file.path(dataDir, 'staging_seeker_array')
metadataOnly = FALSE
dir.create(parentDirArr)
withr::local_file(parentDirArr)

test_that('checkSeekerArrayArgs', {
  skip_on_os('windows', arch = NULL)

  resultObs = checkSeekerArrayArgs(
    paramsArray$study, paramsArray$geneIdType, paramsArray$platform,
    parentDirArr, FALSE)
  resultExp = snapshot(
    resultObs, file.path(dataDir, 'seeker_array_args_output.qs'))

  expect_equal(resultObs, resultExp)
})


test_that('checkSeekerArrayArgs errors', {
  skip_on_os('windows', arch = NULL)

  # GSE platform not null or GPL
  expect_error(checkSeekerArrayArgs(
    paramsArray$study, paramsArray$geneIdType, 'abcd', parentDirArr, metadataOnly))

  # E- with platform
  expect_error(checkSeekerArrayArgs(
    'E-test', paramsArray$geneIdType, 'abcd', parentDirArr, metadataOnly))

  # raw with raw dir not existing
  expect_error(checkSeekerArrayArgs(
    'LOCAL', paramsArray$geneIdType, 'GPL1', parentDirArr, metadataOnly))
})


test_that('seekerArray GSE', {
  skip_on_os('windows', arch = NULL)
  skip_on_cran()
  skip_on_ci() # RMA refuses to work on the GH Actions runner

  seekerArray(
    paramsArray$study, paramsArray$geneIdType, paramsArray$platform,
    parentDirArr)

  resultObs = list.files(parentDirArr, recursive = TRUE)
  resultExp = snapshot(
    resultObs, file.path(dataDir, 'seeker_array_gse_output.qs'))

  expect_equal(resultObs, resultExp)
})


# test_that('seekerArray Ae', {
#   skip_on_os('windows', arch = NULL)
#   skip_on_cran()
#
#   parentDirArrAe = file.path(dataDir, 'staging_seeker_array_ae')
#   dir.create(parentDirArrAe)
#   withr::local_file(parentDirArrAe)
#   paramsArrayAe = paramsArray
#   paramsArrayAe$study = 'E-MTAB-8714'
#
#   seekerArray(paramsArrayAe$study, paramsArrayAe$geneIdType,
#               paramsArrayAe$platform, parentDirArrAe)
#
#   resultObs = list.files(parentDirArrAe, recursive = TRUE)
#   resultExp = snapshot(
#     resultObs, file.path(dataDir, 'seeker_array_ae_output.qs'))
#
#   expect_equal(resultObs, resultExp)
# })


test_that('seekerArray LOCAL', {
  skip_on_os('windows', arch = NULL)
  skip_on_cran()
  skip_on_ci() # RMA refuses to work on the GH Actions runner

  parentDirArrLcl = file.path(dataDir, 'staging_seeker_array_local')
  dir.create(parentDirArrLcl)
  withr::local_file(parentDirArrLcl)
  paramsArrayLocal = yaml::read_yaml(file.path(dataDir, 'LOCAL01.yml'))
  file.copy(file.path(dataDir, 'LOCAL01'), parentDirArrLcl, recursive = TRUE)

  seekerArray(
    paramsArrayLocal$study, paramsArrayLocal$geneIdType,
    paramsArrayLocal$platform, parentDirArrLcl)

  resultObs = list.files(parentDirArrLcl, recursive = TRUE)
  resultExp = snapshot(
    resultObs, file.path(dataDir, 'seeker_array_local_output.qs'))

  expect_equal(resultObs, resultExp)
})
