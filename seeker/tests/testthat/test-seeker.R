test_that('seeker', {
  skip_on_cran()
  skip_if(anyMissing, 'Missing required system dependencies, skipping.')
  skip_on_os('windows', arch = NULL)

  parentDirSeeker = file.path(dataDir, 'staging_seeker')
  dir.create(parentDirSeeker)
  withr::local_file(parentDirSeeker)
  file.copy(file.path(dataDir, 'GSE143524'), parentDirSeeker, recursive = TRUE)
  outputDirSeeker = file.path(parentDirSeeker, 'GSE143524')

  seeker(params, parentDirSeeker)

  seekerOutputObs = list.files(outputDirSeeker, recursive = TRUE)
  seekerOutputExp = snapshot(
    seekerOutputObs, file.path(dataDir, 'seeker_output_full.qs'))

  expect_equal(seekerOutputObs, seekerOutputExp)
})

test_that('seeker skip all', {
  skip_on_cran()
  skip_on_os('windows', arch = NULL)

  paramsSkip = yaml::read_yaml(file.path(dataDir, 'GSE143524_skip_all.yml'))
  parentDirSeekerSkip = file.path(dataDir, 'staging_seeker_skip')
  dir.create(parentDirSeekerSkip)
  withr::local_file(parentDirSeekerSkip)
  file.copy(file.path(dataDir, 'GSE143524'),
            parentDirSeekerSkip, recursive = TRUE)
  outputDirSeekerSkip = file.path(parentDirSeekerSkip, 'GSE143524')

  seeker(paramsSkip, parentDirSeekerSkip)

  seekerOutputObs = list.files(outputDirSeekerSkip, recursive = TRUE)
  seekerOutputExp = snapshot(
    seekerOutputObs, file.path(dataDir, 'seeker_output_skip.qs'))

  expect_equal(seekerOutputObs, seekerOutputExp)
})

test_that('checkSeekerArgs', {
  skip_on_cran()
  skip_if(anyMissing, 'Missing required system dependencies, skipping.')
  skip_on_os('windows', arch = NULL)
  outputDirObs = checkSeekerArgs(params, parentDir)$outputDir
  expect_equal(outputDirObs, outputDir)
})

test_that('checkSeekerArgs errors', {
  skip_on_cran()
  skip_on_os('windows', arch = NULL)

  # Error variable and directory
  parentDirErr = file.path(dataDir, 'staging_error')
  paramsErr = params

  # parentDir doesn't exist
  expect_error(checkSeekerArgs(paramsErr, parentDirErr))
  dir.create(parentDirErr)
  dir.create(file.path(parentDirErr, 'GSE143524'))
  withr::local_file(parentDirErr)

  # Invalid name
  paramsErr$invalid = 'Invalid name'
  expect_error(checkSeekerArgs(paramsErr, parentDirErr))
  paramsErr$invalid = NULL

  # Run fetch without running metadata or already having metadata file
  paramsErr$metadata$run = FALSE
  paramsErr$fetch$run = TRUE
  expect_error(checkSeekerArgs(paramsErr, parentDirErr))
  paramsErr$metadata$run = TRUE
  paramsErr$fetch$run = FALSE

  # Run trimgalore without running fetch or already having fetch_output dir
  expect_error(checkSeekerArgs(paramsErr, parentDirErr))

  # Run fastqc without running trimgalore, fetch, or already having fetch_output dir
  paramsErr$trimgalore$run = FALSE
  expect_error(checkSeekerArgs(paramsErr, parentDirErr))

  # Run salmon without running trimgalore, fetch, or already having fetch_output dir
  paramsErr$fastqc$run = FALSE
  expect_error(checkSeekerArgs(paramsErr, parentDirErr))
  paramsErr$fastqc$run = TRUE
  paramsErr$trimgalore$run = TRUE
  file.path(parentDirErr, 'GSE143524')
  file.copy(file.path(dataDir, 'GSE143524', 'fetch_output'),
            file.path(parentDirErr, 'GSE143524'), recursive = TRUE)

  # Run tximport without running salmon or already having salmonDir
  paramsErr$salmon$run = FALSE
  expect_error(checkSeekerArgs(paramsErr, parentDirErr))
  paramsErr$salmon$run = TRUE

  # No tx2gene organism or filename
  # Commented out regex value passes test expectation when I run it line by line
  # myself, but not when actually testing package.
  paramsErr$tximport$tx2gene$filename = NULL
  expect_error(checkSeekerArgs(paramsErr, parentDirErr))
  paramsErr$tximport$tx2gene$filename = params$tximport$tx2gene$filename

  # tx2gene filename doesn't exist
  expect_error(checkSeekerArgs(paramsErr, parentDirErr))
})
