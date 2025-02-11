test_that('getTx2gene', {
  skip_on_os('windows', arch = NULL)
  step = 'tximport'
  paramsNow = params[[step]]

  paramsNow$run = NULL
  paramsNow$tx2gene$filename = NULL
  paramsNow$tx2gene$checkArgsOnly = TRUE

  tx2geneObs = do.call(getTx2gene, c(
    list(outputDir = outputDir), paramsNow$tx2gene))

  tx2geneExp = snapshot(tx2geneObs, file.path(dataDir, 'get_tx2gene_output.qs'))

  expect_equal(tx2geneObs, tx2geneExp, ignore_attr = TRUE)
})

test_that('tximport', {
  skip_on_os('windows', arch = NULL)
  step = 'tximport'
  paramsNow = params[[step]]

  paramsNow$run = NULL

  salmonExpDir = file.path(dataDir, 'salmon_output_exp')

  # tx2gene = getTx2gene(outputDir = NULL)
  # set.seed(100)
  # tx2gene = tx2gene[sample.int(.N, 100)]
  # qsave(tx2gene, 'tests/testthat/data/tx2gene_output.qs')
  tx2gene = qread(file.path(dataDir, 'tx2gene_output.qs'))
  paramsNow$tx2gene$version = attr(tx2gene, 'version') # for output yml
  paramsNow$tx2gene = NULL # for calling tximport

  tximportObs = do.call(
    tximport,
    c(list(inputDir = salmonExpDir, tx2gene = tx2gene,
           samples = metadata[[sampleColname]],
           outputDir = outputDir),
      paramsNow))

  tximportExp = snapshot(tximportObs, file.path(dataDir, 'tximport_output.qs'))

  for (name in names(tximportExp)) {
    expect_equal(tximportObs[[name]], tximportExp[[name]])}
})
