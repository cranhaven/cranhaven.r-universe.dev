lmInputTest = data.table(
  score = c(5, 3, 4, 1, 0.5, 0),
  allele_count = c(2, 1, 1, 0, 0, 0),
  sex = c('female', 'male', 'female', 'male', 'female', 'male'))


test_that('runLinear output (additive)', {

  resObs = runLinear(lmInputTest, formTest, 'additive', 1, 'snp1')
  resExp = snapshot(resObs, file.path(dataDir, 'run_linear_additive_output.qs'))

  expect_equal(resObs, resExp)
})


test_that('runLinear output (dominant)', {

  resObs = runLinear(lmInputTest, formTest, 'dominant', 1, 'snp1')
  resExp = snapshot(resObs, file.path(dataDir, 'run_linear_dominant_output.qs'))

  expect_equal(resObs, resExp)
})


test_that('runLinear output (recessive)', {

  resObs = runLinear(lmInputTest, formTest, 'recessive', 1, 'snp1')
  resExp = snapshot(resObs, file.path(dataDir, 'run_linear_recessive_output.qs'))

  expect_equal(resObs, resExp)
})


test_that('runLinear output (genotypic)', {

  resObs = runLinear(lmInputTest, formTest, 'genotypic', 1, 'snp1')
  resExp = snapshot(resObs, file.path(dataDir, 'run_linear_genotypic_output.qs'))

  expect_equal(resObs, resExp)
})


test_that('getGeneticAssociations output (additive)', {

  genotypesTest = BEDMatrix(file.path(dataDir, 'geno_test'))
  rownames(genotypesTest) = 1:6
  colnames(genotypesTest) = c('snp1', 'snp2')
  diseaseGeneVarMapTest = data.table(
    disease_id = c(1, 2), gene = c('a', 'b'), variant_id = c('snp1', 'snp2'))
  resObs = getGeneticAssociations(
    scoresTest, genotypesTest, demosTest2, diseaseGeneVarMapTest, formTest,
    modelType = 'additive')

  resExp = snapshot(
    resObs, file.path(dataDir, 'get_genetic_associations_additive_output.qs'))

  expect_equal(resObs, resExp)
})
