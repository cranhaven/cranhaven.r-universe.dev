test_that('getWeights output (prevalence model)', {
  resObs = getWeights(demosTest, phecodeOccurrencesTest, method = 'prevalence')
  resExp = snapshot(
    resObs, file.path(dataDir, 'get_weights_prevalence_output.qs'))
  expect_equal(resObs, resExp)
})


test_that('getWeights output (prevalence model, negative weights)', {
  resObs = getWeights(
    demosTest, phecodeOccurrencesTest, method = 'prevalence',
    negativeWeights = TRUE)
  resExp = snapshot(
    resObs, file.path(dataDir, 'get_weights_prevalence_negative_output.qs'))
  expect_equal(resObs, resExp)
})


test_that('getWeights output (logistic model)', {
  resObs = getWeights(
    demosTest, phecodeOccurrencesTest, method = 'logistic',
    methodFormula = ~ sex)
  resExp = snapshot(resObs, file.path(dataDir, 'get_weights_logistic_output.qs'))
  expect_equal(resObs, resExp)
})


test_that('getWeights output (logistic model, negative weights)', {
  resObs = getWeights(
    demosTest, phecodeOccurrencesTest, method = 'logistic',
    methodFormula = ~ sex, negativeWeights = TRUE)
  resExp = snapshot(
    resObs, file.path(dataDir, 'get_weights_logistic_negative_output.qs'))
  expect_equal(resObs, resExp)
})


test_that('getWeights output (loglinear model)', {
  resObs = getWeights(
    demosTest, phecodeOccurrencesLLTest, method = 'loglinear',
    methodFormula = ~ sex)
  resExp = snapshot(resObs, file.path(dataDir, 'get_weights_loglinear_output.qs'))
  expect_equal(resObs, resExp)
})


test_that('getWeights output (cox model)', {
  resObs = suppressWarnings(getWeights(
    demosTest, phecodeOccurrencesCoxTest, method = 'cox',
    methodFormula = ~ sex))
  resExp = snapshot(resObs, file.path(dataDir, 'get_weights_cox_output.qs'))
  expect_equal(resObs, resExp)
})


test_that('getWeights output (cox model, negative weights)', {
  resObs = suppressWarnings(getWeights(
    demosTest, phecodeOccurrencesCoxTest, method = 'cox',
    methodFormula = ~ sex, negativeWeights = TRUE))
  resExp = snapshot(
    resObs, file.path(dataDir, 'get_weights_cox_negative_output.qs'))
  expect_equal(resObs, resExp)
})


test_that('getWeights output (prevalence_precalc model)', {
  resObs = getWeights(
    demosTest, phecodeOccurrencesPreCalcTest, method = 'prevalence_precalc')
  resExp = snapshot(
    resObs, file.path(dataDir, 'get_weights_prevalence_precalc_output.qs'))
  expect_equal(resObs, resExp)
})


test_that('getWeights args error', {

  # no column named person_id
  demosTestErr = copy(demosTest)
  setnames(demosTestErr, 'person_id', 'person_id_Err')
  expect_error(getWeights(demosTestErr, phecodeOccurrencesTest))

  # no column named person_id in phecodes
  phecodeOccurrencesTestErr = copy(phecodeOccurrencesTest)
  setnames(phecodeOccurrencesTestErr, 'person_id', 'person_id_Err')
  expect_error(getWeights(demosTest, phecodeOccurrencesTestErr))

  # demo has less person_ids than phecodes
  demosTestErr = demosTest[1]
  expect_error(getWeights(demosTestErr, phecodeOccurrencesTest))
})
