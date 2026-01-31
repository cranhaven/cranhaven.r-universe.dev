ws = c(0.5, rep(1, 3), c(1, 0.5, 0.5, 1), c(1, 0.5, 1, 1), c(rep(1, 3), 0.5))
weightsTest = data.table(
  CJ(person_id = 1:4, phecode = c('001', '002', '003', '004')),
  pred = ws, w = -log10(ws))

icdPhecodeMapTest = data.table(
  icd = c('001', '002', '003', '004', '005', '006'),
  phecode = c('001', '002', '003', '004', '005', '006'), flag = 9)

scoresPrevExp = data.table(
  person_id = seq_len(4),
  disease_id = rep(1, 4),
  score = c(0.60206, 0.90309, 0.30103, 0.00000))
setkeyv(scoresPrevExp, c('person_id', 'disease_id'))

scoresLgExp = data.table(
  person_id = seq_len(4),
  disease_id = rep(1, 4),
  score = c(0.30103, 0.60206, 0.30103, 0.00000))
setkeyv(scoresLgExp, c('person_id', 'disease_id'))


test_that('getPhecodeOccurrences output', {

  resObs = getPhecodeOccurrences(
    icdTest, icdPhecodeMap = icdPhecodeMapTest, dxIcd = dxIcdTest)
  setkey(resObs)
  resExp = phecodeOccurrencesTest

  expect_equal(resObs, resExp, ignore_attr = TRUE)
})


test_that('getPhecodeOccurrences output (dxIcd = NULL)', {

  resObs = getPhecodeOccurrences(
    icdTest, icdPhecodeMap = icdPhecodeMapTest, dxIcd = NULL)
  setkey(resObs)
  resExp = data.table(
    person_id = c(1, rep(2L, 3), 3, rep(4L, 3)),
    phecode = c('001', '002', '003', '005', '002', '004', '005', '006'),
    occurrence_age = c(rep(10, 6), c(10, 11)))
  setkey(resExp)

  expect_equal(resObs, resExp)
})


test_that('getPhecodeOccurrences args error', {

  # ICD is numeric
  icdTestErr = copy(icdTest)
  icdTestErr[, icd := as.numeric(icd)]
  expect_error(getPhecodeOccurrences(icdTestErr))

  # no ICD flags
  icdTestErr = icdTest[, .(person_id, icd)]
  expect_error(getPhecodeOccurrences(icdTestErr))
})


test_that('getScores output', {

  resObs = getScores(weightsTest, diseasePhecodeMapTest)
  resExp = scoresLgExp
  expect_equal(resObs, resExp, ignore_attr = TRUE)
})


test_that('getScores args error', {

  # phecodes are numeric in weights
  weightsTestErr = copy(weightsTest)
  weightsTestErr[, phecode := as.numeric(phecode)]
  expect_error(getScores(weightsTestErr, diseasePhecodeMapTest))
})


test_that('getResidualScores output', {
  resObs = getResidualScores(demosTest2, scoresTest, formTest)
  resExp = snapshot(resObs, file.path(dataDir, 'get_residual_scores_output.qs'))

  expect_equal(resObs, resExp)
})
