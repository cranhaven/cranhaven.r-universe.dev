test_that('mapDiseaseToPhecode output', {

  diseaseHpoMapTest = data.table(
    disease_id = 1, hpo_term_id = c('HP:1', 'HP:2', 'HP:3'))
  hpoPhecodeMapTest = data.table(
    hpo_term_id = c('HP:1', 'HP:2', 'HP:3'), phecode = c('001', '002', '003'))
  mapObs = mapDiseaseToPhecode(
    diseaseHpoMap = diseaseHpoMapTest, hpoPhecodeMap = hpoPhecodeMapTest)
  setkey(mapObs)
  mapExp = diseasePhecodeMapTest

  expect_equal(mapObs, mapExp)
})
