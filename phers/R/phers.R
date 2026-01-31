#' Map ICD code occurrences to phecode occurrences
#'
#' This is typically the first step of an analysis using phenotype risk scores,
#' the next is [getWeights()].
#'
#' @param icdOccurrences A data.table of occurrences of ICD codes for each
#'   person in the cohort. Must have columns `person_id`, `icd`, and `flag`.
#' @param icdPhecodeMap A data.table of the mapping between ICD codes and
#'   phecodes. Must have columns `icd`, `phecode`, and `flag`. Default is the
#'   map included in this package.
#' @param dxIcd A data.table of ICD codes to exclude from mapping to phecodes.
#'   Must have columns `icd` and `flag`. Default is the table of Mendelian
#'   diseases and the corresponding ICD codes that indicate a genetic diagnosis.
#'   If `NULL`, no ICD codes will be excluded.
#'
#' @return A data.table of phecode occurrences for each person.
#'
#' @eval example2()
#'
#' @seealso [getWeights()], [getScores()]
#'
#' @export
getPhecodeOccurrences = function(
  icdOccurrences, icdPhecodeMap = phers::icdPhecodeMap,
  dxIcd = phers::diseaseDxIcdMap) {
  flag = icd = person_id = . = NULL

  checkIcdOccurrences(icdOccurrences)
  checkIcdPhecodeMap(icdPhecodeMap)
  checkDxIcd(dxIcd, nullOk = TRUE)

  # remove diagnostic codes
  if (!is.null(dxIcd)) {
    icdOccurrences = icdOccurrences[!dxIcd, on = c('icd', 'flag')]}

  pheOccs = merge(
    icdOccurrences, icdPhecodeMap[, c('icd', 'flag', 'phecode')],
    by = c('icd', 'flag'), allow.cartesian = TRUE)
  pheOccs = unique(pheOccs[, !c('icd', 'flag')])

  setcolorder(pheOccs, c('person_id', 'phecode'))
  setkeyv(pheOccs, c('person_id', 'phecode'))
  return(pheOccs)}


#' Calculate phenotype risk scores
#'
#' A person's phenotype risk score for a given disease corresponds to the
#' sum of the weights of the disease-relevant phecodes that the person has
#' received.
#'
#' @param weights A data.table of phecodes and their corresponding weights.
#'   Must have columns `person_id`, `phecode` and `w`.
#' @param diseasePhecodeMap A data.table of the mapping between diseases and
#'   phecodes. Must have columns `disease_id` and `phecode`.
#'
#' @return A data.table containing the phenotype risk score for each person for
#'   each disease.
#'
#' @eval example2()
#'
#' @seealso [mapDiseaseToPhecode()], [getPhecodeOccurrences()], [getWeights()],
#'   [getResidualScores()]
#'
#' @export
getScores = function(weights, diseasePhecodeMap) {
  person_id = phecode = disease_id = w = score = . = NULL

  checkWeights(weights)
  checkDiseasePhecodeMap(diseasePhecodeMap)

  rBig = merge(
    weights[, .(person_id, phecode, w)], diseasePhecodeMap,
    by =  'phecode', allow.cartesian = TRUE)
  r = rBig[, .(score = sum(w)), keyby = .(person_id, disease_id)]

  return(r[])}


#' Calculate residual phenotype risk scores
#'
#' The residual score indicates to what extent a person's phenotype risk score
#' for a given disease deviates from the expected score, after adjusting for
#' the person's characteristics in a linear model.
#'
#' @param demos A data.table of characteristics for each person in the cohort.
#'   Must have column `person_id`.
#' @param scores A data.table containing the phenotype risk score for each
#'   person for each disease. Must have columns `person_id`, `disease_id`, and
#'   `score`.
#' @param lmFormula A formula representing the linear model to use for
#'   calculating residual scores. All terms in the formula must correspond to
#'   columns in `demos`.
#'
#' @return A data.table, based on `scores`, with an additional column
#'   `resid_score`. Residual scores for each disease are standardized to have
#'   unit variance.
#'
#' @eval example2()
#'
#' @seealso [stats::rstandard()], [getScores()]
#'
#' @export
getResidualScores = function(demos, scores, lmFormula) {
  disease_id = diseaseId = resid_score = . = person_id = score = NULL

  checkDemos(demos)
  checkScores(scores)
  checkLmFormula(lmFormula, demos)

  rInput = merge(scores, demos, by = 'person_id')
  lmFormula = update.formula(lmFormula, score ~ .)

  rScores = rInput[
    , .(person_id, score,
        resid_score = rstandard(lm(lmFormula, data = .SD))),
    keyby = disease_id]
  setkeyv(rScores, c('person_id', 'disease_id'))
  setcolorder(rScores, c('person_id', 'disease_id', 'score', 'resid_score'))

  return(rScores[])}
