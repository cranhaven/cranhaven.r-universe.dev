getPreCalcWeights = function(demos, phecodeOccurrences, negativeWeights) {
  phecode = person_id = . = pred = w = dx_status = NULL

  wBig = CJ(
    person_id = demos$person_id, phecode = unique(phecodeOccurrences$phecode))
  wBig = merge(
    wBig, unique(phecodeOccurrences[, .(person_id, phecode, dx_status = 1)]),
    by = c('person_id', 'phecode'), all.x = TRUE, allow.cartesian = TRUE)
  wBig[is.na(dx_status), dx_status := 0]
  weights = merge(wBig, phers::preCalcWeights[, .(phecode, pred)], by = 'phecode')

  weights[, w := (
    1 - 2 * dx_status) * log10(dx_status * pred + (1 - dx_status) * (1 - pred))]
  if (!negativeWeights) {
    weights[, w := dx_status * w]}
  weights[, dx_status := NULL]
  setcolorder(weights, 'person_id')

  return(weights)}


getWeightsPrevalence = function(demos, phecodeOccurrences, negativeWeights) {
  phecode = person_id = . = pred = w = dx_status = NULL

  weights = phecodeOccurrences[, .(
    pred = uniqueN(person_id) / nrow(demos)),
    keyby = phecode]

  wBig = CJ(
    person_id = demos$person_id, phecode = unique(phecodeOccurrences$phecode))
  wBig = merge(
    wBig, unique(phecodeOccurrences[, .(person_id, phecode, dx_status = 1)]),
    by = c('person_id', 'phecode'), all.x = TRUE, allow.cartesian = TRUE)
  wBig[is.na(dx_status), dx_status := 0]
  weights = merge(wBig, weights, by = 'phecode')

  weights[, w := (
    1 - 2 * dx_status) * log10(dx_status * pred + (1 - dx_status) * (1 - pred))]
  if (!negativeWeights) {
    weights[, w := dx_status * w]}
  weights[, dx_status := NULL]
  setcolorder(weights, 'person_id')

  return(weights)}


getWeightsLogistic = function(
    demos, phecodeOccurrences, methodFormula, negativeWeights, foreachCall, doOp) {
  phecode = person_id = . = w = phe = dx_status = pred = NULL

  weights = doOp(foreachCall, {
    pheSub = unique(phecodeOccurrences[phecode == phe, .(person_id, phecode)])
    glmInput = merge(demos, pheSub, by = 'person_id', all.x = TRUE)
    glmInput[!is.na(phecode), dx_status := 1]
    glmInput[is.na(phecode), dx_status := 0]
    glmInput[, phecode := phe]

    methodFormula = update.formula(methodFormula, dx_status ~ .)
    fit = glm(methodFormula, data = glmInput, family = 'binomial')
    glmInput[, pred := predict(
      fit, newdata = .SD, type = 'response', se.fit = FALSE)]
    glmInput = glmInput[, .(person_id, phecode, pred, dx_status)]})

  weights[, w := (
    1 - 2 * dx_status) * log10(dx_status * pred + (1 - dx_status) * (1 - pred))]
  if (!negativeWeights) {
    weights[, w := dx_status * w]}
  weights[, dx_status := NULL]
  return(weights)}


getWeightsLoglinear = function(
    demos, phecodeOccurrences, methodFormula, negativeWeights, foreachCall, doOp) {
  phecode = person_id = . = w = phe = pred = num_occurrences = NULL

  weights = doOp(foreachCall, {
    pheSub = unique(
      phecodeOccurrences[phecode == phe, .(person_id, phecode, num_occurrences)])
    lmInput = merge(demos, pheSub, by = 'person_id', all.x = TRUE)

    lmInput[is.na(phecode), num_occurrences := 0]
    lmInput[, phecode := phe]

    methodFormula = update.formula(methodFormula, log2(num_occurrences + 1) ~ .)
    fit = lm(methodFormula, data = lmInput)
    lmInput[, pred := predict(fit, newdata = .SD)]
    lmInput = lmInput[, .(person_id, phecode, num_occurrences, pred)]})

  weights[, w := log2(num_occurrences + 1) - pred]
  weights[, num_occurrences := NULL]
  return(weights)}


getWeightsCox = function(
    demos, phecodeOccurrences, methodFormula, negativeWeights, foreachCall, doOp) {
  phecode = person_id = . = w = phe = dx_status = pred = occurrence_age =
    first_age = last_age = age2 = NULL

  weights = doOp(foreachCall, {
    pheSub = unique(
      phecodeOccurrences[phecode == phe, .(person_id, phecode, occurrence_age)])
    coxInput = merge(demos, pheSub, by = 'person_id', all.x = TRUE)

    coxInput[!is.na(phecode), `:=`(dx_status = 1, age2 = occurrence_age)]
    coxInput[is.na(phecode), `:=`(dx_status = 0, age2 = last_age)]
    coxInput[, phecode := phe]
    coxInput[, `:=`(last_age = NULL, occurrence_age = NULL)]

    coxInput[age2 == first_age, age2 := age2 + (1 / 365.25)]
    coxInput = coxInput[age2 > first_age]

    methodFormula = update.formula(
      methodFormula, Surv(first_age, age2, dx_status) ~ .)
    fit = coxph(methodFormula, data = coxInput, model = TRUE)
    coxInput[, pred := 1 - exp(
      -predict(fit, newdata = .SD, type = 'expected', se.fit = FALSE))]
    coxInput = coxInput[, .(person_id, phecode, pred, dx_status)]})

  weights[, w := (
    1 - 2 * dx_status) * log10(dx_status * pred + (1 - dx_status) * (1 - pred))]
  if (!negativeWeights) {
    weights[, w := dx_status * w]}
  weights[, dx_status := NULL]
  return(weights)}


#' Calculate phecode-specific weights for phenotype risk scores
#'
#' This is typically the second step of an analysis using phenotype risk scores,
#' the next is [getScores()].
#'
#' @param demos A data.table having one row per person in the cohort. Must have
#'   a column `person_id`. When the `cox` method is used, `demos`
#'   must have columns `first_age` and `last_age` corresponding to first and
#'   last age of visit (in years).
#' @param phecodeOccurrences A data.table of phecode occurrences for each person
#'   in the cohort. Must have columns `person_id` and `phecode` under the
#'   "prevalence" or "logistic" methods, columns `person_id`, `phecode`, and
#'   `num_occurrences` under the "loglinear" method, and columns `person_id`,
#'   `phecode`, and `occurrence_age` under the "cox" method. `num_occurrences`
#'   refers to the number of unique dates a phecode was recorded for a person.
#'   `occurrence_age` refers to the first age (in years) a person acquired a
#'   phecode.
#' @param method A string indicating the statistical model for calculating
#'   weights.
#' @param methodFormula A formula representing the right-hand side of the model
#'   corresponding to `method`. All terms in the formula must correspond to
#'   columns in `demos`. A method formula is not required for the "prevalence"
#'   and "prevalence_precalc" methods. Do not use age-related covariates with
#'   the "cox" method.
#' @param negativeWeights Logical indicating whether to allow negative weights
#'   for individuals with no occurrences of a phecode. This option is not
#'   required for the "loglinear" method since under this method, individuals
#'   with a nonzero phecode occurrence can also have negative weights.
#' @param dopar Logical indicating whether to run calculations in parallel if
#'   a parallel backend is already set up, e.g., using
#'   [doParallel::registerDoParallel()]. Recommended to minimize runtime.
#'
#' @return A data.table with columns `person_id`, `phecode`, `pred`, and `w`.
#'   The column `pred` represents a different quantity depending on `method`.
#'   Under the "prevalence" `method`, it is fraction of the cohort that has
#'   at least one occurrence of the given phecode. The "prevalence_precalc"
#'   `method` is similar to the "prevalence" `method` but `pred` is calculated
#'   based on EHR data from the Vanderbilt University Medical Center.
#'   Under "logistic" or "cox" `method`, it is the predicted probability of
#'   given individual having a given phecode based on `methodFormula`.
#'   Under the "loglinear" `method`, it is the predicted
#'   `log2(num_occurrences + 1)` of a given phecode for a given individual
#'   based on `methodFormula`. For the "prevalence", "prevalence_precalc",
#'   "cox", and "logistic" `method`s, weight is calculated as `-log10(pred)`
#'   when an individual has non-zero phecode occurrence and `log10(1 - pred)`
#'   when an individual has zero phecode occurrence. For the "loglinear" `method`
#'   weight is calculated as the difference between the observed
#'   `log2(num_occurrences + 1)` and `pred`.
#'
#' @eval example1()
#'
#' @seealso [getPhecodeOccurrences()], [getScores()]
#'
#' @export
getWeights = function(
    demos, phecodeOccurrences,
    method = c('prevalence', 'logistic', 'cox', 'loglinear',
               'prevalence_precalc'),
    methodFormula = NULL, negativeWeights = FALSE, dopar = FALSE) {

  method = match.arg(method)
  checkDemos(demos, method)
  checkPhecodeOccurrences(phecodeOccurrences, demos, method)
  assertFlag(negativeWeights)

  if (method %in% c('prevalence', 'prevalence_precalc')) {
    getWeightsFunc = switch(
      method, prevalence = getWeightsPrevalence,
      prevalence_precalc = getPreCalcWeights)
    weights = getWeightsFunc(demos, phecodeOccurrences, negativeWeights)
    return(weights[])}

  checkMethodFormula(methodFormula, demos)
  assertFlag(dopar)

  reg = foreach::getDoParRegistered()
  doOp = if (dopar && reg) `%dopar%` else `%do%`
  foe = foreach(phe = unique(phecodeOccurrences$phecode), .combine = rbind)

  getWeightsFunc = switch(
    method, logistic = getWeightsLogistic,
    loglinear = getWeightsLoglinear, cox = getWeightsCox)

  weights = getWeightsFunc(
    demos, phecodeOccurrences, methodFormula, negativeWeights, foe, doOp)
  return(weights[])}
