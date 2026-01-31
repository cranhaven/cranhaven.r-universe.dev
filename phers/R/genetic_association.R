#' Perform association tests between phenotype risk scores and genotypes
#'
#' The association test for each disease-variant pair is based on a linear
#' model, with the phenotype risk score as the dependent variable.
#'
#' @param scores A data.table of phenotype risk scores. Must have columns
#'   `person_id`, `disease_id`, `score`.
#' @param genotypes A matrix or 'BEDMatrix' object containing genetic data, with
#'   rownames corresponding to `person_id`s in `demos` and `scores`, and
#'   colnames corresponding to `variant_id`s in `diseaseVariantMap`.
#' @param demos A data.table of characteristics for each person in the cohort.
#'   Must have column `person_id`.
#' @param diseaseVariantMap A data.table indicating which genetic variants to
#'   test for association with phenotype risk scores for which diseases. Must
#'   have columns `disease_id` and `variant_id`.
#' @param lmFormula A formula representing the linear model (excluding the term
#'   for genotype) to use for the association tests. All terms in the formula
#'   must correspond to columns in `demos`.
#' @param modelType A string indicating how to encode genotype in the model.
#' @param level A number indicating the level of the confidence interval.
#'   Default is 0.95.
#' @param dopar Logical indicating whether to run calculations in parallel if
#'   a parallel backend is already set up, e.g., using
#'   [doParallel::registerDoParallel()]. Recommended to minimize runtime.
#'
#' @return A data.table of statistics for the association tests (if a model
#'   fails to converge, NAs will be reported):
#'
#'   * `disease_id`: Disease identifier
#'   * `variant_id`: Variant identifier
#'   * `n_total`: Number of persons with non-missing genotype data for the
#'     given variant.
#'   * `n_wt`: Number of persons homozygous for the wild-type allele.
#'   * `n_het`: Number of persons having one copy of the alternate allele.
#'   * `n_hom`: Number of persons homozygous for the alternate allele.
#'   * `beta`: Coefficient for the association of genotype with score
#'   * `se`: Standard error for `beta`
#'   * `pval`: P-value for `beta` being non-zero
#'   * `ci_lower`: Lower bound of the confidence interval for `beta`
#'   * `ci_upper`: Upper bound of the confidence interval for `beta`
#'
#'   If `modelType` is "genotypic", the data.table will include separate
#'   statistics for heterozygous and homozygous genotypes.
#'
#' @eval example4()
#'
#' @seealso [stats::lm()], [stats::confint()], [getScores()]
#'
#' @export
getGeneticAssociations = function(
  scores, genotypes, demos, diseaseVariantMap, lmFormula,
  modelType = c('genotypic', 'additive', 'dominant', 'recessive'),
  level = 0.95, dopar = FALSE) {

  diseaseId = disease_id = snp = allele_count = count = N = variant_id = person_id = NULL

  checkScores(scores)
  checkGenotypes(genotypes)
  checkDemos(demos)
  checkDiseaseVariantMap(diseaseVariantMap, scores, genotypes)
  checkLmFormula(lmFormula, demos)
  modelType = match.arg(modelType)
  assertNumber(level, lower = 0.5, upper = 1 - .Machine$double.eps)
  assertFlag(dopar)

  lmInput = merge(scores, demos, by = 'person_id')
  lmInput[, person_id := as.character(person_id)]

  reg = foreach::getDoParRegistered()
  doOp = if (dopar && reg) `%dopar%` else `%do%`
  foe = foreach(
    diseaseId = unique(diseaseVariantMap$disease_id), .combine = rbind)

  statsAll = doOp(foe, {

    lmInputSub = lmInput[disease_id == diseaseId, !'disease_id']

    snpNow = unique(diseaseVariantMap[disease_id == diseaseId]$variant_id)
    genoCount = apply(
      genotypes[unique(lmInputSub$person_id), snpNow, drop = FALSE], 2L,
      uniqueN, na.rm = TRUE)
    snpSub = snpNow[genoCount > 1]

    if (length(snpSub) == 0) {
      statsSnps = data.table()}

    else {
      genotypesSub = data.table(
        person_id = rownames(genotypes), genotypes[, snpSub, drop = FALSE])
      statsSnps = foreach(snp = snpSub, .combine = rbind) %do% {
        genotypesSub2 = genotypesSub[, c('person_id', snp), with = FALSE]
        lmInputSub2 = merge(
          lmInputSub, genotypesSub2, by = 'person_id')[, !'person_id']
        setnames(lmInputSub2, snp, 'allele_count')
        lmInputSub2 = lmInputSub2[!is.na(allele_count)]

        lmStat = runLinear(
          lmInputSub2, lmFormula, modelType, diseaseId, snp, level)}}})

  if (nrow(statsAll) > 0) {
    setkeyv(statsAll, c('disease_id', 'variant_id'))
  }

  return(statsAll)}


runLinear = function(
  lmInput, lmFormula, modelType, diseaseId, snp, level = 0.95) {
  ci_lower = melt = pval = se = ci_upper = allele_count = varName = n_het =
    n_hom = n_total = n_wt = genotype = NULL

  checkLmInput(lmInput)
  lmInput1 = copy(lmInput)
  lmFormula = update.formula(lmFormula, score ~ allele_count + .)
  varNames = 'allele_count'

  if (modelType == 'dominant') {
    lmInput1[allele_count == 2, allele_count := 1]}

  else if (modelType == 'recessive') {
    lmInput1[allele_count == 1, allele_count := 0]
    lmInput1[allele_count == 2, allele_count := 1]}

  else if (modelType == 'genotypic') {
    lmInput1[, allele_count := as.factor(allele_count)]
    varNames = c('allele_count1', 'allele_count2')}

  fit = lm(lmFormula, data = lmInput1)
  stat = data.table(disease_id = diseaseId, variant_id = snp)
  stat = cbind(stat, getAlleleCounts(lmInput))

  lmStats = foreach(varName = varNames, .combine = rbind) %do% {
    lmStat = data.table(
      beta = NA, se = NA, pval = NA, ci_lower = NA, ci_upper = NA)

    if (!is.na(fit$coefficients[varName])) {
      fitSnpCoefs = summary(fit)$coefficients[varName, ]
      lmStat[, beta := fitSnpCoefs['Estimate']]
      lmStat[, se := fitSnpCoefs['Std. Error']]
      lmStat[, pval := fitSnpCoefs['Pr(>|t|)']]
      ci = suppressMessages(confint(fit, parm = varName, level = level))
      lmStat[, ci_lower := ci[1L]]
      lmStat[, ci_upper := ci[2L]]}

    if (varName == 'allele_count1') {
      lmStat[, genotype := 'heterozygous']
    } else if (varName == 'allele_count2') {
      lmStat[, genotype := 'homozygous']}

    lmStat}

  stat = cbind(stat, lmStats)
  return(stat)}
