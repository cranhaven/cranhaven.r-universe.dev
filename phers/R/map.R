#' Sample table of ICD occurrences
#'
#' The data are artificial and do not correspond to real patients.
#'
#' @format A data.table with the following columns:
#'
#' * `person_id`: Character vector of the identifier for each person
#' * `icd`: Character vector of the ICD codes recorded for each person
#' * `flag`: Integer vector of the vocabulary of the ICD code
#'   (**9**: ICD-9-CM, **10**: ICD-10-CM)
#' * `entry_date`: Vector of type `Date` indicating the date each ICD code was
#'   recorded.
#'
#' @seealso [getPhecodeOccurrences()], [getWeights()], [getScores()]
'icdSample'


#' Sample table of demographic information
#'
#' The data are artificial and do not correspond to real patients.
#'
#' @format A data table with the following columns:
#'
#' * `person_id`: Character vector of the identifier for each person in the
#'   cohort
#' * `sex`: Character vector indicating biological sex
#'
#' @seealso [getWeights()], [getScores()]
'demoSample'


#' Pre-calculated weights for calculating phenotype risk scores
#'
#' The weights are based on EHR data from the Vanderbilt University Medical
#' Center Synthetic Derivative (SD) and ICD-phecode map version 1.2 and are
#' calculated using the "prevalence" method.
#'
#' @format A data.table with the following columns:
#'
#' * `phecode`: Character vector of phecodes
#' * `prev`: Numeric vector of prevalences, i.e., fraction of subjects in the SD
#'   that have at least one occurrence of the given phecode
#' * `w`: Numeric vector of weights, calculated as `-log10(prev)`
#'
#' @seealso [getWeights()]
'preCalcWeights'


#' Mapping of diseases and diagnostic ICD codes
#'
#' This table provides a mapping between 27 Mendelian diseases and the
#' corresponding ICD-9 and ICD-10 codes that indicate a genetic diagnosis.
#'
#' @format A data.table with the following columns:
#'
#' * `disease_id`: Numeric vector of OMIM disease identifiers
#' * `disease_name`: Character vector of disease names
#' * `icd`: Character vector of ICD codes indicating a genetic diagnosis
#' * `flag`: Integer vector of the vocabulary of the ICD code
#'    (**9**: ICD-9-CM,  **10**: ICD-10-CM)
#' * `icd_name`: Character vector containing the description of each ICD code
#'
#' @seealso [getPhecodeOccurrences()], [getDxStatus()]
'diseaseDxIcdMap'


#' Mapping of Mendelian diseases and their clinical features
#'
#' This table provides a mapping between Mendelian diseases and their clinical
#' features, represented as Human Phenotype Ontology (HPO) terms. The mapping is
#' based on annotations from Online Mendelian Inheritance in Man (OMIM).
#'
#' @format A data.table with the following columns:
#'
#' * `disease_id`: Numeric vector of OMIM disease identifiers
#' * `disease_name`: Character vector of disease names
#' * `hpo_term_id`: Character vector of HPO identifiers corresponding to each
#'   disease's clinical features
#' * `hpo_term_name`: Character vector of HPO term descriptions
#'
#' @source <https://hpo.jax.org/app/download/annotation>
#'
#' @seealso [mapDiseaseToPhecode()]
'diseaseHpoMap'


#' Mapping of HPO terms and phecodes
#'
#' This table provides a mapping between Human Phenotype Ontology (HPO)
#' terms and phecodes, and is useful for using phecodes to represent the
#' clinical features of Mendelian diseases (version 1.2).
#'
#' @format A data.table with the following columns:
#'
#' * `hpo_term_id`: Character vector of HPO term identifiers
#' * `hpo_term_name`: Character vector of HPO term descriptions
#' * `phecode`: Character vector of phecodes
#' * `phecode_name`: Character vector of phecode descriptions
#'
#' @seealso [mapDiseaseToPhecode()]
'hpoPhecodeMap'


#' Mapping of ICD codes and phecodes
#'
#' This table provides a mapping between International Classification of
#' Diseases 9th and 10th revisions (ICD-9-CM and ICD-10-CM) and phecodes
#' (version 1.2).
#'
#' @format A data.table with the following columns:
#'
#' * `icd`: Character vector of ICD codes
#' * `flag`: Integer vector of the vocabulary of the ICD code
#'   (**9**: ICD-9-CM,  **10**: ICD-10-CM)
#' * `icd_name`: Character vector of ICD code descriptions
#' * `phecode`: Character vector of phecodes
#' * `phecode_name`: Character vector of phecode descriptions
#'
#' @source <https://phewascatalog.org/phecodes>
#'
#' @seealso [getPhecodeOccurrences()]
'icdPhecodeMap'


#' Map diseases to phecodes via HPO terms
#'
#' A mapping of diseases to their clinical features, represented as phecodes,
#' is required for calculating phenotype risk scores.
#'
#' @param diseaseHpoMap A data.table containing the mapping between diseases and
#'  HPO terms. Must have columns `disease_id` and `term_id`. Default is the map
#'  included in this package.
#' @param hpoPhecodeMap A data.table containing the mapping between HPO terms
#'   and phecodes. Must have columns `term_id` and `phecode`. Default is the map
#'   included in this package.
#'
#' @return A data.table with columns `disease_id` and `phecode`.
#'
#' @eval example1()
#'
#' @seealso [getScores()]
#'
#' @export
mapDiseaseToPhecode = function(
  diseaseHpoMap = phers::diseaseHpoMap, hpoPhecodeMap = phers::hpoPhecodeMap) {

  assertDataTable(diseaseHpoMap)
  assertNames(
    colnames(diseaseHpoMap), type = 'unique',
    must.include = c('disease_id', 'hpo_term_id'))
  assertCharacter(diseaseHpoMap$hpo_term_id)

  assertDataTable(hpoPhecodeMap)
  assertNames(
    colnames(hpoPhecodeMap), type = 'unique',
    must.include = c('hpo_term_id', 'phecode'))
  assertCharacter(hpoPhecodeMap$hpo_term_id)
  assertCharacter(hpoPhecodeMap$phecode, any.missing = FALSE, min.chars = 1)

  diseasePhecodeMap = merge(diseaseHpoMap, hpoPhecodeMap, by = 'hpo_term_id')
  diseasePhecodeMap = unique(diseasePhecodeMap[, c('disease_id', 'phecode')])
  setkeyv(diseasePhecodeMap, c('disease_id', 'phecode'))

  return(diseasePhecodeMap)}
