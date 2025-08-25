#' Selection of assays of iris

#' @keywords internal
#' @noRd
ice_assays <- function() {
  ice_carc_woe <-
    c(
      "OPP Carcinogenicity",
      "Report on Carcinogens",
      "IRIS Carcinogenicity",
      "IARC Carcinogenicity",
      "IRIS Carcinogenicity"
    )


  ice_invivo_acute_tox <-
    c(
      "Rat Acute Oral Toxicity",
      "Rat Acute Inhalation Toxicity",
      "Rat Acute Dermal Toxicity"
    )


  ice_invivo_sensitization <- c(
    "Human Maximization Test",
    "Human Repeat Insult Patch Test",
    "LLNA",
    "Guinea Pig Maximization/Buehler"
  )

  ice_invivo_irritation <- c(
    "Draize Skin Irritation/Corrosion Test",
    "Draize Eye Irritation/Corrosion Test",
    "Four-hour Human Patch Test"
  )


  ice_invivo_endocrine <- c(
    "Hershberger-Agonist",
    "Hershberger-Antagonist",
    "Uterotrophic-Agonist"
  )

  ice_cancer <- c(
    "In Vitro Genotoxicity",
    "In Vivo Genotoxicity",
    "NTP Carcinogenicity",
    "Two year cancer bioassay"
  )

  ice_dart <- c(
    "Urinalysis", "Microscopic Pathology",
    "Gross Pathology", "Organ Weight", "In Life Observation", "Hematology",
    "Clinical Chemistry", "Cholinesterase", "in vivo", "Offspring Survival Late",
    "Developmental Malformation", "Developmental Landmark", "Reproductive Performance",
    "Offspring Survival Early"
  )

  list(
    ice_carc_woe = ice_carc_woe,
    ice_invivo_acute_tox = ice_invivo_acute_tox,
    ice_invivo_sensitization = ice_invivo_sensitization,
    ice_invivo_irritation = ice_invivo_irritation,
    ice_invivo_endocrine = ice_invivo_endocrine,
    ice_cancer = ice_cancer,
    ice_dart = ice_dart
  )
}
