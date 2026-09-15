#' Psychosis Disorder Data
#'
#' Ratings of positive and negative symptoms in psychosis disorders,
#' based on Andreasen’s Scale for Assessment of Positive Symptoms (SAPS)
#' and Scale for Assessment of Negative Symptoms (SANS).
#'
#'
#' @details
#' This data set comprises 95 subjects, of whom 69 were diagnosed with
#' schizophrenia and 26 with bipolar disorder. All symptoms were rated
#' on a six‐point scale (0–5).
#'
#' @format A data frame with 95 observations and 51 variables:
#' \describe{
#'   \item{UNIQID}{Factor indicating disorder type.}
#'   \item{AH1, AH2, AH3, AH4, AH5, AH6}{Hallucinations subscale (SAPS).}
#'   \item{DL1, DL2, DL3, DL4, DL5, DL6, DL7, DL8, DL9, DL10, DL11, DL12}{Delusions subscale (SAPS).}
#'   \item{BE1, BE2, BE3, BE4}{Behavior subscale (SAPS).}
#'   \item{TH1, TH2, TH3, TH4, TH5, TH6, TH7, TH8}{Thought disorder subscale (SAPS).}
#'   \item{NA1, NA2, NA3, NA4, NA5, NA6, NA7}{Expression subscale (SANS).}
#'   \item{NB1, NB2, NB3, NB4}{Speech subscale (SANS).}
#'   \item{NC1, NC2, NC3}{Hygiene subscale (SANS).}
#'   \item{ND1, ND2, ND3, ND4}{Activity subscale (SANS).}
#'   \item{NE1, NE2}{Inattentiveness subscale (SANS).}
#' }
#'
"Psychosis_Disorder"


#' Diabetes patient records.
#'
#' http://archive.ics.uci.edu/ml/datasets/diabetes
#' https://www.kaggle.com/uciml/pima-indians-diabetes-database
#'
#' @format A data frame with 768 observations and 9 variables:
#' \code{Pregnancies}, \code{Glucose}, \code{BloodPressure}, \code{SkinThickness}, \code{Insulin},
#' \code{BMI}, \code{DiabetesPedigreeFunction}, \code{Age} and \code{Outcome}.
#'
#'
"diabetes"

#' Results of a chemical analysis of wines grown in a specific area of Italy.
#'
#' Three types of wine are represented in the 178 samples,
#' with the results of 13 chemical analyses recorded for each sample.
#'
#' Import with data(wine, package = 'rattle'). Dependent variable: Type.
#' https://rdrr.io/cran/rattle.data/man/wine.html
#' http://archive.ics.uci.edu/ml/datasets/wine
#'
#' @format A data frame with 178 observations and 14 variables:
#' \code{Alcohol}, \code{Malic}, \code{Ash}, \code{Alcalinity},
#' \code{Magnesium}, \code{Phenols}, \code{Flavanoids}, \code{Nonflavanoids},
#' \code{Proanthocyanins}, \code{Color}, \code{Hue}, \code{Dilution}, \code{Proline}
#' and \code{Type} (target).
#'
"wine"

#' Red variant of the Portuguese "Vinho Verde" wine.
#'
#' Fetched from PMLB.
#' Physicochemical and quality of wine.
#'
#' @format A data frame with 1599 observations and 12 variables:
#' \code{fixed.acidity}, \code{volatile.acidity},
#' \code{citric.acid}, \code{residual.sugar}, \code{chlorides}, \code{free.sulfur.dioxide},
#' \code{total.sulfur.dioxide}, \code{density}, \code{pH}, \code{sulphates},
#' \code{alcohol} and \code{target} (quality).
#'
#' http://archive.ics.uci.edu/ml/datasets/Wine+Quality
#'
#' P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis.
#' Modeling wine preferences by data mining from physicochemical properties.
#' In Decision Support Systems, Elsevier, 47(4):547-553, 2009.
#'
#'
"wine_quality_red"

#' Galaxy dataset for regression.
#'
#' Fetched from PMLB.
#'
#' #' @format A data frame with 323 observations and 5 variables:
#' \code{eastwest}, \code{northsouth}, \code{angle}, \code{radialposition}
#' and \code{target} (velocity).
#'
#' https://www.openml.org/d/690
#'
"galaxy"


#' Data of three different species of penguins.
#'
#' Collected and made available by Dr. Kristen Gorman and the Palmer Station,
#' Antarctica LTER, a member of the Long Term Ecological Research Network.
#'
#' Fetched from https://github.com/allisonhorst/penguins.
#'
#' @format A data frame with 344 observations and 7 variables:
#' \code{species}, \code{island}, \code{culmen_length_mm}, \code{culmen_depth_mm},
#' \code{flipper_length_mm}, \code{body_mass_g} and \code{sex}.
#'
#' Gorman KB, Williams TD, Fraser WR (2014).
#' Ecological Sexual Dimorphism and Environmental Variability within a
#' Community of Antarctic Penguins (Genus Pygoscelis).
#' PLoS ONE 9(3): e90081. doi:10.1371/journal.pone.0090081
#'
"penguins"

#' Training dataset.
#' Medical information of Wuhan patients collected between
#' 2020-01-10 and 2020-02-18.
#' Containing NAs.
#'
#' @format A data frame with 375 observations and 77 variables.
#'
#' An interpretable mortality prediction model for COVID-19 patients.
#' Yan et al.
#' https://doi.org/10.1038/s42256-020-0180-7
#' https://github.com/HAIRLAB/Pre_Surv_COVID_19
#'
"train_covid"

#' External test dataset.
#' Medical information of Wuhan patients collected between
#' 2020-01-10 and 2020-02-18.
#'
#' @format A data frame with 110 observations and 7 XGBoost-selected variables:
#' \code{PATIENT_ID}, \code{Lactate dehydrogenase},
#' \code{High sensitivity C-reactive protein}, \code{(\%)lymphocyte},
#' \code{Admission time}, \code{Discharge time} and \code{outcome}.
#'
#' An interpretable mortality prediction model for COVID-19 patients.
#' Yan et al.
#' https://doi.org/10.1038/s42256-020-0180-7
#' https://github.com/HAIRLAB/Pre_Surv_COVID_19
#'
"test_covid"
