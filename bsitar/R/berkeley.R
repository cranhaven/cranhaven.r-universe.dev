

#' @title Berkeley Child Guidance Study Data
#'
#' @description The Berkeley Child Guidance Study data set containing
#'   longitudinal growth data for 136 children from birth to 21 years.
#'
#' @details Data originally provided as an appendix to the book by
#'   \insertCite{Tuddenham1954;textual}{bsitar} and later used in the
#'   \pkg{sitar} \insertCite{R-sitar}{bsitar} package after correcting the
#'   transcription errors. 
#'
#'   A detailed description of the data including the frequency of measurements
#'   per year is provided in the \pkg{sitar} package.
#'   \insertCite{R-sitar}{bsitar}. Briefly, the data comprise of repeated growth
#'   measurements made on 66 boys and 70 girls (birth to 21 years). Children
#'   were born in 1928-29 (Berkeley, California) and were of north European
#'   ancestry. Measurements were made at the following ages: 0 (i.e, at birth),
#'   0.085 year, 0.25 to 2 years (every 3 month), 2 to 8 years (annually), and 8
#'   to 21 years (6-monthly). The children were measured for height, weight
#'   (undressed), stem length, biacromial diameter, bi-iliac diameter, leg
#'   circumference, and dynamo metric strength.
#' 
#' @name berkeley
#' @docType data
#' @format A data frame with 4884 observations on the following 10 variables:
#' \describe{
#' \item{id}{factor with levels 201-278 for males, and 301-385 for females}
#' \item{age}{years, numeric vector}
#' \item{height}{cm, numeric vector}
#' \item{weight}{kg, numeric vector}
#' \item{stem.length}{cm, numeric vector}
#' \item{bi.acromial}{cm, numeric vector}
#' \item{bi.iliac}{cm, numeric vector}
#' \item{leg.circ}{cm, numeric vector}
#' \item{strength}{lb, numeric vector}
#' \item{sex}{factor with level 1 male and level 2 female}
#' }
#' @references
#'  \insertAllCited{}
#'  
#' @keywords datasets
#' 
#' @return A data frame with 10 columns.
#' @author Satpal Sandhu  \email{satpal.sandhu@bristol.ac.uk}
"berkeley"