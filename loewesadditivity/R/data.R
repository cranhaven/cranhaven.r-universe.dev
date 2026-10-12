

#' RH5 and AMA1RON2

#' @description The data is the raw data for a combination dose of RH5 and AMA1RON2.  The data was collected by PEOPLE and on DATE on this GRANT.
#' @format a 38 x 15 data set where the columns are of the following format
#' \describe{
#' \item{well}{ one of iRBC (the max), uRBC (the min), RPMI (??), or comb (which is short for combination)}
#' \item{AMA1RON2}{dose of AMA1RON2 in mg/mL}
#' \item{RH5}{dose of RH5 in mg/mL}
#' \item{exp{x}{y}rep{z}}{the results from experiment x, sub item y, repetition z}
#' }
#' @examples
#' data("rh5_ama1ron2")
#' head(rh5_ama1ron2)
"rh5_ama1ron2"


#' CyRPA and RIPR

#' @description The data is the raw data for a combination dose of CyRPA and RIPR.
#' \describe{
#' \item{well}{ one of iRBC (the max), uRBC (the min), RPMI (??), or comb (which is short for combination)}
#' \item{RIPR}{dose of RIPR in mg/mL}
#' \item{CyRPA}{dose of CyRPA in mg/mL}
#' \item{exp{x}{y}rep{z}}{the results from experiment x, sub item y, repetition z}
#' }
#' @examples
#' data("cyrpa_ripr")
#' head(cyrpa_ripr)
"cyrpa_ripr"



#' RH5 and RH4

#' @description The data is the raw data for a combination dose of RH5 and RH4.  The data was originally published in Williams et al. (2018).
#' @format a 48 x 3 data set where the columns are of the following format
#' \describe{
#' \item{RH4}{dose of RH4 in mg/mL}
#' \item{RH5}{dose of RH5 in mg/mL}
#' \item{GIA}{Percent Growth inhibition assay averaged over two experiments}
#' }
#' @examples
#' data("rh5_rh4")
#' head(rh5_rh4)
"rh5_rh4"
