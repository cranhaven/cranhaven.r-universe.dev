#' Dog's sample data from Pinhais, Brazil, 2017
#'
#' Data described and analyzed by Baquero et al., 2018.
#'
#' @format Data frame with 1252 observations (dogs) and 22 variables:
#' \describe{
#'   \item{interview_id}{Interview's ID.}
#'   \item{census_tract_id}{Census tract's ID.}
#'   \item{name}{Dog's name.}
#'   \item{species}{Animal's species}
#'   \item{sex}{Dog's sex.}
#'   \item{age}{Dog's age. An age equal to 0 means that the dog had less than 1 year.}
#'   \item{sterilized}{Dog's reproductive status.}
#'   \item{sterilized_ly}{For sterilized dogs, indicates if the dog was sterilized during the last year.}
#'   \item{go_out_on_the_street_alone}{Indicates if the dog had access to the street without supervision (free-roaming).}
#'   \item{acquisition}{Acquisition type.}
#'   \item{acquired_ly}{Indicates if the dog was acquired during the last year.}
#'   \item{acquired_sterilized}{Indicates if the dog was sterilized when acquired.}
#'   \item{acquisition_city}{City of acquisition.}
#'   \item{acquisition_state}{State of acquisition.}
#'   \item{lost_animals}{Indicates if the dog was acquired during the year following the lost of another dog}
#'   \item{births_ly}{Litter size if the bitch had the litter during the last year.}
#'   \item{name3}{Dog's name (dogs not present anymore in the household).}
#'   \item{species3}{Animal's species (animals not present anymore in the household).}
#'   \item{sex3}{Dog's sex (dogs not present anymore in the household).}
#'   \item{age3}{Dog's age (dogs not present anymore in the household). An age equal to 0 means that the dog had less than 1 year.}
#'   \item{sterilized3}{Dog's reproductive status (dogs not present anymore in the household).}
#'   \item{fate3}{Dog's fate.}
#' }
#' @references Baquero, O. S., Marconcin, S., Rocha, A., & Garcia, R. D. C. M. (2018). Companion animal demography and population management in Pinhais, Brazil. Preventive Veterinary Medicine.
"dogs"