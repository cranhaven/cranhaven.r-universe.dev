#' Estimated template spectrum for the star 51 Pegasi (HD 217014)
#'
#' A small portion of the estimated template produced with the method of
#' \href{https://arxiv.org/abs/2005.14083}{Holzer et. al (2020)} on a set 
#' of 55 observed spectra from EXPRES.
#'
#' @format A data frame with 1893 rows and 2 variables:
#' \describe{
#'   \item{Wavelength}{the wavelength of the spectrum, in Angstroms}
#'   \item{Flux}{normalized flux of the spectrum, unitless}
#'   ...
#' }
#' @source \url{https://arxiv.org/abs/2003.08851}
"template"

#' Observed spectra for the star 51 Pegasi (HD 217014)
#'
#' 56 observed spectra as collected by EXPRES \strong{Petersburg et. al (2020)}.
#' Only the subset of the spectrum between 5000 and 5005 Angstroms is given here.
#'
#' @format A list with 56 elements, each of which has 2 variables:
#' \describe{
#'   \item{Wavelength}{the wavelength of the spectrum, in Angstroms}
#'   \item{Flux}{normalized flux of the spectrum, unitless}
#'   ...
#' }
#' @source \url{https://arxiv.org/abs/2003.08851}
"spectra"

#' Observed spectrum for the star 51 Pegasi (HD 217014)
#'
#' A small portion of one observed spectrum collected by EXPRES
#' \strong{Petersburg et. al (2020)}.
#'
#' @format A dataframe with 628 rows and the following 3 columns:
#' \describe{
#' \item{Wavelength}{the wavelength of the spectrum, in Angstroms}
#' \item{Flux}{normalized flux of the spectrum, unitless}
#' \item{Uncertainty}{the uncertainty of the flux measurements, unitless}
#' ...
#' }
#' @source \url{https://arxiv.org/abs/2003.08851}
"observed_spec"
