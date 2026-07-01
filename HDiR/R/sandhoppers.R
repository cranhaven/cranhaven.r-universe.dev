#' Orientation measured under natural conditions and other variables of interest for analyzing the behavioral plasticity of two sympatric sandhoppers species, Talitrus saltator and Talorchestia brito. The experiment was carried out on the exposed nontidal sand of Zouara beach located in the Tunisian northwestern coast. More details can be found in Marchetti and Scapini (2003) or Scapini et al. (2002).
#'
#' @docType data
#'
#' @usage data(sandhoppers)
#'
#' @source Authors thank Prof. Felicita Scapini for providing the sandhoppers data (collected under the support of the European Project ERB ICI8-CT98-0270).
#'
#' @format  A data frame with 1828 observations on the following 12 variables:
#' \describe{
#'  \item{angle}{Numeric vector containing the orientation angles between 0 and \eqn{2\pi}.}
#'  \item{date}{A factor where each level indicates the date when angles were measured.}
#'  \item{month}{A factor with two levels indicating the month when angles were measured. Experiments were performed in two different periods, April and October, which were chosen for the abundance of the populations, as well as for their non-extreme and changing climatic conditions.}
#'  \item{time}{A factor with three levels indicating the time when angles were measured (morning, afternoon, noon).}
#'  \item{azim}{A numeric vector indicating the sun azimuth. The sun position was confounded with the time of the day (morning: 100º–150º, noon: az=151º–210º and afternoon: az=211º–260º experiments).}
#'  \item{hour}{A factor with hours when angles were measured.}
#'  \item{species}{A factor with three levels (brito, salt, Not Determined) indicating the specie.}
#'  \item{sex}{A factor with three levels (F, M, J) indicating the sex (female, male, J).}
#'  \item{temp}{Temperature (ºC).}
#'  \item{humid}{Air relative humidity (%).}
#'  \item{land}{A factor with two levels (no, yes) indicating landscape view was either permitted or screened.}
#'  \item{trap}{A numeric vector containing the traps identifier used for capturing the sandhoppers.}
#' }
#' @references
#' \describe{
#' \item{1. }{Marchetti, G. M. and Scapini, F., Use of multiple regression models in the study of sandhopper orientation under natural conditions, Estuar. Coast Shelf S., 58, 207-215 (2003)}
#' \item{2. }{Scapini, F., Aloia, A., Bouslama, M. F., Chelazzi, L., Colombini, I., ElGtari, M., Fallaci, M. and Marchetti, G. M. Multiple regression analysis of the sources of variation in orientation of two sympatric sandhoppers, Talitrus saltator and Talorchestia brito, from an exposed Mediterranean beach, Behav. Ecol. Sociobiol., 51(5), 403-414 (2002)}
#' }
#' @examples
#' \dontrun{
#'  sandhoppers
#' }
"sandhoppers"
