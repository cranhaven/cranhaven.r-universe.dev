#' Occupancy data of 5 bird species in Micronesia.
#'
#' A dataset containing the detections history of 5 species in the
#' Pohnpei Island for 4 consecutive days (Columns) in 120 different sites (Rows).
#' \describe{
#'   \item{CICA.1}{Detection history of \emph{Coracina tenurostris}: on day 1}
#'   \item{CICA.2}{Detection history of \emph{Coracina tenurostris}: on day 2}
#'   \item{CICA.3}{Detection history of \emph{Coracina tenurostris}: on day 3}
#'   \item{CICA.4}{Detection history of \emph{Coracina tenurostris}: on day 4}
#'   \item{CIRW.1}{Detection history of \emph{Acrocephalus syrinx}: on day 1}
#'   \item{CIRW.2}{Detection history of \emph{Acrocephalus syrinx}: on day 2}
#'   \item{CIRW.3}{Detection history of \emph{Acrocephalus syrinx}: on day 3}
#'   \item{CIRW.4}{Detection history of \emph{Acrocephalus syrinx}: on day 4}
#'   \item{CIWE.1}{Detection history of \emph{Zosterops semperi}: on day 1}
#'   \item{CIWE.2}{Detection history of \emph{Zosterops semperi}: on day 2}
#'   \item{CIWE.3}{Detection history of \emph{Zosterops semperi}: on day 3}
#'   \item{CIWE.4}{Detection history of \emph{Zosterops semperi}: on day 4}
#'   \item{LBWE.1}{Detection history of \emph{Rukia longirostra}: on day 1}
#'   \item{LBWE.2}{Detection history of \emph{Rukia longirostra}: on day 2}
#'   \item{LBWE.3}{Detection history of \emph{Rukia longirostra}: on day 3}
#'   \item{LBWE.4}{Detection history of \emph{Rukia longirostra}: on day 4}
#'   \item{MIPI.1}{Detection history of \emph{Ducula oceanica}: on day 1}
#'   \item{MIPI.2}{Detection history of \emph{Ducula oceanica}: on day 2}
#'   \item{MIPI.3}{Detection history of \emph{Ducula oceanica}: on day 3}
#'   \item{MIPI.4}{Detection history of \emph{Ducula oceanica}: on day 4}
#' }
#' @format A data frame with 120 rows and 20 variables
#' @seealso \code{\link[DiversityOccupancy]{Daily_Cov}}
#' @seealso \code{\link[DiversityOccupancy]{siteCov}}
#' @references Oleiro P. 2014. Avian population responses to anthropogenic
#' landscape changes in Pohnpei, Federated Stats of Micronesia. Masters Thesis
#' University of Missouri.

"IslandBirds"

#' Nine covariables measured at 120 survey stations in an island in micronesia
#'
#' A dataframe containing the measurements of 5 variables (columns) correspondant
#' to 120 different survey stations (Rows) in Pohnpei Island where the detection
#' histories of the IslandBirds where taken.
#' \describe{
#'   \item{Elev}{Metres above sea level of the sampled point}
#'   \item{AgroFo}{Proportion of a station (from 0 to 1) corresponding to
#'   agricultural forest incudes areas withsubsistence or commercial staple
#'   crops mixed with undisturbed forest}
#'   \item{SecVec}{Proportion of a station (from 0 to 1) corresponding to
#'   secondary vegetation which represents anthropogenic habitats (e.g.,
#'   savannah) and early colonizers and invasive species (e.g. \emph{Hibiscus spp.}
#'    and \emph{Merremia peltata)}}
#'   \item{Wetland}{Proportion of a station (from 0 to 1) corresponding to areas
#'   classified as mangroves, marshes, swamp forest and fresh water bodies}
#'   \item{Upland}{Proportion of a station (from 0 to 1) corresponding to upland
#'   and undisturbed forest habitats}
#' }
#' @format A data frame with 120 rows and 52 variables:
#' @seealso \code{\link[DiversityOccupancy]{Daily_Cov}}
#' @seealso \code{\link[DiversityOccupancy]{IslandBirds}}
#' @references Oleiro P. 2014. Avian population responses to anthropogenic
#' landscape changes in Pohnpei, Federated Stats of Micronesia. Masters Thesis
#' University of Missouri.

"siteCov"

#' Seven covariables measured at 120 survey stations in an island in micronesia
#'
#' A list containing the measurements of seven variables correspondant to 120
#' different survey stations (Rows) in Pohnpei Island where the detection
#' histories of the IslandBirds where taken.
#'
#' \describe{
#'   \item{Day}{Ordinal day}
#'   \item{Wind}{Wind intensity using a modified Beafourt scale}
#'   \item{Obs}{First initial of the observer}
#'   \item{Time}{Survey time (minutes after sunrise)}
#'   \item{Rain}{Categorical variable where 0 = no rain, 1 = light rain and
#'   2 = heavy rain}
#'   \item{Noise}{Ambient noise (1-10 being 10 the loudest)}
#'   \item{Clouds}{Cloud cover in percentage}
#' }
#' @format A list with 6 elements, each of them with a data frame
#' @seealso \code{\link[DiversityOccupancy]{IslandBirds}}
#' @seealso \code{\link[DiversityOccupancy]{siteCov}}
#' @references Oleiro P. 2014. Avian population responses to anthropogenic
#' landscape changes in Pohnpei, Federated Stats of Micronesia. Masters Thesis
#' University of Missouri.

"Daily_Cov"


#' Raster Stack with five variables measured in Pohnpei Island in Micronesia
#'
#' A Raster Stack containing five rasters from in the  Pohnpei Island in Micronesia
#' where the detection histories of the IslandBirds where taken.
#' \describe{
#'   \item{Elev}{Metres above sea level of the sampled point}
#'   \item{AgroFo}{Proportion of a station (from 0 to 1) corresponding to
#'   agricultural forest incudes areas withsubsistence or commercial staple
#'   crops mixed with undisturbed forest}
#'   \item{SecVec}{Proportion of a station (from 0 to 1) corresponding to
#'   secondary vegetation which represents anthropogenic habitats (e.g.,
#'   savannah) and early colonizers and invasive species (e.g. \emph{Hibiscus spp.}
#'    and \emph{Merremia peltata)}}
#'   \item{Wetland}{Proportion of a station (from 0 to 1) corresponding to areas
#'   classified as mangroves, marshes, swamp forest and fresh water bodies}
#'   \item{Upland}{Proportion of a station (from 0 to 1) corresponding to upland
#'   and undisturbed forest habitats}
#' }
#' @format A raster stack with 5 layers:
#' @seealso \code{\link[DiversityOccupancy]{IslandBirds}}
#' @seealso \code{\link[DiversityOccupancy]{Daily_Cov}}
#' @seealso \code{\link[DiversityOccupancy]{siteCov}}

"Birdstack"
