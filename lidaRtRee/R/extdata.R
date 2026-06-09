#' las data in France (Chablais 3 plot)
#'
#' @description Airborne laser scanning data over the Chablais 3 plot, acquired in 2009 by Sintegra, copyright INRAE
#' 
#' @details Additional information about the data
#' \itemize{
#' \item{Sensor: RIEGL LMS-Q560}
#' \item{EPSG code of coordinates system: 2154}
#'}
#'
#' @docType data
#' @format A compressed LAS file
#' @keywords datasets
#' @references Monnet, J.-M. 2011. Using airborne laser scanning for mountain forests mapping: Support vector regression for stand parameters estimation and unsupervised training for treetop detection. Ph.D. thesis. University of Grenoble, France. pp. 21-22. \url{https://theses.hal.science/tel-00652698/document}
#' @source Monnet J.-M. INRAE
#' @seealso \code{\link{chm_chablais3}}, \code{\link{tree_inventory_chablais3}}
#' @examples
#' LASfile <- system.file("extdata", "las_chablais3.laz", package="lidaRtRee")
#' las_chablais3 <- lidR::readLAS(LASfile)
#' las_chablais3
#' @name las_chablais3
NULL
"las_chablais3"
