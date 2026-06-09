#' @name chm_chablais3
#'
#' @title Canopy height model (Chablais 3 plot)
#'
#' @description Canopy height model computed from airborne laser scanning data acquired in July 2010.
#'
#' @docType data
#'
#' @usage data(chm_chablais3)
#'
#' @format A PackedSpatRaster object
#'
#' @keywords datasets
#'
#' @references Monnet, J.-M. 2011. Using airborne laser scanning for mountain forests mapping: Support vector regression for stand parameters estimation and unsupervised training for treetop detection. Ph.D. thesis. University of Grenoble, France. pp. 21-22 & 34 \url{https://theses.hal.science/tel-00652698/document}
#'
#' @examples
#' data(chm_chablais3)
#' chm_chablais3 <- terra::rast(chm_chablais3)
#' terra::plot(chm_chablais3)
NULL
"chm_chablais3"
