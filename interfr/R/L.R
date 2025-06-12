

#' Computes the degree of transmission under crossed polarizer and analyzer 
#'@author Olivier Eterradossi, \email{olivier.eterradossi@mines-ales.fr}
#' @param lambda Wavelength (nanometers).
#' @param d Thickness (micrometers).
#' @param biref Birefringence .
#' @description
#' \code{L} is called internally to fill the spectral transmission matrix
#' needed for interference color calculation
#' @return a single transmission value
#' @importFrom CircStats rad
#' @references
#' Bloss, F.D. (1999) Optical Cristallography. Mineralogical Society of America
#' Monograph Series, Washington DC, Publication #5. ISBN 0-939950-49-9
#' @examples
#'\dontrun{
#' test.L<-L(lambda=550,d=30,biref=0.00025)
#'}


L<-function(lambda=550,d=30,biref){
  d<-d*1000 # passage en nm
  TT<-d*biref
  L<-sin(CircStats::rad(180)*TT/lambda)^2
}

