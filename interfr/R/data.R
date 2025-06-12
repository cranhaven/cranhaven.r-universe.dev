
#' sample interference dataset
#'@author Olivier Eterradossi, \email{olivier.eterradossi@mines-ales.fr}
#' @description
#' Low-resolution computed interference data
#' @details
#' Low-resolution data that would be obtained by running the
#' \code{InterferenceTable} function with
#' thickVect = \code{seq(0.01,50,length.out = 50)} micrometers  and
#' birefVect = \code{seq(2e-4,1e-2,by = 5e-4)}  
#' @format  IC1 is a data frame with 10000 cases (rows) and 6 variables
#' (columns) named thickness, biref, R, G, B and retardation.
"IC1"
#' @examples
#'\dontrun{
#' head(IC1)
#' PlotChart(IC=IC1,type="RS")
#' PlotChart(IC=IC1,type="ML")
#' PlotChart(IC=IC1,type="ML",radials=TRUE)
#'}

