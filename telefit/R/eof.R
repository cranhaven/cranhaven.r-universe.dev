#' Performs an EOF decomposition of the data
#' 
#' Uses the stats::prcomp function to implement EOF decompositions of data
#' 
#' @export
#' 
#' @importFrom stats prcomp
#' 
#' @param X [variable x observation] matrix of data for which to compute EOFs
#' @param center TRUE/FALSE to center columns of X in call to prcomp
#' @param scale TRUE/FALSE to scale columns of X in call to prcomp
#' 
#' @return A list containing EOF patterns as columns, and their scores 
#' 
#' @examples
#' data("coprecip")
#' attach(coprecip)
#' 
#' # compute ocean surface temperature eofs
#' eofs = eof(Z)
#' 
#' # view first EOF, which corresponds to the El-Nino pattern
#' coords.r.mod = coords.r
#' coords.r.mod[,1][coords.r.mod[,1]>0] =
#'   coords.r.mod[,1][coords.r.mod[,1]>0] - 360
#' fields::quilt.plot(coords.r.mod, eofs$patterns[,1])
#' 
#' # alternatively, the plot.stData function can directly compute and plot EOFs
#' plot(coprecip, type='eof', pattern=1)
#' 

eof = function(X, center = F, scale = F) {
  e = prcomp(X, center = center, scale. = scale)
  
  dimnames(e$rotation)[[2]] = 1:ncol(X)
  
  list(patterns = -e$x,
       scores = -e$rotation,
       sd = e$sdev)
}