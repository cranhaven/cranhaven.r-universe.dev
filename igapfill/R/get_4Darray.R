#' Assembles a 4D-array
#' 
#' This function returns a 4D-array which is an auxiliary
#' object when invoking \code{\link[igapfill]{applyGapfill}}.
#' 
#' @param listPath list, containing lists with names of files to be assembled as a 4D array.
#' @param        i numeric indicating \eqn{i}-th entry of \code{listPath} to be processed.
#' @param      lon character vector whose entries indicate longitude coordinates.
#' @param      lat character vector whose entries indicate latitude coordinates.
#' @param     days numeric vector indicating what days are being considered. Length of this
#'                 object must be equal to length of \code{listPath}.
#' @param    years integer vector indicating what years are being considered. Length of this
#'                 object must be equal to length of \code{listPath}.
#'                 
#' @export                 
#' 
#' @note This function may be useful when employing \code{\link[gapfill]{Gapfill}}
#' independently of the current package.
#' 
#' @details Each entry of \code{listPath} must contain files associated with images
#' registered during the same year. \code{lon} and \code{lat} can be obtained with the functions \code{\link[igapfill]{get_LON}}
#' and \code{\link[igapfill]{get_LAT}}, respectively. \code{days} must be provided by the user, otherwise
#' it will be set to \code{1:length(years)}.
#' 
#' @return An array of 4 dimensions: longitude, latitude, days and years
#' 
#' @seealso \code{\link[igapfill]{create_dirs}}, \code{\link[igapfill]{get_3Darray}}
#' 
get_4Darray <- function(listPath, i, lon, lat, days, years){ #, longitude, latitude, days, years){
  
  layerList <- vector(mode="list", length=length(listPath))
  
  for(k in 1:length(listPath)){
    layerList[[k]] <- get_3Darray(path=listPath[[k]][i])
  }
  
  get_array_lat_lon_day_year(listLayer=layerList, lon=lon, lat=lat, days=days, years=years)
}
