#' Match and set geometry for a data frame
#'
#' This function sets the geometry in a target data frame based on matching values in a source data frame 
#' @return A sf data frame
#' @param source An sf data frame with a geometry column
#' @param target A data frame
#' @param match_field A column name (in quotes) for matching, shared by both data frames
#' @import sf
#' @export st_match_geometry
#' @keywords sf
#' @keywords spatial
#' @keywords map
#' @keywords geometry
#' @examples
#' a <- sf::st_polygon(list(rbind(c(-90,40),c(-90,50),c(-95,50),c(-95,40),c(-90,40))))
#' b <- sf::st_polygon(list(rbind(c(-80,30),c(-80,20),c(-70,20),c(-70,30),c(-80,30))))
#' ab <- sf::st_sfc(a,b)
#' sf::st_crs(ab) <- 4326
#' source.sf <- data.frame("match_field"=c("A","B"))
#' sf::st_geometry(source.sf) <- ab
#' target.df <- data.frame("match_field"=c("A","A","B","C"))

st_match_geometry <- function(source,target,match_field)
{
  target$geometry <- NA
  for(i in 1:nrow(target)){
    temp.df <- source[which(source[[match_field]]==target[[match_field]][i]),]
    if(nrow(temp.df)>0)
    {
      sf::st_geometry(target[i,]) <- temp.df$geometry}
  }
  target <- sf::st_as_sf(target)
  return(target)
}
