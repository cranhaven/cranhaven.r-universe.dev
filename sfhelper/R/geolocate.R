#' Geolocate historical toponyms
#'
#' This function uses the API for the World Historical Gazeteer to geolocate place names.
#' It takes the place name and region as inputs and returns a data frame with results,
#' primarily the longitude and latitude.
#' @return A data frame of the geospatial data
#' @param df A data frame with two columns, places names and two-letter ISO codes for regions
#' @param place A column with toponyms in the data frame
#' @param iso A column of ISO codes for regions
#' @import rjson
#' @import RCurl
#' @export geolocate
#' @keywords sf
#' @keywords spatial
#' @keywords map
#' @examples
#' # Search for Paris and Edo (now Tokyo)
#' example.df <- data.frame("place" = c("Paris","Edo"), "iso" = c("FR","JP"))
#' geolocate(example.df)
geolocate <- function(df,place="place",iso="iso"){
  base_df <- data.frame("toponym"=NA,"codes"=NA,"long"=NA,"lat"=NA)
  for(i in 1:nrow(df)){
    result <- RCurl::getURL(paste0("https://whgazetteer.org/api/index/?name=",get("place",df)[i],"&ccode=",get("iso",df)[i]),.encoding = "UTF-8",
                            .opts = list(ssl.verifypeer = FALSE))
    result.list <- rjson::fromJSON(result)
    
    if(length(result.list$features)==0){coordinates.df = data.frame("toponym"=get("place",df)[i],"codes"=get("iso",df)[i],"long"=NA,"lat"=NA)}else{
      coordinates <- sapply(1:length(result.list$features), FUN = function(x) result.list$features[[x]]$geometry$coordinates)
      coordinates.df <- data.frame(matrix(unlist(coordinates),byrow = TRUE, ncol = 2))
      # coordinates.df
      colnames(coordinates.df) <- c("long","lat")
      titles <- sapply(1:nrow(coordinates.df), FUN = function(x) result.list$features[[x]]$properties$title)
      coordinates.df$toponym <- titles
      # coordinates.df$toponym
      codes <- sapply(1:nrow(coordinates.df), FUN = function(x) result.list$features[[x]]$properties$ccodes)
      coordinates.df$codes <- codes
      # coordinates.df
      coordinates.df <- coordinates.df[,c(3,4,1,2)]
      # coordinates.df
    }
    base_df <-rbind(base_df,coordinates.df)
  }
  base_df <- base_df[-1,]
  return(base_df)
}
