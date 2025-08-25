#' @title Available Boundary Scales for 'nomis' Table
#' 
#' @description 
#' Retrieve available spatial scales for a given 'nomis' table id. 
#' This is useful as each table only has data at a specific set of scales, 
#' e.g. many census tables are available at MSOA and LSOA resolutions but not at Regional level.
#' 
#' @importFrom magrittr %>%
#' @importFrom methods is
#' 
#' @param id A valid 'nomis' table id given as a string.
#' @param base_url Nomis API base url
#' 
#' @examples 
#' get_available_scales(id="NM_1003_1")
#' 
#' @return A tidy dataframe listing the geographical scales available for the 'nomis' table selected. 
#' 
#' @export
 
get_available_scales <- function(id, base_url = "https://www.nomisweb.co.uk/api/v01") {
  
  is_nested <- function(l) {
    stopifnot(is.list(l))
    for (i in l) {
      if (is.list(i))
        return(TRUE)
    }
    return(FALSE)
  }
   
  x <- httr::GET(paste0(base_url,
                        "/dataset/",
                        id,
                        ".overview.json")) %>%
    httr::content()
  
  geography_index <- NULL
  #Loop through dimensions to extract all different geography codes (i.e. boundary scales which the tables are compatiblke with)
  for (i in c(1:length(x$overview$dimensions$dimension))) {
    if (!(is.null(x$overview$dimensions$dimension[[i]]$make$part$name))) {
      if (x$overview$dimensions$dimension[[i]]$make$part$name == "Geography") {
        geography_index <- i
        
      }
    }
  }
  
  if (is.null(geography_index)) {
    y <- "National"
  } else{

    
    if (!is.null(x$overview$dimensions$dimension[[geography_index]]$types$type)) {
      if (is_nested(x$overview$dimensions$dimension[[geography_index]]$types$type)) {
        y_stg <-
          x$overview$dimensions$dimension[[geography_index]]$types$type
          y <- do.call(rbind.data.frame, y_stg) 
      } else{
        y <-
          x$overview$dimensions$dimension[[geography_index]]$types$type
        
      }
    } else{
      y <- "National"
    }
  }
  
  if(is(y,"character")){
    y <- as.data.frame(y)
  }
  return(y)
}

