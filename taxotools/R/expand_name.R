#' Expands Scientific name
#'
#' At times the genus is specified with first character and '.' rather then
#' repeating genus names every time. These are either synonyms or species of the
#' same genus listed one bellow another. To convert these names to canonical
#' names, we need to expand the genus name (typically) using previous entry in
#' the list.
#'
#' @param fullname full scientific name
#' @param shortname scientific name with short form genus name to expand the Genus
#' @return scientific name with Genus expanded using reference name provided as parameter
#' @family Name functions
#' @examples
#' expand_name("Addax gibbosa", "A. mytilopes")
#' expand_name("Oryx addax", "O. nasomaculatus")
#'
#' @export
expand_name <- function(fullname,shortname){
  for(i in 1:length(shortname)){
    if(substr(shortname[i],2,2)=='.'){
      if(substr(shortname[i],1,1)==substr(fullname,1,1)){
        shortname[i] <- paste(strsplit(fullname," ")[[1]][1],
                              substr(shortname[i],4,nchar(shortname[i])))
      } else {
        if(i > 1) {
          if( substr(shortname[i],1,1)==substr(shortname[i-1],1,1)){
            shortname[i] <- paste(strsplit(shortname[i-1]," ")[[1]][1],
                                  substr(shortname[i],4,nchar(shortname[i])))
          }
        }
      }
    }
    species <- strsplit(shortname[i]," ")[[1]][2]
    if(!is.na(species)){
      if(substr(species,2,2)=='.'){
        fspecies <- strsplit(fullname," ")[[1]][2]
        if(substr(species,1,1)==substr(fspecies,1,1)){
          dotpos <- unlist(lapply(strsplit(shortname[i], ''), 
                                  function(x) which(x == '.')))[1]        
          shortname[i] <- paste(strsplit(fullname," ")[[1]][1], fspecies,
                                substr(shortname[i],dotpos+2,
                                       nchar(shortname[i])))
        } else {
          if(i > 1) {
            if( substr(shortname[i],1,1)==substr(shortname[i-1],1,1)){
              dotpos <- unlist(lapply(strsplit(shortname[i], ''), 
                                      function(x) which(x == '.')))[1]        
              shortname[i] <- paste(strsplit(shortname[i-1]," ")[[1]][1], 
                                    fspecies, substr(shortname[i],dotpos+2,
                                                     nchar(shortname[i])))
            }
          }
        }
      }
    }
    shortname[i] <- gsub("\\s+", " ", trimws(shortname[i]))
  }
  return(shortname)
}