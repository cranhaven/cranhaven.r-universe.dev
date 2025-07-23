.check.folder <- function(folder){
  if(nchar(folder > 1) & substr(folder,nchar(folder),nchar(folder)) != '/') folder <- paste0(folder,'/')
    return(folder)
}