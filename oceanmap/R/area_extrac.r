area_extrac <- function(obj,area){
  files <- Sys.glob(obj)
  files <- files[grepl('.gz', files)] # get sure that only \code{'.gz'}-files are treated
  cat(paste0("\nGoing to extract region '",area,"' in ", length(files), " files"))
  file.names <- name_split(files)
  file.names$area <- area
  
  i <- 1
  for(i in 1:length(files)){
    f <- files[i]
    out <- readbin(f,area)
    
    opt <- file.names$option[i]
    file.names$option[i] <- paste0(opt,'.extrac')
    file_out <- name_join(file.names[i,])
    writebin(out,filename=file_out,param=file.names$parameter[i])
    cat(paste0("\nnew area extracted in file: '", file_out,"'"))
  }
}