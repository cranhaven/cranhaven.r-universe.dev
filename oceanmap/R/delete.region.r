delete.region <- function(region,lib.folder,restore=F){
  
  if(missing(lib.folder)) lib.folder <- .libPaths()
  lib.folder.oceanmap <- paste0(lib.folder,"/oceanmap")
  lib.folder.oceanmap <- lib.folder.oceanmap[file.exists(lib.folder.oceanmap)]
  if(length(lib.folder.oceanmap) == 0) stop("error in add.region: could not find oceanmap package. please check library path.")
  if(length(lib.folder.oceanmap) > 1){
    Rcheck <- grepl('oceanmap.Rcheck', lib.folder.oceanmap)
    if(any(Rcheck)){
      lib.folder.oceanmap <- lib.folder.oceanmap[which(Rcheck)]
    }else{
      stop("error in add.region: oceanmap package found in multiple R libraries. please define lib.folder.")
    }
  }
  region_definitions.path <- paste0(lib.folder.oceanmap,"/data/region_definitions.RData")
  file.exists(region_definitions.path)
  
  if(restore){
    cat(paste0("\nGoing to restore original region_definitions-file"))
    enter <- readline("\nPress <Enter> to continue")
    if(enter != "") stop("Operation stopped by user")
    env <- new.env()
    
    region_definitions.bkp <- NULL
    rm(region_definitions.bkp)
    data('region_definitions.bkp',envir=environment())    
    region_definitions <- region_definitions.bkp
  }else{
    region_definitions <- NULL
    rm(region_definitions)
    data('region_definitions',envir=environment())
    
    id <- which(region_definitions$label == region)
    if (length(id) < 1) stop("error in delete.region: selected region not found! please select valid region label:\n",paste(paste(region_definitions[,1],"\t",region_definitions[,2]),collapse='\n'))
    cat(paste0("\nGoing to delete region '",region,"':\n"))
    print(region_definitions[id,])
    enter <- readline("\nPress <Enter> to continue")
    if(enter != "") stop("Operation stopped by user")
    region_definitions <- region_definitions[-id,]
  }
  save(region_definitions,file=region_definitions.path)
}