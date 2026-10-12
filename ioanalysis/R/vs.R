vs <- function(io, ES, regions = "all", sectors = "all"){
  # Preliminaries
  if(!"InputOutput" %in% class(io)) stop('io should be of "InputOutput" class. See ?as.inputoutput')
  RS_label <- io$RS_label
  A <- io$A
  X <- io$X
  E <- export.total(io)
  # Grabbing the regions and sectors
  if(missing(ES)){
    location <- 0
    if("all" %in% regions & "all" %in% sectors){
      location <- 1:length(X)
      regions <- unique(RS_label[, 1])
    }
    if(0 %in% location){
      # Regions
      if("all" %in% regions){
        regions <- unique(RS_label[, 1])
      }
      else if(class(regions) == "character"){
        for(k in 1:length(regions)){
          if(!regions[k] %in% RS_label[, 1]) stop(paste(regions[k], "is not a region in RS_label. Check spelling, capitalization, and punctuation."))
        }
      }
      else if(class(regions) == "numeric" | class(regions) == "integer"){
        region <- unique(RS_label[, 1])
        regions <- region[regions]
      }
      # Sectors
      if("all" %in% sectors){
        sectors <- unique(RS_label[, 2])
      }
      else if(class(sectors) == "character"){
        for(k in 1:length(sectors)){
          if(!sectors[k] %in% RS_label[, 2]) stop(paste(sectors[k], "is not a sector in RS_label. Check spelling, capitalization, and punctuation."))
        }
      }
      else if(class(sectors) == "numeric" | class(sectors) == "integer"){
        sector <- unique(RS_label[, 2])
        sectors <- sector[sectors]
      }
      # Putting it together
      location <- which(RS_label[, 1] %in% regions & RS_label[, 2] %in% sectors)
    }
  } else if(!missing(ES)){
    location <- as.numeric(ES[,1])
    regions <- unique(ES[,2])
  }


  ########################
  ## Calculating the vs ##
  ########################
  vs <- vector("list", length(regions))
  names(vs) <- regions
  for(r in 1:length(regions)){
    i <- which(RS_label[,1] %in% regions[r])
    export <- E[i,1]
    E.tot <- sum(export)
    gap <- i[1] - 1
    i <- intersect(location, i)
    sector <- RS_label[i, 2]
    M.coef <- import.coef(io, region = r)
    i <- i - gap
    L.r <- leontief.inv(io, regions = regions[r])
    vs.r <- M.coef %*% L.r %*% export / E.tot
    vs.r <- matrix(vs.r[i,1])
    colnames(vs.r) <- "vs"
    rownames(vs.r) <- sector
    vs[[r]] <- vs.r
  }
#  if(length(vs) == 1){
#    return(vs[[1]])
#  } else{
#    return(vs)
#  }
  vs
}


















