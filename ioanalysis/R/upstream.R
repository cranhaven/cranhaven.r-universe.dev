upstream <- function(io, ES, regions = "all", sectors = "all"){
  # Preliminaries
  if(!"InputOutput" %in% class(io)) stop('io should be of "InputOutput" class. See ?as.inputoutput')
  RS_label <- io$RS_label
  Z <- io$Z
  A <- io$A
  X <- io$X
  E <- export.total(io)
  M <- import.total(io)
  f_label <- io$f_label
  # Adding intermediate exports and imports
  region <- unique(RS_label[, 1])
  for(r in 1:length(region)){

  }
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
  ###########################
  ## Ladies and Gentlemen! ##
  ###########################
  up <- vector("list", length(regions))
  names(up) <- regions
  n <- dim(A)[1]
  d <- (X + E - M)
  d[which(d == 0)] <- 1
  u <- matrix(X/ d, ncol = n, nrow = n, byrow = TRUE)
  D <- A * u
  n <- dim(D)[1]
  I <- diag(n)
  duration <- round(8e-10 * n^3/60, 2)
  if(duration >= 0.1){
    print(paste("Calculating the upstream inverse. Should take roughtly", duration, "minutes."))
  }
  U <- solve(I - D, tol = 1e-100)
  one <- matrix(1, nrow = n)
  U <- U %*% one
  for(r in 1:length(regions)){
    i <- which(RS_label[, 1] %in% regions[r])
    i <- intersect(i, location)
    sector <- RS_label[i,2]
    u <- matrix(U[i])
    rownames(u) <- sector
    colnames(u) <- regions[r]
    up[[r]] <- u
  }
  if(length(up) == 1){
    return(up[[1]])
  } else{
    return(up)
  }
}
