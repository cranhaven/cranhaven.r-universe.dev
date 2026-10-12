agg.sector <- function(io, sectors, newname = "newname"){
  # Preliminaries
  if(!"InputOutput" %in% class(io)) stop('io should be of "InputOutput" class. See ?as.inputoutput')
  if(missing(sectors)) stop("Please specify which sectors you wish to aggregate")
  if(missing(newname)) stop("Please specify the new name for the aggregated sectors.")
  RS_label <- io$RS_label
  # Grabbing sectors and regions
  # Regions
  regions <- unique(RS_label[, 1])
  # Sectors
  if(class(sectors) == "factor"){
    sectors <- as.character(sectors)
  }
  if(class(sectors) == "character"){
    if('all' %in% sectors){
      sectors = unique(RS_label[, 2])
    } else {
      for(k in 1:length(sectors)){
        if(!sectors[k] %in% RS_label[, 2]) stop(paste(sectors[k], "is not a sector in RS_label. Check spelling, capitalization, and punctuation."))
      }
    }
  }
  else if(class(sectors) == "numeric" | class(sectors) == "integer"){
    sector <- unique(RS_label[, 2])
    sectors <- sector[sectors]
  }
  # Creating the aggregation matrix
  n <- dim(RS_label)[1]
  I <- diag(n)
  # Making it the right size
  for(r in 1:length(regions)){
    i <- which(RS_label[, 1] %in% regions[r] & RS_label[, 2] %in% sectors)
    if(length(i) == 0){next}
    temp <- matrix(I[i[-1], ], ncol = n)
    one <- matrix(1, ncol = dim(temp)[1], nrow = 1)
    temp <- one %*% temp
    I[i[1], ] <- I[i[1], ] + temp
  }
  # Adjusting the labels
  RS_label[which(RS_label[,2] %in% sectors[1]), 2] <- newname
  sectors <- sectors[-1]
  i <- which(RS_label[, 2] %in% sectors)
  RS_label <- RS_label[-i, ]
  S <- I[-i, ]
  if('numeric' %in% class(S)){ # Allowing for aggregating all sectors
    S = matrix(c(S), nrow = 1)
  }
    # creating a new InputOuput object
  if(dim(S)[2] > 1500){
    cat("\n\n    Calculating new InputOuput object...\n\n")
  }
  n <- dim(S)[1]
  IO <- NULL
  IO$Z <- S %*% io$Z %*% t(S)
  IO$RS_label <- RS_label
  # Checking to see if the final demand is aggregated
  IO$f <- S %*% io$f
  
  IO$f_label <- matrix(io$f_label, nrow = 2)
  if(!is.null(io$E)){
    IO$E <- S %*% io$E
    IO$E_label <- matrix(io$E_label, nrow = 2)
  }
  IO$X <- S %*% io$X
  if(!is.null(io$V)){
    IO$V <- io$V %*% t(S)
    IO$V_label <- io$V_label
  }
  if('M' %in% names(io)){
    IO$M <- io$M %*% t(S)
    IO$M_label <- io$M_label
  }
  if(!is.null(io$fV)){
    IO$fV <- io$fV
    IO$fV_label <- io$fV_label
  }
  xhat <- diag(c(1/IO$X))
  if(dim(IO$X)[1] == 1){ # Allowing for aggregating all sectors
    xhat = matrix(1/IO$X)
    IO$Z = matrix(IO$Z)
  }
  IO$A <- IO$Z %*% xhat
  IO$B <- xhat %*% IO$Z
  IO$L <- leontief.inv(A = IO$A)
  IO$G <- ghosh.inv(B = IO$B)
  class(IO) <- "InputOutput"
  IO
}











