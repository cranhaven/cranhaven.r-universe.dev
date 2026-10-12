agg.region <- function(io, regions, newname = "newname"){
  # Preliminaries
  if(!"InputOutput" %in% class(io)) stop('io should be of "InputOutput" class. See ?as.inputoutput')
  if(missing(regions)) stop("Please specify which regions you wish to aggregate")
  if(missing(newname)) stop("Please specify the newname for the aggregated sectors.")
  RS_label <- io$RS_label
  f_label <- io$f_label
  # Checking to see if the final demand is aggregated
  check <- 0         # NOT aggregated
  if(length(unique(f_label[1,])) == 1){
    check <- 1       # IS aggregated
  }
  # Checking for valid regions
  if(class(regions) == "factor"){
    regions <- as.character(regions)
  }
  if(class(regions) == "character"){
    if('all' %in% regions){
      regions = unique(RS_label[,1])
    } else {
      for(k in 1:length(regions)){
        if(!regions[k] %in% RS_label[, 1]) stop(paste(regions[k], "is not a region in RS_label. Check spelling, capitalization, and punctuation."))
      }
    }
  }
  else if(class(regions) == "numeric" | class(regions) == "integer"){
    region <- unique(RS_label[, 1])
    regions <- region[regions]
  }
  # Checking to each region for aggregation has the same regions
  sectors <- unique(RS_label[which(RS_label[, 1] %in% regions), 2])
  if(check.RS(io) == FALSE){
    stop("Each region must have the same sectors in the same order in order to aggregate the regions. See ?check.RS")
  }
  # Aggregating regions
  n <- dim(RS_label)[1]
  I <- diag(n)
  sectors <- unique(RS_label[, 2])
  for(s in 1:length(sectors)){
    i <- which(RS_label[, 1] %in% regions & RS_label[, 2] %in% sectors[s])
    temp <- matrix(I[i[-1], ], ncol = n)
    one <- matrix(1, ncol = dim(temp)[1], nrow = 1)
    temp <- one %*% temp
    I[i[1], ] <- I[i[1], ] + temp
  }
  # Adjusting the labels
  RS_label[which(RS_label[,1] %in% regions[1]), 1] <- newname
  if(check == 0){ # f_label
    m = dim(f_label)[2]
    I.f = diag(m)
    j = which(f_label[1, ] == regions[1])
    R = length(regions)
    for(r in 2:R){
      k = which(f_label[1,] == regions[r])
      I.f[, j] = I.f[, j] + I.f[, k]
    }
    k = which(f_label[1,] %in% regions[-1])
    I.f = I.f[, -k]
    io$f <- io$f %*% I.f
    f_label[1, which(f_label[1,] %in% regions[1])] <- newname
  }
  if('E' %in% names(io)){ # E_label
    E_label <- io$E_label
    if(length(unique(E_label[1,])) > 1){
      m = dim(E_label)[2]
      I.E = diag(m)
      j = which(E_label[1, ] == regions[1])
      R = length(regions)
      for(r in 2:R){
        k = which(E_label[1,] == regions[r])
        I.E[, j] = I.E[, j] + I.E[, k]
      }
      k = which(E_label[1, ] %in% regions[-1])
      I.E = I.E[, -k]
      io$E = io$E %*% I.E
      E_label[1, which(E_label[1,] %in% regions[1])] <- newname
      i = which(E_label[1, ] %in% regions[-1])
      E_label = E_label[, -i]
    }
  }
  regions <- regions[-1]
  i <- which(RS_label[, 1] %in% regions)
  RS_label <- RS_label[-i, ]
  S <- I[-i, ]
  if('numeric' %in% class(S)){ # Allowing for aggregating all regions
    S = matrix(c(S), nrow = 1)
  }
  if(check == 0){
    i <- which(f_label[1, ] %in% regions)
    f_label <- f_label[, -i]
  }
  
  ######################################
  ## creating a new InputOuput object ##
  ######################################
  if(dim(S)[2] > 1500){
    cat("\n\n    Calculating new InputOuput object...\n\n")
  }
  n <- dim(S)[1]
  IO <- NULL
  IO$Z <- S %*% io$Z %*% t(S)
  IO$RS_label <- RS_label
  IO$f <- S %*% io$f
  IO$f_label <- matrix(f_label, nrow = 2)
  if('E' %in% names(io)){
    IO$E <- S %*% io$E
    IO$E_label <- matrix(E_label, nrow = 2)
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
  if('fV' %in% names(io)){
    IO$fV <- io$fV %*% I.f
    IO$fV_label <- io$fV_label
  }
  xhat <- diag(c(1/IO$X))
  if(dim(IO$X)[1] == 1){ # Allowing for aggregating all regions
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
