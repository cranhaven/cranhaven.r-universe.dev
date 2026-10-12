export.total <- function(io){
  if(!"InputOutput" %in% class(io)) stop('io should be of "InputOutput" class. See ?as.inputoutput')
  Z <- io$Z
  f <- io$f
  RS_label <- io$RS_label
  f_label <- io$f_label
  checkf <- 0
  if(dim(f_label)[2] == 1){
    checkf <- 1
  }
  regions <- unique(RS_label[, 1])
  # Getting to work
  export <- rep(0, dim(Z)[1])
  exports <- export
  if(!'E' %in% names(io) & length(regions) == 1) stop("There is only one region and no exports. Check io$E")
  check <- 0
  if('E' %in% names(io)){
    check <- 1
    E <- io$E
    if(dim(E)[2] > 1){          # Turning matrix into vector
      one <- matrix(rep(1, dim(E)[2]))
      E <- E %*% one
    }
  } else {
    E <- 0
  }
  if(length(regions) > 1){
    for(r in 1:length(regions)){
      i <- which(RS_label[, 1] == regions[r])
      export[i] <- rowSums(Z[i, -i])
      if(checkf == 0){
        j <- which(f_label[1, ] != regions[r])
        one <- matrix(rep(1, length(j)))
        exports[i] <- f[i, j] %*% one
      }
    }
  }
  if(checkf == 1){
    exports <- f
  }
  if(check == 1){
    export <- export + exports + E
  }
  matrix(export)
}
