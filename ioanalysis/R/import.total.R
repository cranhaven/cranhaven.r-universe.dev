import.total <- function(io){
  if(!"InputOutput" %in% class(io)) stop('io should be of "InputOutput" class. See ?as.inputoutput')
  Z <- io$Z
  RS_label <- io$RS_label
  regions <- unique(RS_label[, 1])
  # Getting to work
  import <- matrix(0, ncol = dim(Z)[1])
  if(is.null(io$M) & length(regions) == 1) stop("There is only one region and no imports. Check io$M")
  check <- 0
  if(!is.null(io$M)){
    check <- 1
    M <- io$M
    if(dim(M)[1] > 1){
      one <- matrix(rep(1, dim(1)[2]))
      M <- t(one) %*% M
    }
  } else if(length(regions) > 1){
    for(r in 1:length(regions)){
      i <- which(RS_label[, 1] == regions[r])
      import[i] <- colSums(Z[-i, i])
    }
  }
  if(check == 1){
    import <- import + M
  }
  matrix(import)
}
