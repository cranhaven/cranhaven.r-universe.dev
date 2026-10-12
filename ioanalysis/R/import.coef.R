import.coef <- function(io, region){
  if(!"InputOutput" %in% class(io)) stop('io should be of "InputOutput" class. See ?as.inputoutput')
  
  if(is.null(io$M)){
    square = 1
    io$M = matrix(0, ncol = dim(io$A)[1], nrow = dim(io$A)[1])
  } else if(FALSE %in% (dim(io$A) == dim(io$M))){
    if(check.RS(io) == FALSE) stop("Regions must have same sectors. Try ?locate.mismatch, ?agg.sector, ?easy.select")
    square <- 0
  } else {
    square <- 1
  }
  
  if(!class(region) %in% c("numeric", "integer")) stop("Region must be an integer")
  if(length(region) != 1) stop("Region can only be one number")
  A <- io$A
  M <- io$M
  RS_label <- io$RS_label
  regions <- unique(RS_label[, 1])
  if(length(regions) == 1) stop("Cannot calculate import coefficient matrix if there is only one region")
  if(square == 0){
    if(!is.null(M)){
      warning("\nWARNING: io$M exists. This means the import coefficient matrix may be biased\n\nCoefficient matrix is still calculated.")
    }
    j <- which(RS_label[, 1] == regions[region])
    import <- matrix(0, ncol = length(j), nrow = length(j))
    regions <- regions[-region]
    for(r in 1:length(regions)){
      i <- which(RS_label[, 1] == regions[r])
      import <- import + A[i, j]
    }
  } else if(square == 1){
    j <- which(RS_label[, 1] == regions[region])
    import <- A[j, j] + M[j, j]
  }
  return(import)
}
