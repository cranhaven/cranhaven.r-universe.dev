export.coef <- function(io, region){
  # This is a helper function; it is not intended for general use.
  if(!"InputOutput" %in% class(io)) stop('io should be of "InputOutput" class. See ?as.inputoutput')
  if(check.RS(io) == FALSE) stop("Regions must have same sectors. Try ?locate.mismatch, ?agg.sector, ?easy.select")
  if(!class(region) %in% c("numeric", "integer")) stop("Region must be an integer")
  if(length(region) != 1) stop("Region can only be one number")
  A <- io$A
  E <- io$E
  if(!is.null(E)){
    warning("\nWARNING: io$E exists. This means the export coefficient matrix may be biased\n\nCoefficient matrix is still calculated.")
  }
  RS_label <- io$RS_label
  regions <- unique(RS_label[, 1])
  if(length(regions) == 1) stop("Cannot calculate export coefficient matrix if there is only one region")
  i <- which(RS_label[, 1] == regions[region])
  export <- matrix(0, ncol = length(i), nrow = length(i))
  regions <- regions[-region]
  for(r in 1:length(regions)){
    j <- which(RS_label[, 1] == regions[r])
    export <- export + A[i, j]
  }
  return(export)
}
