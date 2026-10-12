leontief.inv <- function(Z = NULL, X, A, RS_label, regions){
  # Preliminaries
  if(!missing(regions)){
    if("InputOutput" %in% class(Z)){
      RS_label <- Z$RS_label
      if(class(regions) == "character"){
        for(k in 1:length(regions)){
          if(!regions[k] %in% RS_label[, 1]) stop(paste(regions[k], "is not a region in RS_label. Check spelling, capitalization, and punctuation."))
        }
      }
      else if(class(regions) == "numeric" | class(regions) == "integer"){
        region <- unique(RS_label[, 1])
        regions <- region[regions]
      }
    } else if(!"InputOutput" %in% class(Z)){
      if(missing(RS_label)) stop("Missing RS_label. This is needed to select the correct elements of Z and X to calculate the Leontief inverse.")
      if(class(regions) == "character"){
        for(k in 1:length(regions)){
          if(!regions[k] %in% RS_label[, 1]) stop(paste(regions[k], "is not a region in RS_label. Check spelling, capitalization, and punctuation."))
        }
      }
      else if(class(regions) == "numeric" | class(regions) == "integer"){
        region <- unique(RS_label[, 1])
        regions <- region[regions]
      }
    }
  } 

  ######################################
  ## Solving for the Leontief inverse ##
  ######################################
  if("InputOutput" %in% class(Z)){
    # Full InputOutput
    if(missing(regions)){
      stop("InputOutput objects have Leontief inverse by design. Use io$L")
    } else if(!missing(regions)){
      # Partial InputOutput
      RS_label <- Z$RS_label
      A <- Z$A
      i <- which(RS_label[, 1] %in% regions)
      A <- A[i, i]
      n <- length(i)
      I <- diag(n)
      duration <- round(8e-10 * n^3/60, 2)
      if(duration >= 0.1){
        print(paste("Calculating the Leontief inverse. Should take roughtly", duration, "minutes."))
      }
      L <- solve(I - A)
    }
  } else {
    # Full Non-InputOutput
    if(missing(A)){
      if(dim(Z)[1] != dim(Z)[2]) stop("The intermediate transaction (Z) matrix needs to be a square matrix")
      if(dim(Z)[1] != length(X)) stop("Check dimensions/length; Z should be a nxn matrix and X should be a nx1 vector")
      n <- length(X)
      if(n > 1500){
        print("Calculating Intermediate Transaction Matrix (A)...")
      }
      A <- Z %*% diag(c(1/X))
    }
    n <- dim(A)[1]
    if(missing(regions)){
      I <- diag(n)
      duration <- round(8e-10 * n^3/60, 2)
      if(duration >= 0.1){
        print(paste("Calculating the Leontief inverse. Should take roughtly", duration, "minutes."))
      }
      L <- solve(I - A)
    } else if(!missing(regions)){
      # Partial Non-InputOutput
      i <- which(RS_label[, 1] %in% regions)
      A <- A[i, i]
      n <- length(i)
      I <- diag(n)
      duration <- round(8e-10 * n^3/60, 2)
      if(duration >= 0.1){
        print(paste("Calculating the Leontief inverse. Should take roughtly", duration, "minutes."))
      }
      L <- solve(I - A)
    }
  }
  L
}



