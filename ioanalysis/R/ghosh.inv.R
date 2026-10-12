ghosh.inv <- function(Z = NULL, X, B, RS_label, regions){
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
  ## Solving for the Ghoshian inverse ##
  ######################################
  if("InputOutput" %in% class(Z)){
    B <- Z$B
    # Full InputOutput
    if(missing(regions)){
      n <- dim(B)[1]
      I <- diag(n)
      duration <- round(8e-10 * n^3/60, 2)
      if(duration >= 0.1){
        print(paste("Calculating the Ghoshian inverse. Should take roughtly", duration, "minutes."))
      }
      G <- solve(I - B)
    } else if(!missing(regions)){
      # Partial InputOutput
      RS_label <- Z$RS_label
      B <- Z$B
      i <- which(RS_label[, 1] %in% regions)
      B <- B[i, i]
      n <- length(i)
      I <- diag(n)
      duration <- round(8e-10 * n^3/60, 2)
      if(duration >= 0.1){
        print(paste("Calculating the Ghoshian inverse. Should take roughtly", duration, "minutes."))
      }
      G <- solve(I - B)
    }
  } else{
    # Full Non-InputOutput
    if(missing(B)){
      if(dim(Z)[1] != dim(Z)[2]) stop("The intermediate transaction (Z) matrix needs to be a square matrix")
      if(dim(Z)[1] != length(X)) stop("Check dimensions/length; Z should be a nxn matrix and X should be a nx1 vector")
      n <- length(X)
      if(n > 1500){
        print("Calculating Intermediate Transaction Matrix (A)...")
      }
      xhat <- diag(c(1/X))
      B <- Z * xhat
    }
    n <- dim(B)[1]
    if(missing(regions)){
      I <- diag(n)
      duration <- round(8e-10 * n^3/60, 2)
      if(duration >= 0.1){
        print(paste("Calculating the Ghoshian inverse. Should take roughtly", duration, "minutes."))
      }
      G <- solve(I - B)
    } else if(!missing(regions)){
      # Partial Non-InputOutput
      i <- which(RS_label[, 1] %in% regions)
      B <- B[i, i]
      n <- length(i)
      I <- diag(n)
      duration <- round(8e-10 * n^3/60, 2)
      if(duration >= 0.1){
        print(paste("Calculating the Ghoshian inverse. Should take roughtly", duration, "minutes."))
      }
      G <- solve(I - B)
    }
  }
  G
}
