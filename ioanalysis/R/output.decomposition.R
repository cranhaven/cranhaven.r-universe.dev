output.decomposition <- function(io1, io2, origin = "all", cause = "all"){
  # Preliminaries
  if(!"InputOutput" %in% class(io1)) stop('io1 should be of "InputOutput" class. See ?as.inputoutput')
  if(!"InputOutput" %in% class(io2)) stop('io2 should be of "InputOutput" class. See ?as.inputoutput')
  if(!all(io1$RS_label == io2$RS_label )) stop("RS_label for io1 and io2 do not match.They must be the same size. Try which(io1$RS_label != io2$RS_label) or see ?locate.mismatch.")
  RS_label <- io1$RS_label
  L1 <- io1$L
  delta.L <- io2$L - L1
  if(dim(io1$f)[2] > 1){
    one <- matrix(rep(1, dim(io1$f)[2]))
    f1 <- io1$f %*% one
  } else if(dim(io1$f)[2] == 1){
    f1 <- io1$f
  }
  if(dim(io2$f)[2] > 1){
    one <- matrix(rep(1, dim(io1$f)[2]))
    delta.f <- io2$f %*% one - f1
  } else if(dim(io2$f)[2] == 1){
    delta.f <- io2$f - f1
  }
  if("all" %in% origin){
    origin <- c("total", "internal", "external")
  } else
    for(i in 1:length(origin)){
      if(!origin[i] %in% c("total", "internal", "external")) stop(paste(origin[i], "is not a valid option for origin. Please choose from total, internal, and/or external."))
    }
  if("all" %in% cause){
    cause <- c("total", "finaldemand", "leontief")
  } else
    for(i in 1:length(cause)){
      if(!cause[i] %in% c("total", "finaldemand", "leontief")) stop(paste(cause[i], "is not a valid option for cause. Please choose from total, finaldemand, and/or leontief."))
    }
  ##########################
  ## Let the games begin! ##
  ##########################
  output <- NULL
  ###########################################
  ## Originating internally and externally ##
  ###########################################
  if("total" %in% origin){
    if("total" %in% cause){
      delta.X <- delta.L %*% delta.f
      colnames(delta.X) <- "delta.X"
      rownames(delta.X) <- paste(RS_label[,1], RS_label[,2], sep = ".")
      output <- cbind(output, delta.X)
    }
    if("finaldemand" %in% cause){
      delta.X.f <- L1 %*% delta.f
      colnames(delta.X.f) <- "delta.X.f"
      rownames(delta.X.f) <- paste(RS_label[,1], RS_label[,2], sep = ".")
      output <- cbind(output, delta.X.f)
    }
    if("leontief" %in% cause){
      delta.X.L <- delta.L %*% f1
      colnames(delta.X.L) <- "delta.X.L"
      rownames(delta.X.L) <- paste(RS_label[,1], RS_label[,2], sep = ".")
      output <- cbind(output, delta.X.L)
    }
  }
  ############################
  ## Originating internally ##
  ############################
  if("internal" %in% origin){
    if("total" %in% cause){
      int.delta.X <- diag(delta.L) * delta.f
      colnames(int.delta.X) <- "int.delta.X"
      rownames(int.delta.X) <- paste(RS_label[,1], RS_label[,2], sep = ".")
      output <- cbind(output, int.delta.X)
    }
    if("finaldemand" %in% cause){
      int.delta.X.f <- diag(L1) * delta.f
      colnames(int.delta.X.f) <- "int.delta.X.f"
      rownames(int.delta.X.f) <- paste(RS_label[,1], RS_label[,2], sep = ".")
      output <- cbind(output, int.delta.X.f)
    }
    if("leontief" %in% cause){
      int.delta.X.L <- diag(delta.L) * f1
      colnames(int.delta.X.L) <- "int.delta.X.L"
      rownames(int.delta.X.L) <- paste(RS_label[,1], RS_label[,2], sep = ".")
      output <- cbind(output, int.delta.X.L)
    }
  }
  ############################
  ## Originating externally ##
  ############################
  if("external" %in% origin){
    if("total" %in% cause){
      ext.delta.X <- delta.L %*% delta.f - diag(delta.L) * delta.f
      colnames(ext.delta.X) <- "ext.delta.X"
      rownames(ext.delta.X) <- paste(RS_label[,1], RS_label[,2], sep = ".")
      output <- cbind(output, ext.delta.X)
    }
    if("finaldemand" %in% cause){
      ext.delta.X.f <- L1 %*% delta.f - diag(L1) * delta.f
      colnames(ext.delta.X.f) <- "ext.delta.X.f"
      rownames(ext.delta.X.f) <- paste(RS_label[,1], RS_label[,2], sep = ".")
      output <- cbind(output, ext.delta.X.f)
    }
    if("leontief" %in% cause){
      ext.delta.X.L <- delta.L %*% f1 - diag(delta.L) * f1
      colnames(ext.delta.X.L) <- "ext.delta.X.L"
      rownames(ext.delta.X.L) <- paste(RS_label[,1], RS_label[,2], sep = ".")
      output <- cbind(output, ext.delta.X.L)
    }
  }
  region <- unique(RS_label[,1])
  r <- length(region)
  # Consolidating output for ease of reading
  if(length(region) > 1){
    outputlist <- vector("list", length(region))
    names(outputlist) <- region
    for(r in 1:length(region)){
      i <- which(RS_label[,1] == region[r])
      temp <- matrix(output[i, ], ncol = dim(output)[2])
      rownames(temp) <- RS_label[i, 2]
      outputlist[[r]] <- temp
    }
    return(outputlist)
  } else
    return(output)
}
