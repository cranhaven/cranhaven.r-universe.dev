extraction <- function(io, ES = NULL, regions = 1, sectors = 1, type = "backward.total", aggregate = FALSE, simultaneous = FALSE, normalize = FALSE){
  # Preliminaries
  if(!"InputOutput" %in% class(io)) stop('io should be of "InputOutput" class. See ?as.inputoutput')
  if(!is.null(ES)){
    if(!"EasySelect" %in% class(ES)) stop('ES should be of "EasySelect". See ?easy.select')
  }
  for(t in 1:length(type)){
    if(!type[t] %in% c("backward", "forward", "backward.total", "forward.total")){
      stop(paste(type[t], "is invalid. Must be any combination of: backward, forward, backward.total, forward.total"))
    }
  }
  A <- io$A                   # Matrix of technical input coefficients
  X <- io$X                   # Total production vector
  B <- io$B
  f <- io$f                   # Final demand
  if(length(X) != length(f)){
    one <- matrix(rep(1, dim(f)[2]))
    f <- f %*% one
  }
  V <- io$V                   # Value Added
  if(length(X) != length(V)){
    one <- matrix(rep(1, dim(V)[1]), nrow = 1)
    V <- one %*% V
  }
  RS_label <- io$RS_label     # Region Sector label
  # Grabbing the regions and sectors
  if(is.null(ES)){
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
  } else if(!is.null(ES)){
    location <- as.numeric(ES[,1])
    regions <- unique(ES[,2])
  }
  #######################
  ## Time for business ##
  #######################
  n <- length(X)
  I <- diag(n)
  one <- matrix(rep(1,n), nrow = 1)
  if(simultaneous == FALSE){
    r <- length(regions)
    ext <- vector("list", r)
    names(ext) <- regions
    if(length(type) > 1){
      linkage <- vector("list", length(type))
      names(linkage) <- type
      for(q in 1:r){
        ext[[q]] <- linkage
      }
    }
    for(i in location){
      link <- vector("list", length(type))
      names(link) <- type
      l <- 0
      sector <- RS_label[i,2]
      ##############
      ## Backward ##
      ##############
      if("backward" %in% type){
        Arr <- A
        Arr[, i] <- 0
        duration <- round(8e-10 * n^3/60, 2)
        if(duration >= 0.1){
          print(paste("Calculating inverse. Should take roughtly", duration, "minutes."))
        }
        BL <- X - solve(I - Arr) %*% f
        rownames(BL) <- paste(RS_label[, 1], RS_label[, 2], sep = ".")
        if(normalize == TRUE){
          BL <- BL/X
        }
        if(aggregate == TRUE){
          BL <- one %*% BL
        }
        colnames(BL) <- sector
        l <- l + 1
        link[[l]] <- BL
      }
      #############
      ## Forward ##
      #############
      if("forward" %in% type){
        Brr <- B
        Brr[i,] <- 0
        duration <- round(8e-10 * n^3/60, 2)
        if(duration >= 0.1){
          print(paste("Calculating the Leontief inverse. Should take roughtly", duration, "minutes."))
        }
        FL <- X - t(V %*% solve(I - Brr))
        rownames(FL) <- paste(RS_label[, 1], RS_label[, 2], sep = ".")
        if(normalize == TRUE){
          FL <- FL/X
        }
        if(aggregate == TRUE){
          FL <- one %*% FL
        }
        colnames(FL) <- sector
        l <- l + 1
        link[[l]] <- FL
      }
      ####################
      ## Backward Total ##
      ####################
      if("backward.total" %in% type){
        Arr <- A
        Arr[, i] <- Arr[i,] <- 0
        Arr[i,i] <- A[i,i]
        duration <- round(8e-10 * n^3/60, 2)
        if(duration >= 0.1){
          print(paste("Calculating the Leontief inverse. Should take roughtly", duration, "minutes."))
        }
        BL <- X - solve(I - Arr) %*% f
        rownames(BL) <- paste(RS_label[, 1], RS_label[, 2], sep = ".")
        if(normalize == TRUE){
          BL <- BL/X
        }
        if(aggregate == TRUE){
          BL <- one %*% BL
        }
        colnames(BL) <- sector
        l <- l + 1
        link[[l]] <- BL
      }
      ###################
      ## Forward Total ##
      ###################
      if("forward.total" %in% type){
        Brr <- B
        Brr[, i] <- Brr[i,] <- 0
        Brr[i,i] <- B[i,i]
        duration <- round(8e-10 * n^3/60, 2)
        if(duration >= 0.1){
          print(paste("Calculating the Leontief inverse. Should take roughtly", duration, "minutes."))
        }
        FL <- X - t(V %*% solve(I - Brr))
        rownames(FL) <- paste(RS_label[, 1], RS_label[, 2], sep = ".")
        if(normalize == TRUE){
          FL <- FL/X
        }
        if(aggregate == TRUE){
          FL <- one %*% FL
        }
        colnames(FL) <- sector
        l <- l + 1
        link[[l]] <- FL
      }
      ##############################
      ## Storing this lovely data ##
      ##############################
      region <- RS_label[i,1]
      j <- which(names(ext) == region)
      if(length(type) == 1){
        link <- link[[1]]
        ext[[j]] <- cbind(ext[[j]], link)
      } else if(length(link) > 1){
        for(t in 1:length(type)){
          ext[[j]][[t]] <- cbind(ext[[j]][[t]], link[[t]])
        }
      }
    }
    if(length(regions) == 1){
      ext <- ext[[1]]
    }
    #############################
    ## Simultaenous Extraction ##
    #############################
  } else if(simultaneous == TRUE){
    ext <- NULL
    i <- location
    ##############
    ## Backward ##
    ##############
    if("backward" %in% type){
      Arr <- A
      Arr[, i] <- 0
      duration <- round(8e-10 * n^3/60, 2)
      if(duration >= 0.1){
        print(paste("Calculating the Leontief inverse. Should take roughtly", duration, "minutes."))
      }
      BL <- X - solve(I - Arr) %*% f
      rownames(BL) <- paste(RS_label[, 1], RS_label[, 2], sep = ".")
      if(normalize == TRUE){
        BL <- BL/X
      }
      if(aggregate == TRUE){
        BL <- one %*% BL
      }
      colnames(BL) <- "backward"
      ext <- cbind(ext, BL)
    }
    #############
    ## Forward ##
    #############
    if("forward" %in% type){
      Brr <- B
      Brr[i,] <- 0
      duration <- round(8e-10 * n^3/60, 2)
      if(duration >= 0.1){
        print(paste("Calculating the Leontief inverse. Should take roughtly", duration, "minutes."))
      }
      FL <- X - t(V %*% solve(I - Brr))
      rownames(FL) <- paste(RS_label[, 1], RS_label[, 2], sep = ".")
      if(normalize == TRUE){
        FL <- FL/X
      }
      if(aggregate == TRUE){
        FL <- one %*% FL
      }
      colnames(FL) <- "forward"
      ext <- cbind(ext, FL)
    }
    ####################
    ## Backward Total ##
    ####################
    if("backward.total" %in% type){
      Arr <- A
      Arr[, i] <- Arr[i,] <- 0
      Arr[i,i] <- A[i,i]
      duration <- round(8e-10 * n^3/60, 2)
      if(duration >= 0.1){
        print(paste("Calculating the Leontief inverse. Should take roughtly", duration, "minutes."))
      }
      BL <- X - solve(I - Arr) %*% f
      rownames(BL) <- paste(RS_label[, 1], RS_label[, 2], sep = ".")
      if(normalize == TRUE){
        BL <- BL/X
      }
      if(aggregate == TRUE){
        BL <- one %*% BL
      }
      colnames(BL) <- "backward.total"
      ext <- cbind(ext, BL)
    }
    ###################
    ## Forward Total ##
    ###################
    if("forward.total" %in% type){
      Brr <- B
      Brr[, i] <- Brr[i,] <- 0
      Brr[i,i] <- B[i,i]
      duration <- round(8e-10 * n^3/60, 2)
      if(duration >= 0.1){
        print(paste("Calculating the Leontief inverse. Should take roughtly", duration, "minutes."))
      }
      FL <- X - t(V %*% solve(I - Brr))
      rownames(FL) <- paste(RS_label[, 1], RS_label[, 2], sep = ".")
      if(normalize == TRUE){
        FL <- FL/X
      }
      if(aggregate == TRUE){
        FL <- one %*% FL
      }
      colnames(FL) <- "forward.total"
      ext <- cbind(ext, FL)
    }
  }
  ext
}







