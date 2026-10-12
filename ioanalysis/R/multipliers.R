multipliers <- function(io, ES, regions = "all", sectors = "all", multipliers, wage.row, employ.closed.row, employ.physical.row){
  # Preliminaries
  if(!"InputOutput" %in% class(io)) stop('io should be of "InputOutput" class. See ?as.inputoutput')
  if(missing(multipliers)) stop('Please specify multipliers = . Options are: output, input, wage, and/or employment')
  multipliers <- unique(multipliers)
  for(m in 1:length(multipliers)){
    if(!multipliers[m] %in% c("input", "output", "wage", "employment.closed", "employment.physical")){
      stop(paste(multipliers[m], "is not a valid option. Valid options are: output, input, wage, employment.closed and/or employment.physical"))
    }
  }
  L <- io$L
  V <- io$V
  V_label <- io$V_label
  A <- io$A
  B <- io$B
  X <- io$X
  n <- dim(L)[1]
  RS_label <- io$RS_label
  # Grabbing the regions and sectors
  if(missing(ES)){
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
  } else if(!missing(ES)){
    location <- as.numeric(ES[,1])
    regions <- unique(ES[,2])
  }
  ######################
  ## Lets get to work ##
  ######################
  mult <- vector("list", length(regions))
  names(mult) <- regions
  ############
  ## output ##
  ############
  if("output" %in% multipliers){
    one <- matrix(rep(1, n), nrow = 1)
    output <- t(one %*% L)
    for(r in 1:length(regions)){
      i <- which(RS_label[, 1] %in% regions[r])
      i <- intersect(i, location)
      sector <- RS_label[i,2]
      out <- matrix(output[i,])
      colnames(out) <- "output"
      rownames(out) <- sector
      mult[[r]] <- cbind(mult[[r]], out)
    }
  }
  ###########
  ## input ##
  ###########
  if("input" %in% multipliers){
    one <- matrix(rep(1, n))
    G <- io$G
    input <- G %*% one
    for(r in 1:length(regions)){
      i <- which(RS_label[, 1] %in% regions[r])
      i <- intersect(i, location)
      sector <- RS_label[i,2]
      inp <- matrix(input[i,])
      colnames(inp) <- "input"
      rownames(inp) <- sector
      mult[[r]] <- cbind(mult[[r]], inp)
    }
  }
  ############
  ## Income ##
  ############
  if("wage" %in% multipliers){
    n <- length(wage.row)
    wages <- matrix(V[wage.row, ], nrow = n)
    wagenames <- V_label[wage.row,1]
    # Creating an object to store the data
    i <- which(X == 0)
    X[i] <- 1
    inc <- t(wages %*% L %*% diag(c(1/X)))
    # Putting it away
    colnames(inc) <- paste("wage",wagenames, sep = ".")
    for(r in 1:length(regions)){
      i <- which(RS_label[, 1] %in% regions[r])
      i <- intersect(i, location)
      sector <- RS_label[i,2]
      income <- matrix(inc[i,], nrow = length(i))
      colnames(income) <- paste("wage", wagenames, sep = ".")
      rownames(income) <- sector
      mult[[r]] <- cbind(mult[[r]], income)
    }
  }
  #######################
  ## Employment.close ##
  ######################
  if("employment.closed" %in% multipliers){
    if(class(employ.closed.row) == "character"){
      for(e in 1:length(employ.closed.row)){
        if(!employ.closed.row[e] %in% unique(RS_label[,2])) stop(paste(employ.closed.row[e], "is not a sector in RS_label. Check spelling, capitalization, and punctuation."))
      }
      employed.row <- which(RS_label[,2] %in% employ.closed.row)
    } else if(class(employ.closed.row) %in% c("numeric", "integer")){
      employed.row = employ.closed.row
    }
    n <- length(employed.row)
    empl <- matrix(A[employed.row, ], nrow = n)
    employed.names <- paste(RS_label[employed.row,1], RS_label[employed.row, 2], sep = ".")
    # Creating an object to store the data
    emp <- t(empl %*% L)
    # Cleaning up after ouselves
    for(r in 1:length(regions)){
      i <- which(RS_label[, 1] %in% regions[r])
      i <- intersect(i, location)
      sector <- RS_label[i,2]
      employ <- matrix(emp[i,], ncol = n)
      colnames(employ) <- paste("employment.closed", employed.names, sep = ".")
      rownames(employ) <- sector
      mult[[r]] <- cbind(mult[[r]], employ)
    }
  }
  #########################
  ## employment.physical ##
  #########################
  if("employment.physical" %in% multipliers){
    if(is.null(io$P)) stop("The physical matrix (P) is missing. Consult ?as.inputoutput")
    P <- io$P
    if(is.null(io$P_label)) stop("The physical matrix label (P_label) is missing. Consult ?as.inputoutput")
    P_label <- io$P_label
    if(class(employ.physical.row) == "character"){
      for(e in 1:length(employ.physical.row)){
        if(!employ.physical.row[e] %in% unique(P_label[,1])) stop(paste(employ.physical.row[e], "is not a sector in P_label. Check spelling, capitalization, and punctuation."))
      }
      employed.row <- which(P_label[,2] %in% employ.physical.row)
    } else if(class(employ.physical.row) %in% c("numeric", "integer")){
      employed.row = employ.physical.row
    }
    n <- length(employed.row)
    empl <- matrix(P[employed.row, ], nrow = n)
    employed.names <- P_label[employed.row,1]
    # Creating an object to store the data
    emp <- t(empl %*% L)
    # Cleaning up after ouselves
    for(r in 1:length(regions)){
      i <- which(RS_label[, 1] %in% regions[r])
      i <- intersect(i, location)
      sector <- RS_label[i,2]
      employ <- matrix(emp[i,], ncol = n)
      colnames(employ) <- paste("employment.closed", employed.names, sep = ".")
      rownames(employ) <- sector
      mult[[r]] <- cbind(mult[[r]], employ)
    }
  }
  if(length(mult) == 1){
    mult <- mult[[1]]
  }
  mult
}
