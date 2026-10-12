key.sector <- function(io, ES = NULL, crit = 1, regions = "all", sectors = "all", type = c("direct"), intra.inter = FALSE){
  # using the linkages function to grab a table of linkages
  type <- sort(type) # Making sure they output the same
  link <- linkages(io, ES = ES, regions = regions, sectors = sectors, type = type, normalize = FALSE, intra.inter = intra.inter)
  #link <- as.matrix(link)
  # Some preliminaries for constructing the output matrix
  keys <- NULL
  RS_label <- io$RS_label                       # Region sector label
  if("all" %in% regions){
    region <- unique(io$RS_label[, 1]) # Region
  } else if(class(regions) == "character"){
    region <- regions
  } else if(class(regions) == "numeric" | class(regions) == "integer"){
    region <- unique(io$RS_label[, 1])[regions]
  }
  if(!is.null(ES)){
    region <- unique(ES[, 2])
  }
  # Creating an object to store the results
  R <- length(region)
  key <- vector("list", R)
  names(key) <- region
  # Beginning the loop!
  if(intra.inter == FALSE){
    for(r in 1:R){
      l <- -1
      temp <- link[[r]]
      n <- dim(temp)[1]
      for(t in 1:length(type)){ # looping over type of linkage
        Key <- matrix(NA, nrow = n, ncol = 1) # Capital K
        l <- l + 2
        BL <- l
        FL <- BL + 1
        hold <- data.frame(temp[, BL], temp[, FL], Key)
        colnames(hold) <- c(colnames(temp)[c(BL,FL)], paste("key", substring(type[t], 1, 3), sep = ".") )
        rownames(hold) <- rownames(temp)
        for(j in 1:n){
          if(hold[j, 1] <  crit & hold[j, 2] <  crit){hold[j,3] <- "I"}
          if(hold[j, 1] <  crit & hold[j, 2] >= crit){hold[j,3] <- "II"}
          if(hold[j, 1] >= crit & hold[j, 2] >= crit){hold[j,3] <- "III"}
          if(hold[j, 1] >= crit & hold[j, 2] <  crit){hold[j,3] <- "IV"}
        }
        if(t == 1){
          holder <- hold
        }
        else if(t > 1){
          holder <- data.frame(holder, hold)
        }
      }
      key[[r]] <- data.frame(holder)
    }
  } else if(intra.inter == TRUE){
    int <- c("intra", "inter", "agg")
    for(r in 1:R){ # looping over regions
      l <- -1
      counter <- 0
      temp <- link[[r]]
      n <- dim(temp)[1]
      for(t in 1:length(type)){ # looping over type of linkage
        for(i in 1:length(int)){
          Key <- matrix(NA, nrow = n, ncol = 1)
          l <- l + 2
          BL <- l
          FL <- BL + 1
          counter <- counter + 1
          hold <- data.frame(temp[, BL], temp[, FL], Key)
          colnames(hold) <- c(colnames(temp)[c(BL,FL)], paste("key", int[i], substring(type[t], 1, 3), sep = ".") )
          rownames(hold) <- rownames(temp)
          for(j in 1:n){
            if(hold[j, 1] <  crit & hold[j, 2] <  crit){hold[j,3] <- "I"}
            if(hold[j, 1] <  crit & hold[j, 2] >= crit){hold[j,3] <- "II"}
            if(hold[j, 1] >= crit & hold[j, 2] >= crit){hold[j,3] <- "III"}
            if(hold[j, 1] >= crit & hold[j, 2] <  crit){hold[j,3] <- "IV"}
          }
          if(counter == 1){
            holder <- hold
          }
          else if(counter > 1){
            holder <- data.frame(holder, hold)
          }
          
        }
      }
      key[[r]] <- data.frame(holder)
    }
  }
#  if(length(region) == 1){
#    key <- key[[1]]
#  }
  if(!is.null(ES)){
    for(r in 1:length(region)){
      sectors = ES[which(ES[,2] == region[r]), 3]
      key[[r]] = key[[r]][rownames(key[[r]]) %in% sectors, ]
    }
  }
  key
}
