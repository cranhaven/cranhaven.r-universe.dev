
# getWprime <- function(lscp){
#   lscp$Wprime$Wprime
#   
# }

  
sum_lscps <- function(lscps, num = NULL, average = TRUE) {
  
  if(!is.list(lscps)){
    stop("'lscps' is not a named list")
    
  }
  if(is.null(names(lscps))){
    warning("Provided list objects are not named")
    
  }
  names(lscps)
  
  Wprimes <- list()
    
    for (l in 1:length(lscps)) {
      
      if(!any(class(lscps[[l]]) == c("grp_Wprime", "wtd_lscp", "poly.surf", 
                                 "data.frame", "matrix"))){
        stop(cat("Object", l ,"is not a dataframe or Morphoscape object containing a wprime surface"))
      }
         
      if(is.data.frame(lscps[[l]]) || is.matrix(lscps[[l]])){
        if(ncol(lscps[[l]]) == 3){
          Wprimes[[l]]$grid <- lscps[[l]]
          
          } else{
          stop(cat("Dataframe", l ,"must have 3 columns corresponding to XYZ spatial data"))
        }
        
      } else{
        # Wprimes[[l]] <- getWprime(lscps[[l]])
        Wprimes[[l]] <- lscps[[l]]$Wprime$Wprime
        
      }
    }
  
  
  grids  <- lapply(Wprimes, FUN = function(X) X$grid)
  
  if(is.null(num)){
    #check grid points match
    for(i in 1:length(grids)){
      
      if(!all(grids[[1]][,1:2] == grids[[i]][,1:2])){
        stop("Grid points do not match between objects 1 and ", i)
      }
    }
    
    
    grid.sum <- Reduce("+", grids)
    
    
  } else{
    
    grid.sum <- list()
    
    if(length(num) == 1){
      

        for(ii in 1:length(Wprimes)){
          grid.sum <- Wprimes[[ii]]$grid + num
          
        
      }
    }
    
    if(length(num) > 1){
      
      if(length(Wprimes) == length(num)){
        for(ii in 1:length(Wprimes)){
          grid.sum <- Wprimes[[ii]]$grid + num[[ii]]
          
        }
      }else{stop("numeric vector does not match number of landscapes")}
    }
    attr(grid.sum, "numeric vector") = num
  }
  

  class(grid.sum) <- "combined.surface"
  attr(grid.sum, "parents") <-  names(lscps)
  attr(grid.sum, "operation") <- "summed"
  attr(grid.sum, "averaged") = average
  
  return(grid.sum)
  
  
}

div_lscps <- function(lscps, binary = FALSE) {
    if(length(lscps) != 2){
      stop("lscps should be a named list of length 2")
    }  
  
  if(is.null(names(lscps))){
    warning("Provided list objects are not named")
    
  }
  
  
  Wprimes <- list()
  
  for (l in 1:length(lscps)) {
    
    if(!any(class(lscps[[l]]) == c("grp_Wprime", "wtd_lscp", "poly.surf", 
                                   "data.frame", "matrix"))){
      stop(cat("Object", l ,"is not a dataframe or Morphoscape object containing a wprime surface"))
    }
    
    if(is.data.frame(lscps[[l]]) || is.matrix(lscps[[l]])){
      if(ncol(lscps[[l]]) == 3){
        Wprimes[[l]]$grid <- lscps[[l]]
        
      } else{
        stop(cat("Dataframe", l ,"must have 3 columns corresponding to XYZ spatial data"))
      }
      
    } else{
      # Wprimes[[l]] <- getWprime(lscps[[l]])
      Wprimes[[l]] <- lscps[[l]]$Wprime$Wprime
      
    }
  }
  
  Az <- Wprimes[[1]]$grid$Z
  Bz <- Wprimes[[2]]$grid$Z
  

  L <- Wprimes[[1]]$grid
  
    L$z <- (Az + 1)/(Bz + 1)

    if (binary) {
        bn <- L$z > 1
        L$z[bn] <- 1
    }
    class(L) <- "combined.surface"
    attr(L, "binary") <-  binary
    attr(L, "parents") <-  names(lscps)
    attr(L, "operation") <- "divided"
    
    
    
    return(L)
}


sub_lscps <- function(lscps, binary = FALSE) {
  if(length(lscps) != 2){
    stop("lscps should be a named list of length 2")
  }  
  
  if(is.null(names(lscps))){
    warning("Provided list objects are not named")
    
  }
  
  
  Wprimes <- list()
  
  for (l in 1:length(lscps)) {
    
    if(!any(class(lscps[[l]]) == c("grp_Wprime", "wtd_lscp", "poly.surf", 
                                   "data.frame", "matrix"))){
      stop(cat("Object", l ,"is not a dataframe or Morphoscape object containing a wprime surface"))
    }
    
    if(is.data.frame(lscps[[l]]) || is.matrix(lscps[[l]])){
      if(ncol(lscps[[l]]) == 3){
        Wprimes[[l]]$grid <- lscps[[l]]
        
      } else{
        stop(cat("Dataframe", l ,"must have 3 columns corresponding to XYZ spatial data"))
      }
      
    } else{
      # Wprimes[[l]] <- getWprime(lscps[[l]])
      Wprimes[[l]] <- lscps[[l]]$Wprime$Wprime
      
    }
  }
  
  Az <- Wprimes[[1]]$grid$Z
  Bz <- Wprimes[[2]]$grid$Z
  
  
  L <- Wprimes[[1]]$grid
  
  L$z <- Az - Bz
  
  if (binary) {
    bn <- L$z > 0
    L$z[bn] <- 1
  }
  class(L) <- "combined.surface"
  attr(L, "binary") <-  binary
  attr(L, "parents") <-  names(lscps)
  attr(L, "operation") <- "subtracted"
  
  
  
  return(L)
}


mult_lscps <- function(lscps, num = NULL) {
  if(length(lscps) != 2){
    stop("lscps should be a named list of length 2")
  }  
  
  if(is.null(names(lscps))){
    warning("Provided list objects are not named")
    
  }
  
  
  Wprimes <- list()
  
  for(l in 1:length(lscps)){
    if(!any(class(lscps[[l]]) == c("grp_Wprime", "wtd_lscp", "poly.surf", 
                                   "data.frame", "matrix"))){
      stop("lscps should contain spatial datasets of class grp_Wprime, wtd_lscp, poly.surf, 
                                   data.frame or matrix")
  }
    if(is.data.frame(lscps[[l]]) || is.matrix(lscps[[l]])){
      if(ncol(lscps[[l]]) == 3){
        Wprimes[[l]]$grid <- lscps[[l]]
        
      } 
    } else{
      # Wprimes[[l]] <- getWprime(lscps[[l]])
      Wprimes[[l]] <- lscps[[l]]$Wprime$Wprime
      
    }
  } 

  
  
  if(!is.null(num)){ #multiply landscapes by numeric vector
    grid.mult <- list()
    
    if(length(num) == 1){
      
        for(ii in 1:length(Wprimes)){
          grid.mult[[ii]] <- Wprimes[[ii]]$grid * num
          
        }
    }
    
    if(length(num) > 1){
      
      if(length(Wprimes) == length(num)){
        for(ii in 1:length(Wprimes)){
          grid.mult[[ii]] <- Wprimes[[ii]]$grid * num[[ii]]
          
        }
      }else{stop("numeric vector does not match number of landscapes")}
    }
    attr(grid.mult, "parents") <-  names(lscps)
    attr(grid.mult, "operation") <- "mult"
    attr(grid.mult, "mult vector") <-  num
    
    return(grid.mult)
  } else{
    
    grids  <- lapply(Wprimes, FUN = function(X) X$grid)
    
    #check grid points match
    for(i in 1:length(grids)){
      
      if(!all(grids[[1]][,1:2] == grids[[i]][,1:2])){
        stop("Grid points do not match between objects 1 and ", i)
      }
    }
    
    grid.mult <- Reduce("*", grids)
    class(grid.mult) <- "combined.surface"
    attr(grid.mult, "parents") <-  names(lscps)
    attr(grid.mult, "operation") <- "mult"
    attr(grid.mult, "mult vector") <-  num
    
    return(grid.mult)
    
  }
  
  
  
  

  
}

