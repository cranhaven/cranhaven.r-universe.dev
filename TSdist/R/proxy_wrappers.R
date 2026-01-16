
# Euclidean distance

EuclideanDistance <- function(x, y) {
  if (is(try(LPInitialCheck(x, y)))[1] == "try-error") {
    return(NA)
  } else {
  d <- as.numeric(dist(rbind(x, y), "euclidean")) 
  return(d)
  }
}


# Manhattan distance
ManhattanDistance <- function(x, y) {
  if (is(try(LPInitialCheck(x, y)))[1] == "try-error") {
    return(NA)
  } else {
    # The Manhattan distance between two series is computed.
    d <- as.numeric(dist(rbind(x, y), "manhattan")) 
    return(d)
  }
}

# Infinite norm distance
InfNormDistance <- function(x, y) {
  if (is(try(LPInitialCheck(x, y)))[1]=="try-error") {
    return(NA)
  } else {
    # The supremum norm between two series is computed.
    d <- as.numeric(dist(rbind(x, y), "supremum")) 
    return(d)
  }
}

# Minkowski distance
MinkowskiDistance <- function(x, y, p) {
  if (is(try(LPInitialCheck(x, y, p)))[1] =="try-error") {
    return(NA)
  } else {
    # The minkowsky distance with the chosen p value is computed.
    d <- as.numeric(dist(rbind(x, y), "minkowski", p=p)) 
    return(d)
  }
}


  
  
  
