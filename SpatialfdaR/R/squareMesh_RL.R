squareMesh_RL <- function(m=1,orientation="L") {
  #  sets up a uniform triangular mesh over a square
  #  arguments
  #  M ... number of rows and columns
  #  ORIENTATION ... "R" or "L" for (right or left triangle orientation, resp.
  #  DELTA       ... The number of corners on a side of the square.
  
  #  Last modified 20 November 2021
  
  if (orientation == "r") orientation = "R"
  if (orientation == "l") orientation = "L"
  
  if (!(orientation == "R" || orientation == "R"))
    stop("Argument orientation is neither L or R.")
  
  delta <- m+1
  
  #  ----------------  Points and triangulations computation  ---------------
      
  if (orientation == "L") {        
    #  compute the points
    npts <- (m+1)^2
    pts <- matrix(0,npts,2)
    n <- 0
    for (i in 0:m) {
      for (j in 0:m) {
        if (abs(i-j) <= delta) {
          n <- n + 1
          pts[n,] <- c(i,j)
        }
      }
    }
    pts <- pts[1:n,]
    #  set up triangle array
    ntri <- 0
    tri <- NULL
    for (i in 1:m) {
      for (j in 1:m) {
        if (abs(i-j) <= delta) {
          ptsk <- matrix(0,3,2)
          #  compute the triangle vertices
          ptsk[1,] <- c(i-1,j-1)
          ptsk[2,] <- c(i,  j-1)
          ptsk[3,] <- c(i,  j  )
          ptsdiff <- ptsk[,1] - ptsk[,2]
          if (all(abs(ptsdiff) <= delta)) {
            triij <- findpts(ptsk,pts)
            tri <- rbind(tri,triij)
            ntri <- ntri + 1
          }
          ptsk[1,] <- c(i-1,j-1)
          ptsk[2,] <- c(i,  j  )
          ptsk[3,] <- c(i-1,j  )
          ptsdiff <- ptsk[,1] - ptsk[,2]
          if (all(abs(ptsdiff) <= delta)) {
            triij <- findpts(ptsk,pts)
            tri <- rbind(tri,triij)
            ntri <- ntri + 1
          }
        }
      }
    }
  }
  
  if (orientation == "R")  {    
    #  compute the points
    npts <- (m+1)^2
    pts <- matrix(0,npts,2)
    n <- 0
    for (i in 0:m) {
      for (j in 0:m) {
        if (abs(i-j) <= delta) {
          n <- n + 1
          pts[n,] <- c(m-i,j)
        }
      }
    }
    pts <- pts[1:n,]
    #  set up triangle array
    ntri <- 0
    tri <- NULL
    for (i in 1:m) {
      for (j in 1:m) {
        if (abs(i-j) <= delta) {
          ptsk <- matrix(0,3,2)
          ptsk[1,] <- c(m-i+1,j-1)
          ptsk[2,] <- c(m-i  ,j-1)
          ptsk[3,] <- c(m-i  ,j  )
          ptsdiff <- (m-ptsk[,1]) - ptsk[,2]
          if (all(abs(ptsdiff) <= delta)) {
            triij <- findpts(ptsk,pts)
            tri <- rbind(tri,triij)
            ntri <- ntri + 1
          }
          ptsk[1,] <- c(m-i+1,j-1)
          ptsk[2,] <- c(m-i+1,j  )
          ptsk[3,] <- c(m-i,  j  )
          ptsdiff <- (m-ptsk[,1]) - ptsk[,2]
          if (all(abs(ptsdiff) <= delta)) {
            triij <- findpts(ptsk,pts)
            tri <- rbind(tri,triij)
            ntri <- ntri + 1
          }
        }
      }
    }
  }
  
  #  -------------------------  Edge computation  -------------------
      
  edg  <- matrix(0,4*delta+2*(m-delta),2)
  irow <- 0
  #  up, left
  for (k in 1:delta) {
    irow <- irow + 1
    ptsk <- matrix(c(0,0,k-1,k),2,2)
    edg[irow,] <- findpts(ptsk,pts)
  }
  
  #  up, diag
  for (k in (delta+1):m) {
    irow <- irow + 1
    ptsk <- matrix(c(k-delta-1, k-delta,k-1,k),2,2)
    edg[irow,] <- findpts(ptsk,pts)
  }
  
  #  up, top
  for (k in 1:delta) {
    irow <- irow + 1
    ptsk <- matrix(c(m+k-delta-1,m+k-delta,m,m),2,2)
    edg[irow,] <- findpts(ptsk,pts)
  }
  
  #  down, right
  for (k in 1:delta) {
    irow <- irow + 1
    ptsk <- matrix(c(m,m,m-k+1,m-k),2,2)
    edg[irow,] <- findpts(ptsk,pts)
  }
  
  #  down, diag
  for (k in (delta+1):m) {
    irow <- irow + 1
    ptsk <- matrix(c(m-k+delta+1,m-k+delta,m-k+1,m-k),2,2)
    edg[irow,] <- findpts(ptsk,pts)
  }
  
  #  down, bottom
  for (k in 1:delta) {
    irow <- irow + 1
    ptsk <- matrix(c(delta-k+1,delta-k,0,0),2,2)
    edg[irow,] <- findpts(ptsk,pts)
  }
  
  return(list(pts=pts,edg=edg,tri=tri))
  
}

#  ----------------------------------------------------------------------------

findpts <- function(ptsk,pts) {
  #  Last modified 18 November 2021
  n      = dim(pts)[1]
  nfind  = dim(ptsk)[1]
  ptsind = matrix(0,1,nfind)
  for (k in 1:nfind) {
    for (i in 1:n) {
      if (ptsk[k,1] == pts[i,1] && ptsk[k,2] == pts[i,2]) {
        ptsind[k] = i
        break
      }
    }
  }
  return(ptsind)
}

