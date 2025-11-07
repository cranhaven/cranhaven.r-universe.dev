randomFEMpts <- function(Nsample, pts, tri, logDensityVec=rep(0,ntri)) {
  #  Assigns random locations to within a triangulation mesh.
  #  If intDensityVec=1, each point that lies within the mesh is assigned.
  #  Otherwise, the location is assigned if a ranndom uniform value
  #  is less than or equal to the vector element corresponding to the
  #  triangle within which the point falls.
  
  #  Last modified 1 December 2021
  
  ntri <- dim(tri)[1] 
  
  #  check that length of DensityVec is equal to the number of triangles
  
  if (length(logDensityVec) != ntri)
      stop("Length of log density vector not equal to the number of triangles.")

  #  set sum of DensityVec to 1
  
  DensityVec <- exp(logDensityVec)
  DensityVec <- DensityVec/sum(DensityVec)
  constDensity <- all(DensityVec == 1/ntri)
  
  #  obtain the boundaries of a square containing the mesh
  
  Xmin <- min(pts[,1])
  Xmax <- max(pts[,1])
  Ymin <- min(pts[,2])
  Ymax <- max(pts[,2])
  Xdif <- Xmax - Xmin
  Ydif <- Ymax - Ymin
  #  generate candidate locations  
  Ni <- 0
  randpts <- matrix(0,Nsample,2)
  while (Ni < Nsample) {
    x <- Xmin + runif(1)*Xdif
    y <- Ymin + runif(1)*Ydif
    obspts <- matrix(c(x,y),1,2)
    i <- insideIndex(obspts, pts, tri)
    if (!is.na(i)) {
      #  location is inside at triangle
      i <- i[1]
      if (constDensity) {
        #  DensityVec == 1/ntri, accept location
        Ni <- Ni + 1
        randpts[Ni,] <- obspts
      } else {
        p <- runif(1)
        if (p <= DensityVec[i]) {
          #  accept location
          Ni <- Ni + 1
          randpts[Ni,] <- obspts
        }
      }
    }
  }
  return(randpts)
}
