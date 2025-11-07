squareMesh <- function(m=1) {
  #  squareMesh sets up a mesh with m^2 squares with nodal points at corners  
  #  and at the centers
  
  #  Last modified 22 November 2021
  
  #  set up the points array
  
  npts = (m+1)^2 + m^2
  ptsindex = 1:npts
  pts = matrix(0,npts,2)
  ipts = 0
  for (i in 1:m) {
    for (j in 1:m) { 
      # upper left point
      ipts = ipts + 1
      pts[ipts,] = c(i-1,j-1)
      # center point
      ipts = ipts + 1
      pts[ipts,] = c(i-1/2,j-1/2)
    }
    # final upper right point
    ipts = ipts + 1
    pts[ipts,] = c(i-1,m)
  }
  # final lower points
  for (i in 1:m) {
    ipts = ipts + 1
    pts[ipts,] = c(m,i-1)
  }
  ipts = ipts + 1
  pts[ipts,] = c(m,m)
  
  #  set up edge array
  
  nedg = 4*m
  edg = matrix(0,nedg,2)
  iedg = 0
  # upper edge
  for (j in 1:m) {
    iedg = iedg + 1
    edg[iedg,1] = ptsindex[(pts[,1] == 0) & (pts[,2] == j-1)]
    edg[iedg,2] = ptsindex[(pts[,1] == 0) & (pts[,2] == j  )]
  }
  # right edge
  for (i in 1:m) {
    iedg = iedg + 1
    edg[iedg,1] = ptsindex[(pts[,1] == i-1) & (pts[,2] == m)]
    edg[iedg,2] = ptsindex[(pts[,1] == i  ) & (pts[,2] == m)]
  }
  # lower edge
  for (k in 1:m) {
    j = m - k + 1
    iedg = iedg + 1
    edg[iedg,1] = ptsindex[(pts[,1] == m) & (pts[,2] == j  )]
    edg[iedg,2] = ptsindex[(pts[,1] == m) & (pts[,2] == j-1)]
  }
  # left edge
  for (k in 1:m) {
    i = m - k + 1
    iedg = iedg + 1
    edg[iedg,1] = ptsindex[(pts[,1] == i  ) & (pts[,2] == 0)]
    edg[iedg,2] = ptsindex[(pts[,1] == i-1) & (pts[,2] == 0)]
  }

  #  set up triangle array
  
  ntri = 4*m^2
  tri  = matrix(0,ntri,3)
  itri = 0
  for (i in 1:m) {
    for (j in 1:m) {
      itri = itri + 1
      tri[itri,1] = ptsindex[(pts[,1] == i-1  ) & (pts[,2] == j-1  )]
      tri[itri,2] = ptsindex[(pts[,1] == i    ) & (pts[,2] == j-1  )]
      tri[itri,3] = ptsindex[(pts[,1] == i-1/2) & (pts[,2] == j-1/2)]
      itri = itri + 1
      tri[itri,1] = ptsindex[(pts[,1] == i    ) & (pts[,2] == j-1  )]
      tri[itri,2] = ptsindex[(pts[,1] == i    ) & (pts[,2] == j    )]
      tri[itri,3] = ptsindex[(pts[,1] == i-1/2) & (pts[,2] == j-1/2)]
      itri = itri + 1
      tri[itri,1] = ptsindex[(pts[,1] == i-1  ) & (pts[,2] == j    )]
      tri[itri,2] = ptsindex[(pts[,1] == i-1  ) & (pts[,2] == j-1  )]
      tri[itri,3] = ptsindex[(pts[,1] == i-1/2) & (pts[,2] == j-1/2)]
      itri = itri + 1
      tri[itri,1] = ptsindex[(pts[,1] == i-1  ) & (pts[,2] == j    )]
      tri[itri,2] = ptsindex[(pts[,1] == i-1/2) & (pts[,2] == j-1/2)]
      tri[itri,3] = ptsindex[(pts[,1] == i    ) & (pts[,2] == j    )]
    }
  }
  return(list(pts=pts, edg=edg, tri=tri)) 
}
  