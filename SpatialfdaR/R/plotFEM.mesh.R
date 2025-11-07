plotFEM.mesh <- function(pts, tri, xlabel="x", ylabel="y", xlim=plim1, ylim=plim2, 
                         shift=0.05, nonum=TRUE) {

  #  Last modified 19 November 2021 by Jim Ramsay

  ntri <- dim(tri)[1]
  npts <- dim(pts)[1]
  plim1 <- c(min(pts[,1]),max(pts[,1]))
  plim2 <- c(min(pts[,2]),max(pts[,2]))
  plot(pts[,1], pts[,2], type="p", lwd=4, xlab=xlabel, ylab=ylabel,
       xlim, ylim)
  if (!nonum) {
    for (i in 1:npts) {
      text(pts[i,1]+shift, pts[i,2]-0.03, i)
    }
  }
  for (i in 1:ntri) {
    lines(c(pts[tri[i,1],1],pts[tri[i,2],1]),
          c(pts[tri[i,1],2],pts[tri[i,2],2]), lwd=2)
    lines(c(pts[tri[i,2],1],pts[tri[i,3],1]),
          c(pts[tri[i,2],2],pts[tri[i,3],2]), lwd=2)
    lines(c(pts[tri[i,3],1],pts[tri[i,1],1]),
          c(pts[tri[i,3],2],pts[tri[i,1],2]), lwd=2)
  }
  nodes1  <- pts[tri[,1],]
  nodes2  <- pts[tri[,2],]
  nodes3  <- pts[tri[,3],]
  nodectr <- (nodes1+nodes2+nodes3)/3
  if (!nonum) {
    ntri <- dim(tri)[1]
    if (ntri == 1) {
      text(nodectr[1], nodectr[2], 1, col=4)
    } else {
      for (itri in 1:ntri) {
        text(nodectr[itri,1], nodectr[itri,2], as.character(itri), col=4)
      }
    }
  }
}
