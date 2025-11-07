plotFEM.fd <- function(fdobj, Xgrid=NULL, Ygrid=NULL, nderivs=rep(0,2), 
                       xlab="X", ylab="Y", zlab="Z", main="",
                       ticktype="detailed", r=10, phi=15, theta=30) {
  
  # PLOT  Plots a FEM object FDOBJ over a rectangular grid defined by
  # vectors Xgrid and Ygrid

  #  Last modified 3 October 2022 by Jim Ramsay.

  if (!is.fd(fdobj)) stop('FDOBJ is not an fd object')
  FEMbasis <- fdobj$basis
  coefmat <- as.matrix(fdobj$coefs)
  nsurf   <- dim(coefmat)[[2]]
  pts <- FEMbasis$params$p
  if (is.null(Xgrid))
  {
    xmin  <- min(pts[,1])
    xmax  <- max(pts[,1])
    nx    <- 51
    Xgrid <- matrix(seq(xmin, xmax, len=nx), ncol=1)
  } else {
    xmin  <- min(Xgrid)
    xmax  <- max(Xgrid)
    nx    <- length(Xgrid)
  }
  if (is.null(Ygrid))
  {
    ymin <- min(pts[,2])
    ymax <- max(pts[,2])
    ny   <- 51
    Ygrid <- matrix(seq(ymin, ymax, len=ny), ncol=1)
  } else {
    ymin  <- min(Ygrid)
    ymax  <- max(Ygrid)
    ny    <- length(Ygrid)
  }
  Xvec  <- matrix(outer(Xgrid,rep(1,ny)),nx*ny,1)
  Yvec  <- matrix(outer(rep(1,nx),Ygrid),nx*ny,1)
  XYpts <- cbind(Xvec,Yvec)
  evalmat <- eval.FEM.fd(XYpts, fdobj, nderivs)
  oldpar <- par(no.readonly = TRUE)    
  on.exit(par(oldpar))            
  aski  <- FALSE
  for (isurf in 1:nsurf) {
    if (nsurf > 1) {
      par(mfrow=c(1,1),ask=aski)
    }
    evalmati <- matrix(evalmat[,isurf],nrow=nx, ncol=ny, byrow=F)
    rgl::open3d()
    zlim <- range(evalmati)
    zlen <- 50
    colorlut <- heat.colors(zlen) # height color lookup table
    col <- colorlut[ zlen*(evalmati - zlim[1])/(zlim[2]-zlim[1]) + 1 ]
    rgl::persp3d(as.numeric(Xgrid),as.numeric(Ygrid), evalmati, color = col, 
                   xlab=xlab, ylab=ylab, zlab=zlab)
  }
}

