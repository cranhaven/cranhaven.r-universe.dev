meshgrid2d <- function(xx,yy){
  xnum <- length(xx)
  ynum <- length(yy)
  out <- matrix(0, nrow=xnum*ynum, ncol=2)
  out[,1] <- rep(xx,ynum)
  out[,2] <- rep(yy,each=xnum)
  return(out)
}
