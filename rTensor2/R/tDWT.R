tDWT <- function (tnsr) {
  # Performs the Discrete Wavelet Transform of a 3-D Tensor.
  # Input: 3D tensor
  # Output: 3D tensor where a DWT has been performed along
  # mode 3.

  if (tnsr@num_modes != 3)
    stop("tDwt only implemented for 3d so far")

  modes <- tnsr@modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]

  # Check to make sure n3 is more than 2.
  if (n3==2)
    stop("Mode 3 must have length 2^n where n>2")
  #  Check to see if mode 3 is a power of 2.
  if (sum(as.numeric(intToBits(n3))) != 1)
    stop("Mode 3 must be a power of 2 otherwise using 0 padding")

  dwtz = array(0,dim=c(n1,n2,n3))
  for (i in 1:n1) {
    for (j in 1:n2) {
      x <- tnsr[i,j,]@data
      # Transform mode 3 with Haar DWT
      wt <- wd(x,filter.number=1,family="DaubExPhase")
      l <- log(length(x)/2,2)
      c.in <- accessC.wd(wt,level=l)
      d.in <- accessD.wd(wt,level=l)

      dwtz[i,j,] <- c(c.in,d.in)
    }
  }
  dwtz <- as.tensor(dwtz)
  return(dwtz)
}
