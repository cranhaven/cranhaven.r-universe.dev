t_tpose <- function (tnsr,tform)
{
  # Performs a transpose of a symmetric 3-mode tensor using any discrete
  # transform by transposing each of the lateral slices.

  # Input: a 3-mode tensor
  # Output: the transpose of the tensor

  x <- as.array(tnsr@data)
  if (tnsr@num_modes != 3)
    stop("tsym only implemented for 3d so far")
  modes <- tnsr@modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  if (tform=="fft") {
    dTsym <- aperm(apply(tnsr@data, MARGIN = 1:2, fft), c(2,3,1))
  } else if (tform=="dwt") {
    dTsym <- tDWT(tnsr)
  } else if (tform=="dct") {
    dTsym <- aperm(apply(tnsr@data, MARGIN = 1:2, dct), c(2,3,1))
  } else if (tform=="dst") {
    dTsym <- aperm(apply(tnsr@data, MARGIN = 1:2, dst), c(2,3,1))
  } else if(tform=="dwht") {
    dTsym <- aperm(apply(tnsr@data, MARGIN = 1:2, fwht), c(2,3,1))
  } else if(tform=="dht") {
    dTsym <- aperm(apply(tnsr@data, MARGIN = 1:2, fht), c(2,3,1))
  } else {
    stop("Transform not supported")
  }
  tTsym <- array(0,dim = c(n2,n1,n3))
  for (i in 1:n3){
    tTsym[,,i] <- base::t(x[,,i])
  }
  indices <- c(n2,n1,n3)
  if (tform=="fft") {
    dTsym <- aperm(apply(tnsr@data, MARGIN = 1:2, ifft), c(2,3,1))
  } else if (tform=="dwt") {
    dTsym <- tIDWT(as.tensor(tTsym))
  } else if (tform=="dct") {
    dTsym <- aperm(apply(tnsr@data, MARGIN = 1:2, idct), c(2,3,1))
  } else if (tform=="dst") {
    dTsym <- aperm(apply(tnsr@data, MARGIN = 1:2, idst), c(2,3,1))
  } else if(tform=="dwht") {
    dTsym <- aperm(apply(tnsr@data, MARGIN = 1:2, ifwht), c(2,3,1))
  } else if (tform=="dht") {
    dTsym <- aperm(apply(tnsr@data, MARGIN = 1:2, ifht), c(2,3,1))
  } else {
    stop("Transform not supported")
  }

  return(as.tensor(tTsym))
}
