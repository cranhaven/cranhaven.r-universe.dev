tmean <- function(tnsr) {

  # Determines the mean of a 3D tensor
  # along mode 2

  # Input: a 3D tensor of dimensions n1,n2,n3
  # Output: a 3D tensor of dimensions n1,1,n3

  if (tnsr@num_modes != 3)
    stop("tmean only implemented for 3d so far")
  modes <- tnsr@modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]

  # Initialize mean tensor
  mu = rand_tensor(modes = c(n1,1,n3), drop = FALSE) *0

  # Determine the mean of all samples in a tensor.
  for (i in 1:n2) {
    mu[,1,] = mu[,1,] + tnsr[,i,]@data
  }
  mu = mu/n2

  return(mu)
}
