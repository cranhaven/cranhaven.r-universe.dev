tLDA <- function(tnsr,nClass,nSamplesPerClass,tform)
{
  # Performs linear discriminate analysis on a 3D tensor

  # Input: tnsr, a 3D tensor (r x c x d)
  # c is the number of samples, each sample is r x d.

  # nClass, number of classes to discriminate against
  # nSamplesPerClass, number of samples per class (assumes balanced)
  # tform, any discrete transform

  # Output: Returns the eigenvalues and vectors of the ratio
  # between the between group scatter tensor and the within
  # group scatter tensor

if (tnsr@num_modes != 3)
  stop("tLDA only implemented for 3d so far")

# Expects the tensor to be sorted into classes along
# mode 2.

  # i.e. Class 1, 1:nSamplesPerClass
  #      Class 2, (nSamplesPerClass+1):(2*nSamplesPerClass)
  # etc.

modes <- tnsr@modes
r <- modes[1]
c <- modes[2]
d <- modes[3]
N <- nSamplesPerClass

# Center the Tensor by subtracting the tensor mean
CT <- rand_tensor(modes=c(r,c,d), drop = FALSE)*0
for (i in 0:(nClass-1)) {
  M <- tmean(tnsr[,(i*N+1):(N*(i+1)),])
  for (j in (i*N+1):(N*(i+1))) {
    CT[,j,] <- tnsr[,j,]-M[,,]
  }
}

# Compute the within-class scatter
Sw <- rand_tensor(modes = c(r,r,d), drop = FALSE) * 0


for (i in 0:(nClass-1)) {
  CTtrans <- t_tpose(CT[,(i*N+1):(N*(i+1)),],tform)
  Tcov <- tmult(CT[,(i*N+1):(N*(i+1)),],CTtrans,tform)
  Sw <- Sw + Tcov
}

# Add noise to prevent it form being singular
Sw <- Sw + .0001* rand_tensor(modes = c(r,r,d), drop = FALSE)
# Invert within class scatter tensor
inv_Sw = tINV(Sw,tform)

# Compute the between-class scatter

# Construct a tensor with only the mean for mean subtraction %
tMu <- tmean(tnsr)

Sb <- rand_tensor(modes = c(r,r,d), drop = FALSE) * 0

if (tform=="fft") {
  for (i in 0:(nClass-1)) {
    M <- tmean(tnsr[,(i*N+1):(N*(i+1)),])
    Sb <- Sb + N*tmult((M-tMu),t(M-tMu),tform)
  }
} else {
  for (i in 0:(nClass-1)) {
    M <- tmean(tnsr[,(i*N+1):(N*(i+1)),])
    Sb <- Sb + N*tmult((M-tMu),t_tpose(M-tMu,tform),tform)
   }
}


ratio <- tEIG(tmult(inv_Sw,Sb,tform),tform)
modes <- ratio$P@modes
r <- modes[1]
c <- modes[2]
d <- modes[3]

if (d < nClass) {
  M <- Re(ratio$P@data[,1:d,])
  } else {
  M <- Re(ratio$P@data[,1:(nClass-1),])
}

return(as.tensor(M))

}









