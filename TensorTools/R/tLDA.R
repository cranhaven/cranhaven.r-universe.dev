#' Linear discriminate analysis (LDA) on a 3D tensor
#' @param tnsr, a 3-mode tensor S3 class object
#' @param nClass, Number of classes
#' @param nSamplesPerClass, Samples in each class
#' @param tform, Any discrete transform.
#' fft: Fast Fourier Transorm
#'
#' dwt: Discrete Wavelet Transform (Haar Wavelet)
#'
#' dct: Discrete Cosine transform
#'
#' dst: Discrete Sine transform
#'
#' dht: Discrete Hadley transform
#'
#' dwht: Discrete Walsh-Hadamard transform
#' @return S3 class tensor
#' @examples
#' data("Mnist")
#' T <- Mnist$train$images
#' myorder <- order(Mnist$train$labels)
#' # tLDA need to be sorted by classes
#' T_sorted <- T$data[,myorder,]
#' # Using small tensor, 2 images for each class for demonstration
#' T <- T_sorted[,c(1:2,1001:1002,2001:2002,3001:3002,4001:4002,
#' 5001:5002,6001:6002,7001:7002,8001:8002,9001:9002),]
#' tLDA(as.Tensor(T),10,2,"dct")
#' @author Kyle Caudle
#' @author Randy Hoover
#' @author Jackson Cates
#' @author Everett Sandbo
#' @references Xanthopoulos, P., Pardalos, P. M., Trafalis, T. B., Xanthopoulos, P., Pardalos, P. M., & Trafalis, T. B. (2013). Linear discriminant analysis. Robust data mining, 27-33.

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

  # Expects the tensor to be sorted into classes along
  # mode 2.

  # i.e. Class 1, 1:nSamplesPerClass
  #      Class 2, (nSamplesPerClass+1):(2*nSamplesPerClass)
  # etc.

  modes <- tnsr$modes
  r <- modes[1]
  c <- modes[2]
  d <- modes[3]
  N <- nSamplesPerClass

  # Center the Tensor by subtracting the tensor mean
  CT <- as.Tensor(array(0, dim = c(r, c, d)))
  for (i in 0:(nClass-1)) {
    M <- tmean(as.Tensor(tnsr$data[,(i*N+1):(N*(i+1)),]))
    for (j in (i*N+1):(N*(i+1))) {
      CT$data[,j,] <- tnsr$data[,j,]-M$data[,,]
    }
  }

  # Compute the within-class scatter
  Sw <- as.Tensor(array(0, dim = c(r, r, d)))


  for (i in 0:(nClass-1)) {
    CTsub <- as.Tensor(CT$data[,(i*N+1):(N*(i+1)),])
    CTtrans <- t_tpose(CTsub,tform)
    Tcov <- tmult(CTsub,CTtrans,tform)
    Sw$data <- Sw$data + Tcov$data
  }

  # Add noise to prevent it form being singular
  tNoise <- t_rand(modes = c(r,r,d))
  tNoise$data <- .0001*tNoise$data
  Sw$data <- Sw$data + tNoise$data
  # Invert within class scatter tensor
  inv_Sw = tINV(Sw,tform)

  # Compute the between-class scatter

  # Construct a tensor with only the mean for mean subtraction %
  tMu <- tmean(tnsr)

  Sb <- as.Tensor(array(0, dim = c(r, r, d)))

  tTemp <- as.Tensor(M$data-tMu$data)

  for (i in 0:(nClass-1)) {
    M <- tmean(as.Tensor(tnsr$data[,(i*N+1):(N*(i+1)),]))
    tTemp2 <- tmult((tTemp),t_tpose(tTemp,tform),tform)
    Sb$data <- Sb$data + N*tTemp2$data
  }


  ratio <- tEIG(tmult(inv_Sw,Sb,tform),tform)
  modes <- ratio$P$modes
  r <- modes[1]
  c <- modes[2]
  d <- modes[3]

  if (d < nClass) {
    M <- Re(ratio$P$data[,1:d,])
  } else {
    M <- Re(ratio$P$data[,1:(nClass-1),])
  }

  return(as.Tensor(M))

}
