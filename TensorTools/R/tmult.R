#' Tensor multiplication
#' @description Performs the tensor product of two 3D tensors using any discrete transform
#' @param x, a 3-mode S3 tensor class object
#' @param y, a 3-mode S3 tensor class object
#' @param tform, Any discrete transform.
#'
#' fft: Fast Fourier Transform
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
#'
#' @return S3 tensor object
#' @examples
#' T1 <- t_rand(modes=c(2,2,4))
#' T2 <- t_rand(modes=c(2,3,4))
#' print(tmult(T1,T2,"dst"))
#' @author Kyle Caudle
#' @author Randy Hoover
#' @author Jackson Cates
#' @author Everett Sandbo
#' @references M. E. Kilmer, C. D. Martin, and L. Perrone, “A third-order generalization of the matrix svd as a product of third-order tensors,” Tufts University, Department of Computer Science, Tech. Rep. TR-2008-4, 2008
#'
#' K. Braman, "Third-order tensors as linear operators on a space of matrices", Linear Algebra and its Applications, vol. 433, no. 7, pp. 1241-1253, 2010.

tmult <- function (x,y,tform)
{
  # Multiplies 2 3-mode tensors together using
  # any discrete transform.

  # Note: in order for the product to be conformable,
  # Mode 2 of x and Mode 1 of y must match.

  modes_x <- x$modes
  modes_y <- y$modes
  if (modes_x[2] != modes_y[1])
    stop("Mode 2 of x and Mode 1 of y must match")
  n3 <- modes_x[3]
  if (n3 != modes_y[3])
    stop("Modes 3 of x and y must match")
  if (tform=="dwt"){
    if (sum(as.numeric(intToBits(modes_x[3]))) != 1)
      stop("Mode 3 must be a power of 2 otherwise using 0 padding")
    if (sum(as.numeric(intToBits(modes_y[3]))) != 1)
      stop("Mode 3 must be a power of 2 otherwise using 0 padding")
  }
  if (tform=="fft") {
    t.form=fft
    it.form=ifft
  } else if (tform=="dct") {
    t.form=dct
    it.form=idct
  } else if(tform=="dst") {
    t.form=dst
    it.form=idst
  } else if(tform=="dht") {
    t.form=fht
    it.form=ifht
  } else if(tform=="dwht") {
    t.form=fwht
    it.form=ifwht
  } else if(tform!="dwt") {
    stop("Transform not supported")
  }

  if (tform=="dwt") {
    t_x <- tDWT(x)$data
    t_y <- tDWT(y)$data
  } else {
    t_x <- aperm(apply(x$data, MARGIN = 1:2,t.form), c(2, 3,1))
    t_y <- aperm(apply(y$data, MARGIN = 1:2,t.form), c(2, 3,1))
  }

  ret <- array(0, dim = c(modes_x[1], modes_y[2], n3))
  for (i in 1:n3) {
    first <- t_x[, , i, drop = FALSE]
    second <- t_y[, , i, drop = FALSE]
    ret[, , i] <- matrix(first, nrow = dim(first)[1]) %*%
      matrix(second,nrow = dim(second)[1])
  }
  if (tform=="dwt") {
    ret <- tIDWT(as.Tensor(ret))
  } else {
    as.Tensor(aperm(apply(ret, MARGIN = 1:2,it.form), c(2,3,1)))
  }
}
