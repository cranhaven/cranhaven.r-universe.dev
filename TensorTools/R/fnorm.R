#' The Frobenius Norm
#' @description The Frobenius norm of an array is the square root of the sum of its squared elements. This function works for vector and matrix arguments as well.
#' @param tnsr, a 3-mode tensor S3 class object
#' @return The Frobenius norm
#' @references Friedland, S., & Aliabadi, M. (2018). Linear algebra and matrices. Society for Industrial and Applied Mathematics.
#' @examples
#' T <- t_rand(modes=c(2,2,4))
#' fnorm(T$data)
#' @author Kyle Caudle
#' @author Randy Hoover
#' @author Jackson Cates
#' @author Everett Sandbo


fnorm <- function (tnsr)
{
  return(sqrt(sum(tnsr^2)))
}
