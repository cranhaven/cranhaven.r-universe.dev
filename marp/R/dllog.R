#' Density function of Log-Logistics model
#' @param x input data for Log-Logistics model
#' @param shape shape parameter of Log-Logistics model
#' @param scale scale parameter of Log-Logistics model
#' @param log logic function to determine whether log of logistics to be returned
#' @return returns the density of the Log-Logistics model
#' @examples
#' x <- as.numeric(c(350., 450., 227., 352., 654.))
#' # set paramters
#' shape <- 5
#' scale <- 3
#' log <- FALSE
#' result_1 <- marp::dllog(x, shape, scale, log)
#'
#' # alternatively, set log == TRUE
#' log <- TRUE
#' result_2 <- marp::dllog(x, shape, scale, log)
#'
#' @export

dllog <- function (x,shape = 1,scale = 1,log = FALSE) {
  fx <- (shape / scale) * (x / scale) ^ {shape - 1} / (1 + (x / scale) ^ shape) ^ 2
  if (log)
    return(log(fx))
  else
    return(fx)
}
