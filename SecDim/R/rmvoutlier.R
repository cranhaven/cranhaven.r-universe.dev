#' Removing outliers.
#'
#' @description Function for removing outliers.
#'
#' @usage rmvoutlier(x, coef = 2.5)
#'
#' @param x A vector of a variale
#' @param coef A number of the times of standard deviation. The default value is 2.5.
#'
#' @return Location of outliers in the vector
#'
#' @importFrom stats sd
#'
#' @examples
#' data("obs")
#' obs$Cr_ppm <- log(obs$Cr_ppm)
#' krm <- rmvoutlier(obs$Cr_ppm)
#' krm
#'
#' @export
#'

rmvoutlier <- function(x, coef = 2.5){
  k <- which((is.na(x)) |
               (x > (mean(x, na.rm = T) + coef * sd(x, na.rm = T)) |
                  x < (mean(x, na.rm = T) - coef * sd(x, na.rm = T))  ))
  if (length(k) > 0) {
    message("Remove ", length(k), " outlier(s)")
  } else {
    message("No outlier.")
  }

  return(k)
}
