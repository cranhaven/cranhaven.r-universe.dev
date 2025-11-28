#' Create sominp object
#' 
#' @description `sominp` transforms the data set from `data.table` to 
#' `somsp` format, which can be used as argument in the `somspa` function.
#'
#' @param x The `data.table` object which will be tranformed to `somsp` object.
#' 
#' @details `x` should be in tidy format
#' with four columns: time, latitude, longitude and variable.
#' 
#' @return A `sominp` object. It contains: 
#' 
#' \itemize{
#' 
#' \item{a `matrix` that can be used as input for the `som` function of 
#' the `kohonen package`.}
#' 
#' \item{a `data.table` with the with spatial point coordinates and a corresponding id.}
#' 
#' \item{a `data.table` with the original dataset.}
#' }
#' 
#' @seealso \code{\link{somspa}} 
#' 
#' @examples
#'
#' 
#' \donttest{
#' dummy <- owda[Time <= 1510]
#' inp_som <- sominp(dummy)}
#' 
#' @importFrom stats complete.cases cor cutree dist hclust median reshape sd time
#' @export

sominp <- function(x)
{
  out <- list()
  names(x) <- c("time", "lat", "lon", "variable")
  x[, id := .GRP, by = list(lat, lon)]
  setkey(x, id, time)

  input_for_som <- x[, list(id, time, variable)]
  input_for_som <- as.matrix(reshape(input_for_som, 
                                     ids = "id", 
                                     timevar = "time", 
                                     direction = "wide"))
  
  input_for_som <- input_for_som[complete.cases(input_for_som), ] 
  id_coords <- unique(x[id %in% input_for_som[, 1], list(id, lat, lon)])
  input_for_som <- input_for_som[, -1] 

  out$input_for_som <- input_for_som
  out$coords <- id_coords
  out$input_dt <- x
  out
  class(out) <- 'somin'
  return(out)
}

globalVariables(c("time", "lat", "lon", "variable", "id"))

