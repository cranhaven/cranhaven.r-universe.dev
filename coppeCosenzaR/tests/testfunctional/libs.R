

#' LoadAgregationMatrix
#'
#' @param x  address to a file with AgregationMatrix
#'
#' @return NULL
#' @export
#'
# @examples
#'
#' @importFrom utils read.csv
#'
LoadAgregationMatrix <- function(x){
  if (is.na(x)) x = "./R/AgregationMatrix.csv"
  agregationMatrix <- utils::read.csv(x, row.names = 1, header = TRUE)
  save(agregationMatrix,file = "./R/agregationMatrix.Rda")
  load("./R/agregationMatrix.Rda")
  }
