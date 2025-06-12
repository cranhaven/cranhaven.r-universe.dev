#' @title Check the type of variables
#'
#' @description Function extracted (with small changes) of the function \code{\link{gowdis}}
#' to check the type of variables in a data.frame or matrix.
#'
#' @encoding UTF-8
#' @param data A data.frame or matrix.
#' @return A vector with the variable types, where 'c' is continuous/numeric, 'o' is
#' ordinal, 'b' is binary, 'n' is nominal and 'f' is factor.
#' @author Vanderlei Julio Debastiani <vanderleidebastiani@@yahoo.com.br>
#' @seealso  \code{\link{syncsa}}, \code{\link{organize.syncsa}}, \code{\link{var.dummy}}
#' @keywords SYNCSA
#' @export
var.type <- function(data)
{
  if(!inherits(data, c("data.frame", "matrix"))){
    stop("data must be a matrix or a data.frame")
  }
  colnames(data) <- colnames(data, do.NULL = FALSE, prefix = "var")
  is.bin <- function(k) all(k[!is.na(k)] %in% c(0, 1))
  nc <- ncol(data)
  if (is.data.frame(data)) {
    type <- sapply(data, data.class)
    type2 <- type
    bin.var <- rep(NA, nc)
    for (i in 1:nc) {
      bin.var[i] <- is.bin(data[, i])
    }
    type[type %in% c("numeric", "integer")] <- "c"
    type[type == "ordered"] <- "o"
    type[type == "character"] <- "n"
    type[type == "factor"] <- "f"
    type[bin.var] <- "b"
    type[type2 == "character"] <- "n"
    type[type2 == "factor"] <- "f"
    names(type) <- NULL
  }
  else {
    if(any(sapply(data, data.class) == "character")){
      stop("\n If data is a matrix class it must be entirely numeric \n")
    }
    type <- rep("c", nc)
  }
  return(type)
}
