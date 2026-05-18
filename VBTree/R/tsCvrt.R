#' Convert a structured character tensor to double list
#'
#' @description Convert a structured character tensor to a double list. All character elements in tensor will be splited by
#' a specific pattern then sorted intrinsically in each layer of the double list.
#' @param x A structured character tensor to be converted.
#' @param ... Argument in \code{\link[VBTree:chrvec2dl]{chrvec2dl}} to control split pattern.
#'
#' @return Return a double list based on the input tensor.
#' @export ts2dl
#' @seealso \code{\link[VBTree:ts2vbt]{ts2vbt}}, \code{\link[VBTree:chrvec2dl]{chrvec2dl}}.
#'
#' @examples
#' #Write the column names of datatest into a tensor:
#' ts <- dl2ts(chrvec2dl(colnames(datatest)))
#'
#' #Recover the double list from character tensor:
#' ts2dl(ts)
#' @keywords tensor Double.List
ts2dl <- function(x, ...){
  if (!inherits(x, "tensor")){
    stop("x must be a tensor.", call. = FALSE)
  }
  x <- as.vector(x)
  result <- chrvec2dl(x, ...)
  return(result)
}

#' Convert a structured character tensor to double list
#'
#' @description Convert a structured character tensor to a vector binary tree. All character elements in tensor will be splited by
#' a specific pattern then sorted intrinsically in each layer of the vector binary tree.
#' @param x A structured character tensor to be converted.
#' @param ... Argument in \code{\link[VBTree:chrvec2dl]{chrvec2dl}} to control split pattern.
#'
#' @return Return a vector binary tree based on the input tensor.
#' @export ts2vbt
#' @seealso \code{\link[VBTree:ts2dl]{ts2dl}}, \code{\link[VBTree:chrvec2dl]{chrvec2dl}}.
#'
#' @examples
#' #Write the column names of datatest into a tensor:
#' ts <- dl2ts(chrvec2dl(colnames(datatest)))
#'
#' #Recover the vector binary tree from character tensor:
#' ts2vbt(ts)
#' @keywords tensor Vector.Binary.Tree
ts2vbt <- function(x, ...){
  if (!inherits(x, "tensor")){
    stop("x must be a tensor.", call. = FALSE)
  }
  x <- as.vector(x)
  x <- chrvec2dl(x, ...)
  result <- dl2vbt(x)
  return(result)
}
