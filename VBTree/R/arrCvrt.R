#' Convert a structured character array to double list
#'
#' @description Convert a structured character array to a double list. All character elements in array will be splited by
#' a specific pattern then sorted intrinsically in each layer of the double list.
#' @param x A structured character array to be converted.
#' @param ... Argument in \code{\link[VBTree:chrvec2dl]{chrvec2dl}} to control split pattern.
#'
#' @return Return a double list based on the input array.
#' @export arr2dl
#' @seealso \code{\link[VBTree:arr2vbt]{arr2vbt}}, \code{\link[VBTree:chrvec2dl]{chrvec2dl}}.
#'
#' @examples
#' #Write the column names of datatest into a array:
#' arr <- dl2arr(chrvec2dl(colnames(datatest)))
#'
#' #Recover the double list from character array:
#' arr2dl(arr)
#' @keywords array Double.List
arr2dl <- function(x, ...){
  if (!inherits(x, "array")){
    stop("x must be a array.", call. = FALSE)
  }
  x <- as.vector(x)
  result <- chrvec2dl(x, ...)
  return(result)
}

#' Convert a structured character array to double list
#'
#' @description Convert a structured character array to a vector binary tree. All character elements in array will be splited by
#' a specific pattern then sorted intrinsically in each layer of the vector binary tree.
#' @param x A structured character array to be converted.
#' @param ... Argument in \code{\link[VBTree:chrvec2dl]{chrvec2dl}} to control split pattern.
#'
#' @return Return a vector binary tree based on the input array.
#' @export arr2vbt
#' @seealso \code{\link[VBTree:arr2dl]{arr2dl}}, \code{\link[VBTree:chrvec2dl]{chrvec2dl}}.
#'
#' @examples
#' #Write the column names of datatest into a array:
#' arr <- dl2arr(chrvec2dl(colnames(datatest)))
#'
#' #Recover the vector binary tree from character array:
#' arr2vbt(arr)
#' @keywords array Vector.Binary.Tree
arr2vbt <- function(x, ...){
  if (!inherits(x, "array")){
    stop("x must be a array.", call. = FALSE)
  }
  x <- as.vector(x)
  x <- chrvec2dl(x, ...)
  result <- dl2vbt(x)
  return(result)
}
