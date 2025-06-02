#' @title 
#' Transform values to NA
#' 
#' @description 
#' These methods transform values to \code{\link[base]{NA}} 
#' for different classes of objects. 
#' 
#' @param x
#' The object at stake. 
#' 
#' @param ...
#' Additional arguments (unused).
#' 
#' @return 
#' An object of the same class as \code{x}; 
#' the attributes of \code{x} are passed unchanged to the result. 
#' 
#' @export
#' 
#' @examples 
#' x <- c("a", "b", "c")
#' as.na(x)
#' class(as.na(x)) # still a character
#' 
#' x <- factor(LETTERS)
#' as.na(x)        # levels are kept
#' class(as.na(x)) # still a factor
#' 
#' x <- data.frame(x = 1:3, y = 2:4)
#' as.na(x)
#' dim(as.na(x))
#' 
#' x <- matrix(1:6, 2, 3)
#' attr(x, "today") <- Sys.Date()
#' as.na(x)        # attributes are kept
#'
as.na <-
function(x, 
         ...)
{
  UseMethod("as.na")
}


#' @export
#' @rdname as.na
#'
as.na.default <-
function(x, 
         ...)
{
  w <- seq_along(x)
  x[w] <- rep(NA, length(w))
  x
}


#' @export
#' @rdname as.na
#'
as.na.data.frame <-
function(x, 
         ...)
{
  y <- as.data.frame(lapply(x, FUN = as.na, ...), 
                     stringsAsFactors = FALSE)
  attributes(y) <- attributes(x)
  y
}


#' @export
#' @rdname as.na
#'
as.na.list <-
function(x, 
         ...)
{
  y <- lapply(x, FUN = as.na, ...)
  attributes(y) <- attributes(x)
  y
}
