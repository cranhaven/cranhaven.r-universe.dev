#' @title 
#' Maximum length of a stack
#' 
#' @description 
#' The function \code{max_length} returns the maximum number 
#' of objects a stack can contains; this number can be changed 
#' with \code{max_length<-}. 
#' 
#' @param .stack,x
#' A stack. 
#' 
#' @param value
#' numeric. The new maximum length of the stack. 
#' 
#' @return 
#' \code{max_length} returns a (possibly infinite) 
#' nonnegative numeric. 
#' 
#' @export
#' 
max_length <- 
function(.stack)
{
  attr(.stack, "max_length")
}


#' @export
#' @rdname max_length
#' 
"max_length<-" <-
function(x, 
         value)
{
  attr(x, "max_length") <- value
  x
}
