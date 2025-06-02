#' @title 
#' String concatenation
#' 
#' @description 
#' The function \code{concat} 
#' concatenates character vectors all together. 
#' 
#' \code{concat0(.)} is a wrapper for \code{concat(., sep = "")}. 
#' \code{concat_(.)} is a wrapper for \code{concat(., sep = "_")}. 
#' 
#' @param ...
#' One or more objects, to be converted to character vectors 
#' and concatenated. 
#' 
#' @param sep 
#' character. The character to use to separate the result. 
#' 
#' @param na.rm 
#' logical. If \code{TRUE} (the default), missing values 
#' are removed before concatenation. 
#' 
#' @return 
#' Always a character value (vector of length \code{1}). 
#' 
#' @seealso \code{\link[base]{paste}}. 
#' 
#' @export
#' 
#' @examples 
#' v <- c("Florence", "Julie", "Angela")
#' concat0(v)
#' concat_(v)
#' concat(v, sep = "^^")
#' concat0(c("a", "b"), c(1, NA, 3), NA)
#' concat(c(NA, NA))
#' concat(c(NA, NA), na.rm = FALSE) # usually not desirable
#' 
concat <- 
function(..., 
         sep = " ", 
         na.rm = TRUE)
{
  x <- unlist(list(...))
  nx <- length(x)

  if (na.rm && nx > 0L) {
    x <- x[!is.na(x)]
  }
  if (nx == 0L) return(NA_character_)
  paste(x, collapse = sep)
}


#' @export
#' @rdname concat
#' 
concat0 <- 
function(..., 
         na.rm = TRUE)
{
  concat(..., sep = "", na.rm = na.rm)  
}


#' @export
#' @rdname concat
#' 
concat_ <- 
function(..., 
         na.rm = TRUE)
{
  concat(..., sep = "_", na.rm = na.rm)
}
