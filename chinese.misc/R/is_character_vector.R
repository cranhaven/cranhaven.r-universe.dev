#' A Convenient Version of is.character
#'
#' This function checks to see if the object is a character vector. It is designed to have different
#' actions from \code{\link{is.character}} and thus sometimes more convenient. See Details.
#'
#' Sometimes we want to check if an object is a character vector. But \code{is.character} cannot
#' do this, because it also returns \code{TRUE} for a character matrix or data frame.
#' What's more, we usually not only want to see if an object is of class 
#' character, but also want to see 
#' if it is valid, that is, can be passed to other functions without errors. But \code{is.character} 
#' even returns \code{TRUE} for \code{character(0)}. 
#' Also, \code{is.character(NA)} returns \code{FALSE}, but 
#' \code{is.character(as.character(NA))} returns \code{TRUE}, but in fact there is really 
#' no difference between the two for users and many functions that do not allow \code{NA}.
#'
#' We list below the returns of \code{is.character2}: 
#'
#' \itemize{
#'   \item (1) if the object is \code{NULL}, \code{is.character2} returns \code{FALSE}. 
#'   \item (2) if the object is of length 0, it always returns \code{FALSE}. 
#'   \item (3) if the object is not vector, \code{FALSE}. 
#'   \item (4) if it has only one element and this element is \code{NA}, under all circumstances it 
#' returns \code{FALSE}. 
#'   \item (5) if the vector is of length>1, all the elements are \code{NA}, 
#' but the vector's class is not character, it returns \code{FALSE}. 
#'   \item (6) if a character vector is of length>1, and all the elements
#' are \code{NA}, then the result depends on argument \code{allow_all_na}, if 
#' \code{allow_all_na = TRUE}, then \code{TRUE}, otherwise, \code{FALSE}.
#' }
#'
#' @param x object to be checked
#' @param len numeric vector represents the permitted length of character vector. If an 
#' object is a character vector, but its length is not in \code{len}, the function 
#' still returns \code{FALSE}. The default is \code{NULL},
#' which means any length is OK.
#' @param allow_all_na for length>1 character vector whose elements are 
#' all \code{NA}, if this argument is \code{FALSE}, then the function returns \code{FALSE}, 
#' if this argument is \code{TRUE} (default), then returns \code{TRUE}.
#'
#' @return \code{TRUE} or \code{FALSE}.
#'
#' @export
#' @examples
#' is_character_vector(character(0))
#' is_character_vector(NA)
#' is_character_vector(as.character(NA))
#' is_character_vector(c(NA, NA))
#' is_character_vector(as.character(c(NA,NA)))
#' is_character_vector(as.character(c(NA, NA)), allow_all_na = FALSE)
#' is_character_vector(as.character(c(NA, NA)), allow_all_na = TRUE)
#' is_character_vector(matrix(c("a", "b", "c", "d"), nr = 2))
#' is_character_vector(c("a", "b", "c"), len = c(1, 10))
#' is_character_vector(c("a", "b", "c"), len = c(1:10))
is_character_vector <- function(x, len = NULL, allow_all_na = TRUE) {
  stopifnot(allow_all_na %in% c(TRUE, FALSE))
  if (is.list(x)) {
    y <- FALSE
  }
  else if (!is.vector(x)) {
    y <- FALSE
  }
  else {
    if (length(x) == 0) {
      y <- FALSE
    }
    else {
      if (is.character(x)) {
        if (length(x) > 1) {
          y <- TRUE
          if (allow_all_na == FALSE) {
		    if (all(is.na(x))) {
              y <- FALSE
            }
          }
        }
        else {
          y <- ifelse(is.na(x), FALSE, TRUE)
        }
      }
      else {
        y <- FALSE
      }
    }
  }
  if (y == FALSE) {
    return(FALSE)
  }
  else {
    if (is.null(len)) {
      return(TRUE)
    }
    else {
      y <- ifelse(length(x) %in% len, TRUE, FALSE)
      return(y)
    }
  }
}
