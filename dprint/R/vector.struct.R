#' Vector Structure
#'
#' Defines the structure of a vector given to footnote, main, etc.
#'
#' @param vctr character vector
#' @export
vector.struct <-
function(vctr=NA)
{
  vctr.nrw=NA
  if (!is.na(vctr[1]))
    {
      vctr      <- vector.linebreak(vctr)
      vctr.nrw  <- length(vctr)
    }
  return(list(vctr=vctr,          # Newly formatted title
              vctr.nrw=vctr.nrw   # Number of rows title will take
              ))
}

