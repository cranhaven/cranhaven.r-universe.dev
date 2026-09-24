
#' @title Remove Leading/Trailing and Duplicated (Symbols that Look Like) White Spaces
#' 
#' @description
#' To remove leading/trailing and duplicated (symbols that look like) white spaces.
#' 
#' More aggressive than function \link[base]{trimws}.
#' 
#' @param x an object with \link[base]{typeof} being \link[base]{character}
#' 
#' @details 
#' Function [trimws_] is more aggressive than \link[base]{trimws}, that it removes
#' 
#' \itemize{
#' \item {duplicated white spaces}
#' \item {symbols that look like white space, such as `\u00a0` (no-break space)}
#' }
#' 
#' @note 
#' \link[base]{gsub} keeps \link[base]{attributes}
#' 
#' @returns 
#' Function [trimws_] returns an object of \link[base]{typeof} \link[base]{character}.
#' 
#' @examples 
#' (x = c(A = ' a  b  ', b = 'a .  s', ' a  ,  b ; ', '\u00a0  ab '))
#' base::trimws(x)
#' # raster::trim(x) # do not want to 'Suggests'
#' trimws_(x)
#' 
#' (xm = matrix(x, nrow = 2L))
#' trimws_(xm)
#' 
#' #library(microbenchmark)
#' #microbenchmark(trimws(x), trimws_(x))
#' 
#' @export
trimws_ <- function(x) {
  # http://stackoverflow.com/questions/14737266/removing-multiple-spaces-and-trailing-spaces-using-gsub
  # https://stackoverflow.com/questions/25707647/merge-multiple-spaces-to-single-space-remove-trailing-leading-spaces
  if (typeof(x) != 'character') stop('must be typeof-character input')
  
  # '\u00a0' can also looks like a whitespace
  x0 <- gsub(pattern = '\u00a0', replacement = '', x = x)
  
  gsub(pattern = '^ *|(?<= ) | *$', replacement = '', x = x0, perl = TRUE)
}

