#' Check if the DNA sequence are in the 4 default types
#'
#' Check if the DNA sequence are in the 4 default types
#'
#' This function checks if the DNA sequence types are in the 4.
#'
#' @param x A character vector, as the input DNA sequence.
#' 
#' @return Logical. \code{TRUE} if all of the DNA types of the sequence
#'         are within the 4 default types.
#'
#' @return The result character vector
#'
#' @keywords check
#'
#' @aliases dnacheck
#'
#' @author Min-feng Zhu <\email{wind2zhu@@163.com}>
#'
#' @export dnacheck
#'
#' @examples
#' x = 'GACTGAACTGCACTTTGGTTTCATATTATTTGCTC'
#' dnacheck(x) # TRUE
#' dnacheck(paste(x, 'Z', sep = '')) # FALSE

dnacheck = function (x) {
  
  DNADict = c("A","G","C","T")
  return(all(strsplit(x, split = "")[[1]]%in%DNADict)) 

}