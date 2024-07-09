#' Get DNA/RNA Sequences from Genbank by GI ID
#'
#' Get DNA/RNA Sequences from Genbank by GI ID
#'
#' This function get DNA/RNA sequences from Genbank by GI ID(s).
#'
#' @param id A character vector, as the GI ID(s).
#'
#' @return A list, each component contains one of the DNA/RNA sequences.
#'
#' @keywords Genbank
#'
#' @aliases getGenbank
#'
#' @author Min-feng Zhu <\email{wind2zhu@@163.com}>
#'
#' @seealso See \code{\link{readFASTA}} for reading FASTA format files.
#'
#' @export getGenbank
#'
#' @examples
#' \donttest{
#' # Network latency may slow down this example
#' # Only test this when your connection is fast enough
#' require(RCurl)
#' 
#' ids = c(2, 11)
#' getGenbank(ids)}


getGenbank = function (id) {
  id = as.character(id)
  
  n = length(id)
  
  dna = vector('list', n)
  
  for (i in 1:n) {
    url = paste('http://www.ncbi.nlm.nih.gov/sviewer/viewer.cgi?tool=portal&sendto=on&log$=seqview&db=nuccore&dopt=fasta&sort=&val=', id[i], 
              '&from=begin&to=end&maxplex=1', sep = '')
    genb = RCurl::getURL(url) 
    sequences = strsplit(genb[[1]], split = "\n")[[1]]
    start = 2
    end = length(sequences)
    dna[[i]]=  paste(sequences[start:end], collapse = "")
  }
  
  gi_name = lapply(1:n, function(i) paste('gi', id[i], sep = "_"))
 
  names(dna) = gi_name
  return(dna)
} 



