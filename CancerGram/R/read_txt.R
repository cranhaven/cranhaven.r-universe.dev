#' Read sequences from .txt file
#'
#' Read sequence data in a fasta format saved in text file.
#'
#' @param connection a \code{\link{connection}} to the text (.txt) file.
#' @keywords manip
#' @return a list of sequences. 
#' @details The input file should contain one or more amino acid sequences separated by 
#' empty line(s) in a fasta format.
#' @importFrom biogram read_fasta
#' @export
#' @keywords manip
#' @examples 
#' (sequences <- read_txt(system.file("CancerGram/prots.txt", package = "CancerGram")))

read_txt <- function(connection) {
  require_CancerGramModel()
  
  content <- readLines(connection)
  
  #test for empty content
  if(content[1] != "" || length(content) > 1) {
    if (sum(grepl(">", content, fixed = TRUE)) == 0) {
      if (content[1] != "")
        content <- c("", content)
      
      #number of empty lines
      nel <- 0
      #content without too many empty lines
      content2 <- c()
      for (i in 1L:length(content)) {
        if(content[i] == "") {
          nel <- nel + 1
        } else {
          nel <- 0
        }
        if (nel <= 1)
          content2 <- c(content2, content[i])
      }
      content <- content2
      content_end <- length(content)
      while(content[content_end] == "i")
        content_end <- content_end - 1
      prot_names <- sapply(1L:sum(content == ""), function(i)
        paste0(">sequence", i))
      content[content == ""] <- prot_names
    }
    read_fasta(textConnection(content))
  } else {
    warning("No text detected.")
    NULL
  } 
}
