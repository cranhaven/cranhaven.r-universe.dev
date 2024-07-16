#' Guess the taxonomic rank of Scientific Name
#'
#' Guesses the taxonomic rank i.e. Genus, Species or Subspecies based on
#' number of words
#'
#' @param name scientific name string to be checked
#' @return \describe{\item{"Genus or above"}{ = single word} \item{"Species"}{ = 
#' two words}  \item{"Subspecies"}{ = three words} \item{"Unknown"}{ = zero or 
#' more than three words}}
#' @family Name functions
#' @examples
#' guess_taxo_rank("")
#' guess_taxo_rank("Akodon longipilis")
#' guess_taxo_rank("Akodon")
#' guess_taxo_rank("Abrocoma cinerea shistacea")
#' guess_taxo_rank("Abrocoma cinerea shistacea shistacea")
#' @export
guess_taxo_rank <- function(name){
  level <- "Unknown"
  if(!is.empty(name)){
    name <- gsub("\\s+", " ", trimws(name))
    wordcount <- length(strsplit(name," ")[[1]])
    level <- switch(wordcount,
                    "Genus or above",
                    "Species",
                    "Subspecies")
  }
  if(is.null(level)){  
    level <- "Unknown"
  }
  return(level)
}
