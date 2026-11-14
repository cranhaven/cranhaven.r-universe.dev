## ---------- getseq.R ------------ ##
#                                    #
#     get.seq                        #
#                                    #
## -------------------------------- ##

## ---------------------------------------------------------------- ##
#      get.seq <- function(id, db = 'uniprot', as.string = TRUE)     #
## ---------------------------------------------------------------- ##
#' Import a Protein Sequence from a Database
#' @description Imports a protein sequence from a selected database.
#' @usage get.seq(id, db = 'uniprot', as.string = TRUE)
#' @param id the identifier of the protein of interest.
#' @param db a character string specifying the desired database; it must be one of 'uniprot' or 'metosite'.
#' @param as.string logical, if TRUE the imported sequence will be returned as a character string.
#' @details MetOSite uses the same type of protein ID than UniProt.
#' @return Returns a protein  sequence either as a character vector or a as a character string.
#' @examples get.seq('P01009')
#' @importFrom jsonlite fromJSON
#' @export

get.seq <- function(id, db = 'uniprot', as.string = TRUE){

  db <- tolower(db)

  if (db == 'uniprot'){
    baseUrl <- "http://uniprot.org/uniprot/"
    call <- paste(baseUrl, id, ".fasta", sep = "")

  } else if (db == 'metosite'){
    baseUrl <- 'https://metosite.uma.es/api/proteins/scan/'
    call <- paste(baseUrl, id, sep = "")

  } else {
    stop('You should indicate a proper DB')
  }

  ## -------- Client <-> Server Communication -------- ##
  if (!is.null(call)){
    text <- gracefully_fail(call)
  }

  ## -------------- Parsing the response --------------- ##
  if (!is.null(text)){
    if (db == 'uniprot'){
      seq <- strsplit(text, split = "\\n")[[1]][-1]
      seq <- paste(seq, collapse = "")
    } else if (db == 'metosite'){
      data <- jsonlite::fromJSON(text, flatten = TRUE)
      seq <- data$prot_seq
    } else if (db == 'kegg-aa' | db == 'kegg-nt'){
      seq <- text
    }

    if (is.null(seq) || is.na(seq)){
      message(paste("The entry", id, "is not found in the", toupper(db), "database"))
      output <- NULL
    } else {
      output <- seq
      if (!as.string){
        output <- strsplit(seq, split ="")
      }
    }

  } else {
    message("Sorry, no result could be retrieved")
    output <- NULL
  }

  if(!is.null(output)){
    attr(output, "ID") <- id
    attr(output, "DB") <- db
  }

  return(output)
}
