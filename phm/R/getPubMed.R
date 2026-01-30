#########1#########2#########3#########4#########5#########6#########7#########8
#' Create a data table from a text file in PubMed format
#'
#' This function takes as input a file produced via PubMed in PubMed format
#' and outputs a data frame with the id equal to the PMID, text equal to the
#' abstract, date, title, and author for each publication in the file.
#'
#' @param file path to the PubMed file
#' @return A data table with a row for each publication holding the id
#' equal to the PMID, text equal to the abstract, date, title, and author for
#' that publication.
#' @examples
#' #Go to Pubmed and enter search criteria, save the result to PubMed format.
#' #If the file is called pubmed_result.txt and located in the current
#' #directory:
#' #PM=getPubMed("pubmed_result.txt")
#' #Will load the data from the search into a data table called PM
#' @import data.table
#' @export
################################################################################
getPubMed<-function(file) {
  dt=data.table::as.data.table(getPubMed_cpp(file))
  dt[, date := as.Date(date,format="%Y/%m/%d")]
  dt
}