library('GSVA')          ## for calling gsva function
library('GSA')           ## for calling GSA.read.gmt function
library(circlize)
library(stringr)


#' ssGSEA methods to calculate EMT score
#'
#' @param geneExp A numeric matrix of gene expression values where rows represent genes and columns represent samples/cells.
#' @param geneList A list of signature gene sets to be used for ssGSEA analysis. 
#' @param colnames A character string specifying the name to assign to the column containing the scores in the output data frame.
#'
#' @return A data frame containing sample/cell ID and EMT scores
#' @export
#'
#' @examples
#'
#' library(GSVA)
#' library(curl)
#' url <- "https://zenodo.org/record/15213845/files/geneExp.rda"
#' destfile <- tempfile(fileext = ".rda")
#' download.file(url, destfile, mode = "wb")
#' load(destfile)
#' data(Panchy_et_al_E_signature)
#' Execute_ssGSEA(geneExp, geneList = Panchy_et_al_E_signature, colnames = "Escore")

Execute_ssGSEA <- function(geneExp,geneList,colnames){
  geneslist = list(gene = geneList$GeneName)
  gsvaPar <- gsvaParam(t(geneExp), geneslist)
  Result = gsva(gsvaPar, verbose=FALSE)
  Result = data.frame(t(Result))
  colnames(Result) <- colnames
  return(Result)
}