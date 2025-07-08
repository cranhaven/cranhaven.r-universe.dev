library(stringr)
library(AUCell)          ## provide AUCell function
library(foreach)        
library(doParallel)     ## for parallal processing
library('GSA')          ## for calling GSA.read.gmt function

########################### AUCell Function to compute the scores ###############################################

## Function Input: single-cell gene expression data and genes in one gene sets

## Function Output: Scores for each cell/sample ID

#' AUCell EMT Score Calculation
#'
#' Calculate AUCell scores for a given gene set across single-cell gene expression data.
#'
#' @param geneExp A matrix or data frame of gene expression data, where rows are genes and columns are cells/samples.
#' @param genes A vector of gene names that defines the gene set for the AUCell analysis.
#'
#' @return A data frame with two columns: `SampleID` and `AUCell`, representing the AUCell score for each cell/sample.
#' @export
#'
#' @examples
#' library(curl)
#' url <- "https://zenodo.org/record/15213845/files/geneExp.rda"
#' destfile <- tempfile(fileext = ".rda")
#' download.file(url, destfile, mode = "wb")
#' load(destfile)
#' data(Panchy_et_al_E_signature)
#' geneList = Panchy_et_al_E_signature
#' genes = unlist(geneList$GeneName)
#' AUCellMethod = AUCellfunc(t(geneExp),genes)

AUCellfunc <- function(geneExp,genes){
  
  out <- tryCatch(
    {
      cells_rankings <- AUCell_buildRankings(geneExp,plotStats=FALSE)
      Panchy_et_al_E_signature <- list(geneSet1=genes)
      cells_AUC <- AUCell_calcAUC(Panchy_et_al_E_signature, cells_rankings, aucMaxRank=nrow(cells_rankings)*0.05)
      cellsAUCretrieve = getAUC(cells_AUC)
      cells_AUCellScores = t(cellsAUCretrieve)
      cells_AUCellScores = data.frame(rownames(cells_AUCellScores),cells_AUCellScores)
      colnames(cells_AUCellScores) = c('SampleID','AUCell')
      row.names(cells_AUCellScores)= NULL
      return(cells_AUCellScores)
      
    },
    
    error = function(cond) {
      cells_AUCellScores = data.frame(SampleID = "NA", AUCell = "NA")
      return(cells_AUCellScores)
    }
  )
  return(out)
}

############################## Calling of AUCell function using the Msigb Gene Sets ####################################

## Function Input: single-cell gene expression data, all gene sets in a list, and k represents the index for a gene set in a list

## Function Output: A dataframe containing sample/cell ID, gene set score and pathwayName

### AUCell function in AUCell
## This function is called by AUCell function to compute ranks of each cell for a gene signature

#' AUCell methods to calculate EMT score
#' 
#' @param geneExp A numeric matrix of gene expression values
#' @param geneList A list of signature gene sets
#' @param colnames A character string specifying the name to assign to the column containing the scores in the output data frame.
#'
#' @return A data frame containing sample/cell ID and EMT scores
#' @export
#'
#' @examples
#' library(curl)
#' url <- "https://zenodo.org/record/15213845/files/geneExp.rda"
#' destfile <- tempfile(fileext = ".rda")
#' download.file(url, destfile, mode = "wb")
#' load(destfile)
#' data(Panchy_et_al_E_signature)
#' Execute_AUCell(geneExp, geneList = Panchy_et_al_E_signature, colnames = "Escore")
Execute_AUCell <- function(geneExp,geneList,colnames )
{
  
  genes = unlist(geneList$GeneName)
  AUCellMethod = AUCellfunc(t(geneExp),genes)
  if(nrow(AUCellMethod) == ncol(t(geneExp)))
  {
    Samples = AUCellMethod$SampleID
    AUCellMethod = data.frame(AUCellMethod[,-1])
    AUCellMethod = data.frame(t(AUCellMethod))
    names(AUCellMethod) = Samples
    result = data.frame(t(AUCellMethod))
    colnames(result) <- colnames
    return(result)
  }
}
