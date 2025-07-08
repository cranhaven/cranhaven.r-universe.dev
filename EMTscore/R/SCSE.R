library(stringr)
library(foreach)
library(doParallel)
library('GSA')         ## for calling GSA.read.gmt function

################ SCSE function ############ 

## Function Input: single-cell gene expression data and genes in one gene sets (This function is implemented using the equation described in Pont et al.,2019)

## Function Output: Scores for each cell/sample ID

################################## SCSE function #########################################################

### SingleCellSigExplorer function in SCSE
## This function is called by SCSE function to compute ranks of each cell for a gene signature

#' SingleCellSigExplorer
#' 
#' A function to print the words "New function!"
#' @param data A numeric matrix where rows are genes and columns are cells or samples.
#' @param genes A character vector of gene names to include in the signature.
#'
#' @return A character vector
#' @export
#'

SingleCellSigExplorer <- function(data,genes)
{
  DataRanks = data[which(rownames(data) %in% genes),]
  if(length(nrow(DataRanks))!=0)
  {
    CumSum = data.frame(colSums(DataRanks, na.rm = FALSE, dims = 1))
    colnames(CumSum)[1]='RawRankSum'
    SampleID = rownames(CumSum)
    CumSum1 = data.frame(SampleID,CumSum)
    row.names(CumSum1)=NULL
    TotalUMICount = data.frame(colSums(data, na.rm = FALSE, dims = 1))
    colnames(TotalUMICount)[1]='TotalUMISum'
    SampleID = rownames(TotalUMICount)
    TotalUMICount = data.frame(SampleID,TotalUMICount)
    row.names(TotalUMICount)=NULL
    FinalScore = merge(CumSum1, TotalUMICount, by='SampleID')
    FinalScore$SingleScorer = FinalScore$RawRankSum/FinalScore$TotalUMISum*100
    colnames(FinalScore)[4] = 'scSigExp'
    FinalScore = FinalScore[,c(1,4)]
    return(FinalScore)
  } 
}

############### Calling SCSE function using gene sets #########################

## Function Input: single-cell gene expression data, all gene sets in a list, and k represents the index for a gene set in a list

## Function Output: A dataframe containing sample/cell ID, gene set score and pathwayName

### Execute_SCSE function in SCSE
## This function is called by SCSE function to compute ranks of each cell for a gene signature


#' SCSE methods to calculate EMT score
#' 
#' @param geneExp A numeric matrix of gene expression values, where rows represent genes and columns represent samples/cells.
#' @param geneList A list of signature gene sets, with each element containing gene names for the respective signature.
#' @param colnames A character string representing the name to assign to the column containing the EMT scores in the output data frame.
#'
#' @return A data frame containing sample/cell ID and their respective EMT scores.
#' @export
#'
#' @examples
#' library(curl)
#' url <- "https://zenodo.org/record/15213845/files/geneExp.rda"
#' destfile <- tempfile(fileext = ".rda")
#' download.file(url, destfile, mode = "wb")
#' load(destfile)
#' data(Panchy_et_al_E_signature)
#' Execute_SCSE(geneExp, geneList = Panchy_et_al_E_signature,colnames = "Escore")

Execute_SCSE <- function(geneExp,geneList,colnames)
{
  genes = unlist(geneList$GeneName)
  SCSE = SingleCellSigExplorer(t(geneExp),genes)
  if(length(SCSE)!=0)
  {
    Samples = SCSE$SampleID
#    SCSE = data.frame(SCSE[,-1])
    SCSE = data.frame(SCSE)
    rownames(SCSE)=SCSE[,1]  #取出第一列
    #SCSE=SCSE[,-1]
    SCSE[,1] <- NULL
    colnames(SCSE) <- colnames
    return(SCSE)
  }
}
