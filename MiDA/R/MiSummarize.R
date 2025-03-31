#' Microarray data summarization
#'
#' Counts median of intensities for multiple probes that target one gene/transcript.
#'
#'@param Matrix numeric matrix of intensities data where each row corresponds to a probe (gene, transcript),
#'and each column correspondes to a specimen (patient).
#'Row names of \code{Matrix} should contain probe IDs that consist of three terms:
#'gene name - transcript name - probe name.
#'@param sep a character string to separate the terms in probe IDs.
#'@param method character string specifying summarization method. Possible values are "median" and "mean".
#'
#'@details This function is used for summarizing expression intensities data when multiple probes target
#'one gene/transcript. Row names of \code{Matrix} should contain probe IDs that consist of 3 terms:
#'"Gene name - sep - transcript name - sep - probe name" (for example, "AGTR2.ALL" - for gene, only one probe;
#'"AGTR2.NM_000686.z1" - 1st probe to AGTR2 NM_000686 mRNA transcript).
#'
#' @return gene/transcript expression matrix with median/mean of expression intensities for each gene/transcript.
#'
#' @examples
#' data("IMexpression") # load data
#' # See 5 zonds to AGTR2.NM_000686
#' IMexpression [1:10, 1:5]
#' SumMatrix<-MiSummarize(IMexpression, sep=".")
#' # now there is median expression for AGTR2.NM_000686
#' SumMatrix[ 1:10, 1:5]
#'
#' @author Elena N. Filatova
#'
#' @export

MiSummarize <- function (Matrix, sep, method="median"){
  #make unique gene names
  #row names of  Matrix are probe IDs (Gene.transcript.zond - "CAD.NM_00015634.z1")
  Names.split <- strsplit(rownames(Matrix), split = sep, fixed = T) # split each gene name for 3 character
  Names.short <- c()
  for (i in 1:length(rownames(Matrix))){Names.short[i] <- paste(Names.split[[i]][1], Names.split[[i]][2], sep = sep)} # combine first two
  Names.short <- unique(Names.short) # take away repeats
  #summarize by median
  DataMat <- as.data.frame(Matrix)
  data.res <- data.frame()
  if (method=="median"){
    for (i in 1:length(Names.short)){
      ids <- grep(Names.short[i], rownames(DataMat))
      int.data <- DataMat[ids,] # make data with all zonds for 1 gene
      res <- apply(int.data, MARGIN = 2, stats::median) # count median
      data.res <- rbind(data.res, res)
    }
  }
  if (method=="mean"){
    for (i in 1:length(Names.short)){
      ids <- grep(Names.short[i], rownames(DataMat))
      int.data <- DataMat[ids,] # make data with all zonds for 1 gene
      res <- apply(int.data, MARGIN = 2, mean) # count median
      data.res <- rbind(data.res, res)
    }
  }
  data.res <- as.matrix(data.res) # make result matrix
  colnames(data.res) <- colnames(Matrix)
  rownames(data.res) <- Names.short
  return(data.res)
}
