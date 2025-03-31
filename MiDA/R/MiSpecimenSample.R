#' Select values from factor vector
#'
#' This function takes factor vector with multiple levels and selects values for 1 or 2 levels only.
#'
#'@param x factor vector
#'@param group1 value of factor level to sample
#'@param group2 additional value of factor level to sample
#'
#'@details
#' This function is ment for sampling specimens for binary classification when they belong to more
#' than two groups. The aim is to create factor vector with only two levels specifying specimens
#' in the columns of corresponding microarray expression matrix.
#' It should be used together with \code{\link{MiDataSample}} that samples columns of microarray
#' expression matrix in the same way.
#'
#'@return factor vector with values for 1 or 2 levels only
#'
#'@examples
#' #get gene expression and specimen data
#' data("IMexpression");data("IMspecimen")
#' dim(IMexpression) # 100 columns (genes/transcripts) - 89 specimens
#' colnames(IMexpression)[1:10] # look at first 10 columns of matrix - specimens IDs
#' IMspecimen[1:10,] # specimens IDs and group factor - diagnoses in attendant vector
#' # note that specimens in matrix columns are in the same order as specimens in description data
#' # select specimens with only EBV and NORM diagnoses (and sample the description data as well)
#' SampleMatrix<-MiDataSample(IMexpression, IMspecimen$diagnosis, "ebv", "norm")
#' SampleSamples<-MiSpecimenSample(IMspecimen$diagnosis, "ebv", "norm")
#' dim(SampleMatrix)# only 68 specimens with EBV and NORM diagnoses left
#' colnames(SampleMatrix)[1:10]
#' SampleSamples[1:10] # corresponding diagnoses
#'
#'@author Elena N. Filatova
#'
#'@seealso \code{\link{MiDataSample}}
#'
#'@export

MiSpecimenSample <- function(x, group1, group2=NULL){
  colid <- x %in% c(group1, group2)
  samps <- x[colid]
  samps <- as.character(samps) # make factor with new levels
  samps <- as.factor(samps)
  return(samps)
}
