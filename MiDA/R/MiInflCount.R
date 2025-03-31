#' Mean microarray probes' feature importance from binary classification
#'
#' Counts mean of probes' feature importance for multiple models of binary classification built
#' on microarray gene/transcript expression data
#'
#' @param importance.list a list of data frames, containing the result of binary classification:
#'probe IDs in first column and probe's feature importance (relative influence) in the second column
#'in the order from most important to the least important for classification.
#'Such list is the \code{\link{MiBiClassGBODT}} output (\code{Importance}).
#'
#'@details This function takes the result of binary classification performed with cross-validation
#'and counts mean of each probe's feature importance (relative influence) gained in all fitted models.
#'
#'@return a list of 2
#'\cr
#'\code{data.mean} - data frame of probe names (in alphabetical order), their mean feature importance
#'and standard deviation.
#'\cr
#'\code{data.importance} - data frame of probe IDs (in alphabetical order) and their
#' original feature importance values gained in all cross-validation models.
#'
#'@examples
#'
#'# get gene expression and specimen data
#' data("IMexpression");data("IMspecimen")
#' # sample expression matrix and specimen data for binary classification,
#' # only "NORM" and "EBV" specimens are left
#' SampleMatrix<-MiDataSample(IMexpression, IMspecimen$diagnosis,"norm", "ebv")
#' SampleSpecimen<-MiSpecimenSample(IMspecimen$diagnosis, "norm", "ebv")
#' #Fitting, low tuning for faster running
#' ClassRes<-MiBiClassGBODT(SampleMatrix, SampleSpecimen, n.crossval = 3,
#'                         ntrees = 10, shrinkage = 1, intdepth = 2)
#' # List of influence data frames for all 3 models build using cross-validation
#' # is the 2nd element of BiClassGBODT results
#' Importances<-MiInflCount(ClassRes[[2]])
#' Importances[[1]][1:10,] # mean and sd. 0s are for low feature importance
#' Importances[[2]][1:10,] # original values for n.crossval = 3 models
#'
#'@seealso \code{\link{MiBiClassGBODT}}
#'
#'@author Elena N. Filatova
#'
#'@export

MiInflCount <- function (importance.list){
  names <- rownames(importance.list[[1]])
  names <- sort(names, decreasing = F) # genes in alphabetical order
  data.infl <- data.frame(names)
  infl.int <- c()
  for (i in 1:length(importance.list)){
    infl.int <- importance.list[[i]][sort(rownames(importance.list[[i]]), decreasing = F), 2] # influence for genes in alphabetical order
    data.infl <- cbind(data.infl, infl.int) # make data of influence from all model trials
  }
  colnames(data.infl) <- c("probeID", paste("infl", 1:(ncol(data.infl)-1)))
  data.mean <- data.frame(names, "infl.mean" = apply(data.infl[, 2:ncol(data.infl)], MARGIN = 1, mean), # count mean and sd
                        "infl.sd" = apply(data.infl[, 2:ncol(data.infl)], MARGIN = 1, stats::sd))
  infl.res <- list("data.mean"=data.mean, "data.importance"=data.infl) # result list with statistics and importances
  return(infl.res)
}
