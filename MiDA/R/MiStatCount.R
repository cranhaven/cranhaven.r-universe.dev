#' FDR for microarray gene expression data
#'
#' Performs descriptive statistics and FDR (False Discovery Rate) test for microarray expression matrix
#'
#'@param Matrix numeric matrix of expression data where each row corresponds to a probe (gene, transcript),
#'  and each column correspondes to a specimen (patient).
#'@param specimens factor vector with two levels specifying specimens in the columns of the \code{Matrix}.
#'
#'@details This function takes matrix of expression data and performs T-test with FDR correction for two groups
#'for each probe.
#'\cr
#'T-test is a two-sided, two-class with equal variances against the null hypothesis 'mean1=mean2' for each row.
#' See \code{\link{rowttests}} for details.
#'
#'@return a data frame containing for each probe: mean and sd values for both groups, difference of means,
#'p-value for T-test and q-value for FDR (False Discovery Rate) correction.

#'
#' @examples
#' data("IMexpression"); data("IMspecimen") # load data and specimen information
#' #sampling data and specimen information
#' ExpData<-MiDataSample(IMexpression, IMspecimen$diagnosis,"ebv", "norm")
#' Specimens<-MiSpecimenSample(IMspecimen$diagnosis, "ebv", "norm")
#' #Counting statistics
#' StatRes<-MiStatCount(ExpData, Specimens)
#' head(StatRes)
#'
#'@author Elena N. Filatova
#'
#'@references Welch B.L.(1951) On the comparison of several mean values: an alternative approach. Biometrika 38, 330-336.
#'\url{https://doi.org/10.1093/biomet/38.3-4.330}
#'
#'@seealso \code{\link{rowttests}}
#'
#'@export


MiStatCount <- function (Matrix, specimens){
  Matrix.ordered <- Matrix[sort(rownames(Matrix), decreasing = F),] # genes in alphabetical order
  sample.names <- levels(specimens) # two groups to compare
  mean1 <- apply(Matrix.ordered[, specimens == sample.names[1]], 1, mean); # means and sds
  sd1 <- apply(Matrix.ordered[, specimens == sample.names[1]], 1, stats::sd)
  mean2 <- apply(Matrix.ordered[, specimens == sample.names[2]], 1, mean);
  sd2 <- apply(Matrix.ordered[, specimens == sample.names[2]], 1, stats::sd)
  ttest.res <- genefilter::rowttests(Matrix.ordered, specimens) # ttest
  ttest.p <- ttest.res$p.value; ttest.p<-round(ttest.p, digits=5)
  FDR.q <- stats::p.adjust(ttest.p, method = "fdr") # FDR correction
  FDR.q<-round(FDR.q, digits=5)
  diff.means<-ttest.res$dm
  data.res <- data.frame(rownames(Matrix.ordered), mean1, sd1, mean2, sd2, diff.means, ttest.p, FDR.q) # result data
  column.names <- c("names", paste(c("mean", "sd"), sample.names[1], sep="."),
                  paste(c("mean", "sd"), sample.names[2], sep = "."),
                  paste("diff.means.", sample.names[1], "-", sample.names[2], sep = ""),
                  "ttest.p", "FDR.q")
  colnames(data.res) <- column.names
  return(data.res)
}
