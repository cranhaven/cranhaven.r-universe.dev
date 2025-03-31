#' Microarray data normalization
#'
#'
#'Normalizes microarray expression intensities using different methods with or without background correction.
#'
#'
#'@param Matrix numeric matrix of intensities data where each row corresponds to a probe (gene, transcript),
#'  and each column correspondes to a specimen (patient).
#' @param posNC numeric vector specifying numbers of rows containing  negative controls (non-coding areas).
#' Used for \code{method="SQN"} only.
#' Rows with negative controls will be removed from an intensity matrix after the normalization if \code{leaveNC=FALSE}.
#' @param method character string specifying normalization method. Possible values are:
#'\code{"none"} (no normalization)
#'\code{"center"} (subtracting the row mean),
#'\code{"scale"} (dividing by row standard deviation),
#'\code{"standardize"} (subtracting the row mean and dividing by row standard deviation - z-score transformation),
#'\code{"range"} (ranges from 0 to 1),
#'\code{"QN"} (normalization based upon quantiles),
#'\code{"SQN"} (subset quantile normalization using negative control features),
#'\code{"Loess"} (cyclicly applying loess normalization).
#' @param leaveNC logical value indicating whether rows with negative control should be deleted
#' from intensity matrix after normalization.
#' @param BGcor logical value indicating whether background correction should be done before normalization.
#' Could be used for background correction only (without data normalization) if \code{method="none"}.
#'
#'
#'@details This function is intended to normalize microarray intensities data between arrays.
#'Background correction is optional.
#' \cr
#' Background correction method is "normexp",  which is based on a convolution model (Ritchie, 2007).
#' See \code{\link{backgroundCorrect}} for details.
#' \cr
#' Quantile normalization method implies that we can give each array the same distribution
#' See \code{\link{normalize.quantiles}} for details.
#' \cr
#' Subset quantile normalization is performed based on a subset of negative (or non-coding) controls
#' according to (Wu and Aryee, 2010). Number of normal distributions in the mixture approximation is 5,
#' weight given to the parametric normal mixture model is 0.9. See \code{\link{SQN}} for details.
#' \cr
#' Cyclic loess normalization implements method of Ballman et al (2004),
#' whereby each array is normalized to the average of all the arrays.
#' See \code{\link{normalizeCyclicLoess}} for details.
#'
#'
#' @return A matrix of the same dimensions as \code{Matrix} containing normalized values with or without
#' background correction. If \code{leaveNC=FALSE} the function returns a matrix with
#' normalized values without rows containing negative controls.
#'
#'
#' @examples
#' data("IMexpression")
#' # Loess normalization
#' LoMatrix<-MiNorm(IMexpression, method="Loess")
#' par(mfrow=c(1,2))
#' boxplot(log2(IMexpression),main="Before normalization")
#' boxplot(log2(LoMatrix),main="Loess normalization")
#' par(mfrow=c(1,1))
#'
#'
#' @author Elena N. Filatova
#'
#'
#' @references
#' Ballman K.V., Grill D.E., Oberg A.L. and Therneau T.M. (2004). Faster cyclic loess: normalizing RNA arrays via linear models. Bioinformatics 20, 2778-2786.
#' \url{https://doi.org/10.1093/bioinformatics/bth327}
#' \cr
#' \cr
#' Bolstad B.M., Irizarry R.A., Astrand M. and Speed T.P. (2003) A Comparison of Normalization Methods for High Density Oligonucleotide Array Data Based on Bias and Variance. Bioinformatics 19(2), 185-193.
#' \url{https://doi.org/10.1093/bioinformatics/19.2.185}
#'\cr
#'\cr
#'Ritchie M.E., Silver J., Oshlack A., Silver J., Holmes M., Diyagama D., Holloway A. and Smyth G.K. (2007). A comparison of background correction methods for two-colour microarrays. Bioinformatics 23, 2700-2707.
#'\url{https://doi.org/10.1093/bioinformatics/btm412}
#'\cr
#'\cr
#'Wu Z and Aryee M. (2010). Subset Quantile Normalization using Negative Control Features. Journal of Computational Biology 17(10), 1385-1395.
#'\url{https://doi.org/10.1089/cmb.2010.0049}
#'
#'
#'@seealso \code{\link{backgroundCorrect}}, \code{\link{normalizeCyclicLoess}},
#'\code{\link{normalize.quantiles}}, \code{\link{SQN}}
#'
#'
#' @export




MiNorm <- function(Matrix, posNC, method="none", leaveNC=TRUE, BGcor=FALSE){
  if (BGcor == TRUE){ # background correction
    Matrix <- limma::backgroundCorrect(Matrix, method="normexp")
  }
  normatrix <- Matrix # matrix of soon-to-be normalized data, copied for method="none"
  if (method == "SQN"){ # normalization with all methods
    normatrix <- SQN::SQN(Matrix, ctrl.id=posNC)
  }
  if (method == "center"){
    meanss <- apply(Matrix, MARGIN = 1, mean)
    normatrix <- (Matrix-meanss)
  }
  if (method == "scale"){
    sdss <- apply(Matrix, MARGIN = 1, stats::sd)
    normatrix <- (Matrix/sdss)
  }
  if (method == "standardize"){
    meanss <- apply(Matrix, MARGIN = 1, mean)
    sdss <- apply(Matrix, MARGIN = 1, stats::sd)
    normatrix <- ((Matrix-meanss)/sdss)
  }
  if (method == "range"){
    minss <- apply(Matrix, MARGIN = 1, min)
    maxss <- apply(Matrix, MARGIN = 1, max)
    normatrix <- ((Matrix-minss)/(maxss-minss))
  }
  if (method == "QN"){
    normatrix <- preprocessCore::normalize.quantiles(Matrix, copy=F)
  }
  if (method == "Loess"){
    normatrix <- limma::normalizeCyclicLoess(Matrix, method = "fast")
  }
  rownames(normatrix)<-rownames(Matrix)
  if (leaveNC == FALSE){ # take away non-coding rows
    normatrix <- normatrix[-posNC, ]
  }
  return(normatrix)
}
