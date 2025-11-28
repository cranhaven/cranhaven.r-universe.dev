#' It is a function that takes the LRR obtained from SNP array data and returns the estimated tumor and normal proportions. Currently, the function can performs the proportion estimations by assuming the number of tumor clones to be 1 or 2 or 3. The normalization step is not required and the normalization constant will be returned by this function. The function will output two sets of solutions corresponding to the top 2 optimal solutions based on the posterior distribution. You can choose according to your expertise the one that is more reasonable.
#' @usage est_mixture(BAF, LRR, chr, x, GT, seg_raw = "NA", num_tumor = 1)
#' @param BAF a numeric vector containing the B Allele Frequency for the sample, corresponding to the location (chr, x).
#' @param LRR a numveric vector containing the Log R ratio for the sample, corresponding to the location (chr, x). In practice, the LRR values you include should be the raw LRR output devided by 0.55.
#' @param chr a factor vector containing the chromosome.
#' @param x a numeric vector containing the location on the chromosome, measured by base pair.
#' @param GT a factor vector containing the genotype. Possible values are "AA", "AB", "BB" and NA.
#' @param seg_raw Optional. A dataframe containing the segmentaiton results. If not supplied, function \code{segmentByPairedPSCBS} from package PSCBS will be used to obtain the segmentation. You can also use the \code{segmentByPairedPSCBS} function to preprocess your data set and obtain the segmentation results and use that and the input. (On examples about how to obtain the segmentation results beforehand, please see the examples section below.)
#' @param num_tumor 1 or 2 or 3, indicating the number of tumor clones. 1 indicates a mixture for a normal and one tumor clone. 2 indicates a mixture for a normal and 2 tumors and so on. Default value is set to be 1.
#' @return \item{sol1_pct}{the estimated percentages for all tumor clones for optimal solution 1. Each value is between 0 and 100.}
#' \item{sol1_scale}{a scaler that provide the normalization constant for LRR for optimal solution 1. That is 2*2^LRR/scale will be on the same scale as the copy number.}
#' \item{sol1_cn1}{a vector of length S, where S is the number of segments. It is the estimated copy number for tumor 1 for the optimal solution.}
#' \item{sol1_cn2}{a vector of length S, where S is the number of segments. It is the estimated copy number for tumor 2 for the optimal solution. }
#' \item{sol1_pscn1}{a vector of length S, where S is the number of segments. It is the estimated parent specifit copy number for tumor 1 for the optimal solution.}
#' \item{sol1_pscn2}{a vector of length S, where S is the number of segments. It is the estimated parent specifit copy number for tumor 2 for the optimal solution.}
#' \item{sol2_pct}{the estimated percentages for all tumor clones for optimal solution 2. Each value is between 0 and 100.}
#' \item{sol2_scale}{a scaler that provide the normalization constant for LRR for optimal solution 2. That is 2*2^LRR/scale will be on the same scale as the copy number.}
#' \item{sol2_cn1}{a vector of length S, where S is the number of segments. It is the estimated copy number for tumor 1 for the second optimal solution.}
#' \item{sol2_cn2}{a vector of length S, where S is the number of segments. It is the estimated copy number for tumor 2 for the second optimal solution.}
#' \item{sol2_pscn1}{a vector of length S, where S is the number of segments. It is the estimated parent specifit copy number for tumor 1 for the second optimal solution.}
#' \item{sol2_pscn2}{a vector of length S, where S is the number of segments. It is the estimated parent specifit copy number for tumor 2 for the second optimal solution.}
#' @examples
#' ##########################################################
#' ##
#' ## short example
#' ##
#' #########################################################
#' ## first load the data
#' BAF <- example_data$BAF
#' LRR <- example_data$LRR ## In practice, the orignal LRR should be devided by 0.55
#' chr <- example_data$chr
#' loc <- example_data$x
#' GT <- example_data$GT
#' gt = (GT=='BB')*2+(GT=='AB')*1.5+(GT=='AA')-1;gt[gt==(-1)]=NA
#'
#' ## then perform segmentation
#' gaps = PSCBS::findLargeGaps(x=loc,minLength=5e6,chromosome=chr)
#' if(!is.null(gaps)) knownSegments = PSCBS::gapsToSegments(gaps)
#' p <- 0.0001
#' fit <- PSCBS::segmentByPairedPSCBS(CT=2*2^LRR,betaT=BAF,muN=gt,chrom=chr,
#' knownSegments=knownSegments,tbn=FALSE,x=loc,seed=1, alphaTCN=p*.9,alphaDH=p*.1)
#' seg_eg = fit$output
#'
#' ## then perform tumor mixture estimation by assuming 1 tumor clones
#' out = est_mixture(BAF, LRR, chr, loc, GT, num_tumor = 1, seg_raw = seg_eg)
#' out$sol1_pct
#' out$sol1_scale

#' ## References: Quantification of multiple tumor clones using gene array and sequencing data.
#' ## Y Cheng, JY Dai, TG Paulson, X Wang, X Li, BJ Reid, C Kooperberg.
#' ## Annals of Applied Statistics 11 (2), 967-991
#' ## Segmentation-based detection of allelic imbalance and loss-of-heterozygosity
#' ## in cancer cells using whole genome SNP arrays.
#' ## J Staaf, D Lindgren, J Vallon-Christersson, A Isaksson, H Goransson, G Juliusson,
#' ## R Rosenquist, M H, A Borg, and M Ringner
#' @export
est_mixture <- function(BAF, LRR, chr, x, GT, seg_raw = "NA", num_tumor = 1){
  #source("./R/calc_1d.R")
  #source("./R/calc_2d.R")
  options(warn=-1)

  if(length(seg_raw)==1){
    if(seg_raw == "NA"){
      seg_raw <- get_segmentation(BAF, LRR, chr, x, GT)
    }
  }

  if (num_tumor == 1){
    res <- calc_1d(BAF, LRR, chr, x, GT, seg_raw = seg_raw)
  }
  else if (num_tumor == 2){
    res <- calc_2d(BAF, LRR, chr, x, GT, seg_raw = seg_raw)
  }
  else if (num_tumor == 3){
    res <- calc_3d(BAF, LRR, chr, x, GT, seg_raw = seg_raw)
  }
  else{
    res <- "Error. Please retry and input num_tumor as 1, 2 or 3"
  }
  return (res)
}
