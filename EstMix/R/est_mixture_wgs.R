#' It is a function that takes the count data obtained from whole genome sequencing (WGS) data and returns the estimated tumor and normal proportions. Currently, the function can performs the proportion estimations by assuming the number of tumor clones to be 1 or 2. The normalization step is not required and the normalization constant will be returned by this function. The function will output two sets of solutions corresponding to the top 2 optimal solutions based on the posterior distribution. You can choose according to your expertise the one that is more reasonable.
#' @usage est_mixture_wgs(exp_data, normal_snp, tumor_snp, f_path, num_tumor = 1)
#' @param exp_data a string. It provides the file name of interval. exp_data.intervals should be the name of the interval file. For the format of this file, please see the example section. The file should contain 6 and only 6 columns with each column corresponds to "ID","chrm","start","end","tumorCount" and "normalCount". It is very important to keep the order of the columns the same as listed.
#' @param normal_snp a string. It provides the file name of WGS count data for a normal sample or a control sample.
#' @param tumor_snp a string. It provides the file name of WGS count data for the tumor sample.
#' @param f_path a string. It provides the absolute path of the folder that contains the files above.
#' @param num_tumor 1 or 2, indicating the number of tumor clones. 1 indicates a mixture for a normal and one tumor clone. 2 indicates a mixture for a normal and 2 tumors and so on. Default value is set to be 1.
#' @return \item{sol1_pct}{the estimated percentages for all tumor clones for optimal solution 1. Each value is between 0 and 100.}
#' \item{sol1_scale}{sol1_scale: a scaler that provide the normalization constant for LRR for optimal solution 1. That is 2*tumor_count/normal_count will be on the same scale as the copy number.}
#' \item{sol1_cn1}{a vector of length S, where S is the number of segments. It is the estimated copy number for tumor 1 for the optimal solution.}
#' \item{sol1_cn2}{a vector of length S, where S is the number of segments. It is the estimated copy number for tumor 2 for the optimal solution. }
#' \item{sol1_pscn1}{a vector of length S, where S is the number of segments. It is the estimated parent specifit copy number for tumor 1 for the optimal solution.}
#' \item{sol1_pscn2}{a vector of length S, where S is the number of segments. It is the estimated parent specifit copy number for tumor 2 for the optimal solution.}
#' \item{sol2_pct}{the estimated percentages for all tumor clones for optimal solution 2. Each value is between 0 and 100.}
#' \item{sol2_scale}{sol1_scale: a scaler that provide the normalization constant for LRR for optimal solution 2. That is 2*tumor_count/normal_count will be on the same scale as the copy number.}
#' \item{sol2_cn1}{a vector of length S, where S is the number of segments. It is the estimated copy number for tumor 1 for the second optimal solution.}
#' \item{sol2_cn2}{a vector of length S, where S is the number of segments. It is the estimated copy number for tumor 2 for the second optimal solution.}
#' \item{sol2_pscn1}{a vector of length S, where S is the number of segments. It is the estimated parent specifit copy number for tumor 1 for the second optimal solution.}
#' \item{sol2_pscn2}{a vector of length S, where S is the number of segments. It is the estimated parent specifit copy number for tumor 2 for the second optimal solution.}
#' @examples
#' exp_data = "data_exp_eg" ## exp_data.intervals should be the file name of the segments.
#' ## For the format of the input files, you can use the example code below.
#' normal_snp = "snp_norm_eg" ## snp_norm_eg.txt should be the count file name for the normal sample.
#' tumor_snp = "snp_tum_eg" ## snp_tum_eg.txt should be the count file name for the tumor sample.
#' f_path = system.file("extdata",package="EstMix")
#' ## f_path should be the absolute path of folder that contains the txt and interval files.
#' out_wgs = est_mixture_wgs(exp_data, normal_snp, tumor_snp,f_path,num_tumor = 1)
#' out_wgs$sol1_pct
#' out_wgs$sol1_scale
#'
#' ## for the format of the input files, please see the following code
#' data_exp_path = file.path(f_path, paste("/", exp_data, ".intervals", sep=""))
#' snp_norm_path = file.path(f_path, paste("/",normal_snp, ".txt", sep=""))
#' snp_tumor_path = file.path(f_path, paste("/",tumor_snp, ".txt", sep=""))
#' data_exp = read.table(data_exp_path);
#' colnames(data_exp) = c("ID","chrm","start","end","tumorCount","normalCount")
#' snp_norm = read.table(snp_norm_path)
#' snp_tum = read.table(snp_tumor_path)
#'
#' ## References: Quantification of multiple tumor clones using gene array and sequencing data.
#' ## Y Cheng, JY Dai, TG Paulson, X Wang, X Li, BJ Reid, C Kooperberg.
#' ## Annals of Applied Statistics 11 (2), 967-991
#' @export
est_mixture_wgs <- function(exp_data, normal_snp, tumor_snp, f_path, num_tumor = 1){
  #source("./R/calc_1d_wgs.R")
  #source("./R/calc_2d_wgs.R")

  options(warn=-1)
  ####### preprocessing #######
  tmp <- preprocessing(exp_data, normal_snp, tumor_snp, f_path)
  baf <- tmp$baf
  lrr <- tmp$lrr
  nrc <- tmp$nrc
  n_baf <- tmp$n_baf

  if (num_tumor == 1){
    res <- calc_1d_wgs(baf, lrr, n_baf, nrc)
  }
  else if (num_tumor == 2){
    res <- calc_2d_wgs(baf, lrr, n_baf, nrc)
  }
  else{
    res <- "Error. Please retry and input num_tumor as 1 or 2"
  }
  return (res)
}
