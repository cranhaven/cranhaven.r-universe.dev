#' Preprocessing data
#'
#' Takes \code{exp_data}, \code{normal_snp}, and \code{tumor_snp}
#' @param exp_data a string, file name of xxx
#' @param normal_snp a string, file name of xxx
#' @param tumor_snp a string, file name of xxx
#' @param f_path a string, file path of the files above
#' @return \item{df}{a dataframe containing lrr, nrc, baf and n_baf }
#' @keywords internal
#' @export
preprocessing <- function(exp_data, normal_snp, tumor_snp, f_path){

  data_exp_path = file.path(f_path, paste("/", exp_data, ".intervals", sep=""))
  snp_norm_path = file.path(f_path, paste("/",normal_snp, ".txt", sep=""))
  snp_tumor_path = file.path(f_path, paste("/",tumor_snp, ".txt", sep=""))
  data_exp = read.table(data_exp_path)
  snp_norm = read.table(snp_norm_path)
  snp_tum = read.table(snp_tumor_path)

  data_sel = data_exp[data_exp[,6]>80&data_exp[,5]>80,]
  lrr = log(data_sel[,5]/data_sel[,6])
  nrc = data_sel[,6]/2
  baf_norm = snp_norm[,4]/(snp_norm[,3]+snp_norm[,4])
  baf_tum = snp_tum[,4]/(snp_tum[,3]+snp_tum[,4])
  id = which(baf_norm<.8&baf_norm>.2)
  BAF = cbind(snp_tum[id,1:2],baf_tum[id]/baf_norm[id]/2,snp_tum[id,3:4])
  baf = c();n_baf = c()
  for(i in 1:nrow(data_sel)){
    chr = data_sel[i,2]
    st = data_sel[i,3];end = data_sel[i,4]
    id_temp = which(BAF[,1]==chr&BAF[,2]<=end&BAF[,2]>=st)
    temp = BAF[id_temp,]
    beta = temp[,3];beta=beta[!is.na(beta)]
    baf[i] = mean(beta)
    n_baf[i] = nrow(temp)
  }
  scale_max = mean(exp(lrr))*2
  scale_min = mean(exp(lrr))/3
  proc.fn = file.path(f_path, paste("output", ".RData", sep=""))
  df = data.frame(lrr,nrc,baf,n_baf)
  return (df)
}
