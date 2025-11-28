#' Return mixture estimation for percentage of normal cells and tumor (1 normal + 1 tumor) with wgs data
#' Takes \code{baf}, \code{lrr}, \code{n_baf} and \code{nrc}
#' @param baf a numeric vector. Each element is the mean adjusted B allele frequency for that segment, calculated as the mean of baf_tumor/baf_normal/2 for that segment
#' @param lrr a numeric vector. Each element is the log ratio of the tumor read count and the normal read count for a segment, defined as log(tumorCount/normalCount)
#' @param n_baf a numeric vector
#' @param nrc a numeric vector. Each element is the normal read count of the segment divided by two
#' @return \item{sol1}{a numeric number. It provides the estimated percentages of normal from the best solution. The number is the percentage of the estimated normal percentage.}
#' \item{sol2}{a numeric number. It provides the estimated percentages of normal from the second best solution. The number is the percentage of the estimated normal percentage.}
#' @keywords internal
#' @export
calc_1d_wgs <- function(baf, lrr, n_baf, nrc){

  #source("./R/sel_scale_1d_wgs.R")
  #sourceCpp("./src/calcll_1d_baf.cpp")

  #2D prior on pscn -- defined as lprior_f_2d
  tt = exp(-(0:6-1)^2);prior_f = tt%*%t(tt);lprior_f_2d = -log(prior_f)*1e-3
  rtt = exp(-(6:0-1)^2);rprior_f = rtt%*%t(rtt);rlprior_f_2d = -log(rprior_f)*1e-3

  ##proc.fn = paste("wgs.rdata")
  ##save(baf, lrr, n_baf, nrc, lprior_f_2d, rlprior_f_2d, file=proc.fn)
  temp <- data.frame(baf, lrr, n_baf, nrc)

  res=sel_scale_1d_wgs(temp = temp)
  n_seg = length(baf)
  L_tot_1d_scale1 = matrix(0,100,n_seg)
  L_tot_1d_scale2 = matrix(0,100,n_seg)
  CN_sol1 = matrix(0,100,n_seg)
  CN_sol2 = matrix(0,100,n_seg)
  PSCN_sol1 = matrix(0,100,n_seg)
  PSCN_sol2 = matrix(0,100,n_seg)

  ## find the 1st optimal solution
  for(i in which(!is.na(baf))){
    res1 = calcll_1d_baf(lrr[i],nrc[i],baf[i],n_baf[i],lprior_f_2d*0,rlprior_f_2d*0,res$scale1d[1],6,6)
    L_tot_1d_scale1[,i] = res1[[1]]
    CN_sol1[,i] = res1[[2]]
    PSCN_sol1[,i] = res1[[3]]
  }
  L_sum_1d.1 = apply(L_tot_1d_scale1[,which(!is.na(baf))],1,sum)*80
  sol1_pct = which(L_sum_1d.1<min(L_sum_1d.1)+.1,arr.ind=TRUE) - 1
  sol1_scale = res$scale1d[1]
  sol1_cn = CN_sol1[sol1_pct, ]
  sol1_pscn = PSCN_sol1[sol1_pct, ]

  ## find the 2nd optimal solution
  for(i in which(!is.na(baf))){
    res2 = calcll_1d_baf(lrr[i],nrc[i],baf[i],n_baf[i],lprior_f_2d*0,rlprior_f_2d*0,res$scale1d[2],6,6)
    L_tot_1d_scale2[,i] = res2[[1]]
    CN_sol2[,i] = res2[[2]]
    PSCN_sol2[,i] = res2[[3]]
  }
  L_sum_1d.2 = apply(L_tot_1d_scale2[,which(!is.na(baf))],1,sum)*80
  sol2_pct = which(L_sum_1d.2<min(L_sum_1d.2)+.1,arr.ind=TRUE) - 1
  sol2_scale = res$scale1d[2]
  sol2_cn = CN_sol2[sol2_pct, ]
  sol2_pscn = PSCN_sol2[sol2_pct, ]

  return (list(sol1_pct = sol1_pct, sol1_scale = sol1_scale, sol1_cn1 = sol1_cn, sol1_pscn1 = sol1_pscn, sol2_pct = sol2_pct, sol2_scale = sol2_scale, sol2_cn1 = sol2_cn, sol2_pscn1 = sol2_pscn))
}
