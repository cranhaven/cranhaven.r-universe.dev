#' Return mixture estimation for percentage of normal cells and tumor (1 normal + 2 tumors) with wgs data
#' Takes \code{baf}, \code{lrr}, \code{n_baf} and \code{nrc}
#' @param baf a numeric vector. Each element is the mean adjusted B allele frequency for that segment, calculated as the mean of baf_tumor/baf_normal/2 for that segment
#' @param lrr a numeric vector. Each element is the log ratio of the tumor read count and the normal read count for a segment, defined as log(tumorCount/normalCount)
#' @param n_baf a numeric vector
#' @param nrc a numeric vector. Each element is the normal read count of the segment divided by two
#' @return \item{sol1}{a numeric vector of length 2. It provides the estimated percentages of normal and tumor from the best solution. The first number is the percentage of the estimated normal percentage. The second number is the percentage of the estimated tumor 1 percentage}
#' \item{sol2}{a numeric vector of length 2. It provides the estimated percentages of normal and tumor from the second best solution. The first number is the percentage of the estimated normal percentage. The second number is the percentage of the estimated tumor 1 percentage}
#' @keywords internal
#' @export
calc_2d_wgs <- function(baf, lrr, n_baf, nrc){

  #source("./R/sel_scale_2d_wgs.R")
  #sourceCpp("./src/calcll_baf.cpp")

  #2D prior on pscn -- defined as lprior_f_2d
  tt = exp(-(0:6-1)^2);prior_f = tt%*%t(tt);lprior_f_2d = -log(prior_f)*1e-3
  rtt = exp(-(6:0-1)^2);rprior_f = rtt%*%t(rtt);rlprior_f_2d = -log(rprior_f)*1e-3

  ##proc.fn = paste("wgs.rdata")
  ##save(baf, lrr, n_baf, nrc, lprior_f_2d, rlprior_f_2d, file=proc.fn)
  temp <- data.frame(baf, lrr, n_baf, nrc)

  res=sel_scale_2d_wgs(temp = temp)
  n_seg = length(baf)
  L_tot_2d_scale1 = array(0,dim=c(n_seg,100,51))
  L_tot_2d_scale2 = array(0,dim=c(n_seg,100,51))
  CN1_sol1 = array(0,dim=c(n_seg,100,51))
  CN2_sol1 = array(0,dim=c(n_seg,100,51))
  CN1_sol2 = array(0,dim=c(n_seg,100,51))
  CN2_sol2 = array(0,dim=c(n_seg,100,51))
  PSCN1_sol1 = array(0,dim=c(n_seg,100,51))
  PSCN2_sol1 = array(0,dim=c(n_seg,100,51))
  PSCN1_sol2 = array(0,dim=c(n_seg,100,51))
  PSCN2_sol2 = array(0,dim=c(n_seg,100,51))

  ## find the 1st optimal solution
  for(i in which(!is.na(baf))){
    res1 = calcll_baf(lrr[i],nrc[i],baf[i],n_baf[i],lprior_f_2d*0,rlprior_f_2d*0,res$scale2d[1],6,6)
    L_tot_2d_scale1[i,,] = res1[[1]]
    CN1_sol1[i,,] = res1[[2]]
    CN2_sol1[i,,] = res1[[3]]
    PSCN1_sol1[i,,] = res1[[4]]
    PSCN2_sol1[i,,] = res1[[5]]
  }
  L_sum_2d.1 = apply(L_tot_2d_scale1[which(!is.na(baf)),,],c(2,3),sum)*80
  sol1_pct = which(L_sum_2d.1<min(L_sum_2d.1)+.000001,arr.ind=TRUE) - 1
  sol1_scale = res$scale2d[1]
  sol1_cn1 = CN1_sol1[,sol1_pct[1],sol1_pct[2]]
  sol1_cn2 = CN2_sol1[,sol1_pct[1],sol1_pct[2]]
  sol1_pscn1 = PSCN1_sol1[,sol1_pct[1],sol1_pct[2]]
  sol1_pscn2 = PSCN2_sol1[,sol1_pct[1],sol1_pct[2]]

  ## find the 2nd optimal solution
  for(i in which(!is.na(baf))){
    res2 = calcll_baf(lrr[i],nrc[i],baf[i],n_baf[i],lprior_f_2d*0,rlprior_f_2d*0,res$scale2d[2],6,6)
    L_tot_2d_scale2[i,,] = res2[[1]]
    CN1_sol2[i,,] = res2[[2]]
    CN2_sol2[i,,] = res2[[3]]
    PSCN1_sol2[i,,] = res2[[4]]
    PSCN2_sol2[i,,] = res2[[5]]
  }
  L_sum_2d.2 = apply(L_tot_2d_scale2[which(!is.na(baf)),,],c(2,3),sum)*80
  sol2_pct = which(L_sum_2d.2<min(L_sum_2d.2)+.000001,arr.ind=TRUE) - 1
  sol2_scale = res$scale2d[2]
  sol2_cn1 = CN1_sol2[,sol2_pct[1],sol2_pct[2]]
  sol2_cn2 = CN2_sol2[,sol2_pct[1],sol2_pct[2]]
  sol2_pscn1 = PSCN1_sol2[,sol2_pct[1],sol2_pct[2]]
  sol2_pscn2 = PSCN2_sol2[,sol2_pct[1],sol2_pct[2]]

  return (list(sol1_pct = sol1_pct, sol1_scale = sol1_scale, sol1_cn1 = sol1_cn1, sol1_cn2 = sol1_cn2, sol1_pscn1 = sol1_pscn1, sol1_pscn2 = sol1_pscn2, sol2_pct = sol2_pct, sol2_scale = sol2_scale, sol2_cn1 = sol2_cn1, sol2_cn2 = sol2_cn2, sol2_pscn1 = sol2_pscn1, sol2_pscn2 = sol2_pscn2))
}
