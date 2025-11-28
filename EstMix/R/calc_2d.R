#' Return mixture estimation of a normal and 2 tumors
#' Takes BAF, LRR, chr, x, gt, seg_raw
#' @param BAF vector containing B allen frequency (BAF)
#' @param LRR vector
#' @param chr vector
#' @param x vector
#' @param GT vectors of factors containing genotype
#' @param seg_raw dataframe about segmentation
#' @return \item{sol1}{a numeric vector of length 2. It provides the estimated percentages of normal and tumor from the best solution. The first number is the percentage of the estimated normal percentage. The second number-1 is the percentage of the estimated tumor 1 percentage} \item{sol2}{a numeric vector of length 2. It provides the estimated percentages of normal and tumor from the second best solution. The first number is the percentage of the estimated normal percentage. The second number-1 is the percentage of the estimated tumor 1 percentage}
#' @keywords internal
#' @export
calc_2d <- function(BAF, LRR, chr, x, GT, seg_raw){
  #source("./R/combine_close_seg.R")######### a list of R functions
  #sourceCpp("./src/calcll_cpp.cpp")
  #################################################### seg prior #########################################################
  ### 2D prior
  tt = exp(-(0:6-1)^2);prior_f = tt%*%t(tt);lp_2d = -log(prior_f)*1e-3
  rtt = exp(-(6:0-1)^2);rprior_f = rtt%*%t(rtt);rlp_2d = -log(rprior_f)*1e-3

  gt = (GT=='BB')*2+(GT=='AB')*1.5+(GT=='AA')-1;gt[gt==(-1)]=NA
  data= data.frame(x,chr,2*2^(LRR),GT,BAF)
  ## step 1: calculate variance for tcn and baf
  var = get_var_tcn_baf(LRR,BAF,gt)
  ##
  var_tcn=var[1]; var_baf=var[2]
  scale_max = mean(2*2^LRR,na.rm=TRUE); scale_min = scale_max/6
  ## step 2: combine close segments
  delta = 0.008

  ####################### DO DATA CLEANING AND SAVE THE CLEAN DATA IN PROC.FN ####################
  ITB = combine_close_seg(seg_raw,var_baf,data,delta=delta) #### note: extra variable data is included in the function
  IT_new = ITB[,1:7]; B_new = ITB[,1:7+7]
  ### exclude segments with 0 copy number and baf of large variance
  id_excl = which(IT_new[,5]<.05)
  if(length(id_excl)>0){IT_new = IT_new[-id_excl,];B_new = B_new[-id_excl,]}
  ## proc.fn = paste("js_50","_",delta,".Rda",sep="")
  ## save(scale_min,scale_max,IT_new,B_new,lp_2d,rlp_2d,var_baf,var_tcn,file=proc.fn)
  temp <- data.frame(scale_max, scale_min, var_baf, var_tcn)

  ####################### SELECT SCALE AND SAVE SCALE IN SEL.SCALE.FN ####################
  res2d=sel_scale_2d(cnMax=6,pscnMax=6,ngrid = 100,nslaves = 50, B_new = B_new, IT_new = IT_new, temp = temp)

  ####################### OPTIMIZATION FOR 1D and 2D AND SAVE IN MATRIX SOL.1d and SOL.2d ####################
  B_new[,7] = B_new[,7]*5
  n_seg = nrow(IT_new)
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
  for(i in 1:n_seg){
    res1 = calcll_cpp(as.numeric(IT_new[i,]),as.numeric(B_new[i,]),lp_2d,rlp_2d,var_baf,var_tcn,res2d$scale2d[1],6,6)
    L_tot_2d_scale1[i,,] = res1[[1]]
    CN1_sol1[i,,] = res1[[2]]
    CN2_sol1[i,,] = res1[[3]]
    PSCN1_sol1[i,,] = res1[[4]]
    PSCN2_sol1[i,,] = res1[[5]]
    res2 = calcll_cpp(as.numeric(IT_new[i,]),as.numeric(B_new[i,]),lp_2d,rlp_2d,var_baf,var_tcn,res2d$scale2d[2],6,6)
    L_tot_2d_scale2[i,,] = res2[[1]]
    CN1_sol2[i,,] = res2[[2]]
    CN2_sol2[i,,] = res2[[3]]
    PSCN1_sol2[i,,] = res2[[4]]
    PSCN2_sol2[i,,] = res2[[5]]
  }
  L_sum_2d_scale1 = apply(L_tot_2d_scale1,c(2,3),sum)
  L_sum_2d_scale2 = apply(L_tot_2d_scale2,c(2,3),sum)
  sol1_pct = which(L_sum_2d_scale1<min(L_sum_2d_scale1)+.000001,arr.ind=TRUE) - 1
  sol1_scale = res2d$scale2d[1]
  sol1_cn1 = CN1_sol1[,sol1_pct[1],sol1_pct[2]]
  sol1_cn2 = CN2_sol1[,sol1_pct[1],sol1_pct[2]]
  sol1_pscn1 = PSCN1_sol1[,sol1_pct[1],sol1_pct[2]]
  sol1_pscn2 = PSCN2_sol1[,sol1_pct[1],sol1_pct[2]]
  sol2_pct = which(L_sum_2d_scale2<min(L_sum_2d_scale2)+.000001,arr.ind=TRUE) - 1
  sol2_scale = res2d$scale2d[2]
  sol2_cn1 = CN1_sol2[,sol2_pct[1],sol2_pct[2]]
  sol2_cn2 = CN2_sol2[,sol2_pct[1],sol2_pct[2]]
  sol2_pscn1 = PSCN1_sol2[,sol2_pct[1],sol2_pct[2]]
  sol2_pscn2 = PSCN2_sol2[,sol2_pct[1],sol2_pct[2]]
  return (list(sol1_pct = sol1_pct, sol1_scale = sol1_scale, sol1_cn1 = sol1_cn1, sol1_cn2 = sol1_cn2, sol1_pscn1 = sol1_pscn1, sol1_pscn2 = sol1_pscn2, sol2_pct = sol2_pct, sol2_scale = sol2_scale, sol2_cn1 = sol2_cn1, sol2_cn2 = sol2_cn2, sol2_pscn1 = sol2_pscn1, sol2_pscn2 = sol2_pscn2, segmentation = seg_raw))
}
