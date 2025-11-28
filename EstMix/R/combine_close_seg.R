#' combine close segmentation
#' @keywords internal
#' @export
combine_close_seg <- function(seg_raw,var_baf,data,delta){
  IT_tot=seg_raw[,c(1,4:7,7,12:13,13)]
  dhNbrOfLoci = seg_raw$dhNbrOfLoci
  IT = subset(IT_tot,dhNbrOfLoci>10)
  B = IT[,c(1:3,7:9,9)];IT = IT[,c(1:6,6)];
  names(IT)[6:7]=c('tcnVar','eff.size');
  names(B)[5:7]=c('bafMean','bafVar','eff.size')
  #### step 1: combine segments with difference in mean baf less than delta*2
  IT_new = IT;B_new = B;l=nrow(B)
  d = abs(B_new[2:l,5]-B_new[1:(l-1),5])+abs(B_new[2:l,1]-B_new[1:(l-1),1])##second part is to set apart segs from different chr
  while(sum(d<delta*2)>0&l>1){
    id = which.min(d)
    B_new[id,3]=B_new[id+1,3]
    B_new[id,5]=sum(B_new[id+0:1,4]*B_new[id+0:1,5],na.rm=TRUE)/sum(B_new[id+0:1,4],na.rm=TRUE)
    B_new[id,4]=sum(B_new[(id+0:1),4])
    B_new = B_new[-id-1,]
    l=nrow(B_new)
    d = abs(B_new[2:l,5]-B_new[1:(l-1),5])+abs(B_new[2:l,1]-B_new[1:(l-1),1])##second part is to set apart segs from different chr
    sum(d<delta*2)
  }
  #### step 2: calculate mean, variance and effective sample size for each segment
  l=nrow(B_new)
  IT_new = IT_new[1:l,];
  IT_new[,1:3]=B_new[,1:3]
  n_seg = nrow(IT_new)
  for(i in 1:n_seg){
    ### for each segment, select the original data corresponding to that segment
    s = IT_new[i,2];e = IT_new[i,3];c = IT_new[i,1]
    temp2 = subset(data,data[,1]>s-1&data[,1]<e+1&data[,2]==c)
    ### calculation for tcn
    IT_new$tcnMean[i]=mean2(temp2[,3])## to prevent outliers
    IT_new$tcnVar[i]=mean2((temp2[,3]-IT_new$tcnMean[i])^2)## to prevent outliers
    IT_new$tcnNbrOfLoci[i] = nrow(temp2)
    IT_new$eff.size[i] = calc_n(nrow(temp2))
    ### calculation for baf
    temp1 = subset(data,data$x>s-1&data$x<e+1&data$gt==.5&data$chr==c)
    beta = temp1$BAF_raw;beta = beta[!is.na(beta)]
    betamirror = ifelse(beta>.5,1-beta,beta)
    xmin <- optimize(f_baf, c(0, 1), tol = 0.0001, beta=beta, var = var_baf*2)$min
    B_new$bafMean[i] = .5-abs(0.5-xmin)
    B_new$bafVar[i] = var_baf
    B_new$dhNbrOfLoci[i]=nrow(temp1)
    B_new$eff.size[i]=calc_n(nrow(temp1))
  }
  ITB_new = cbind(IT_new,B_new)
  return(ITB_new)
}

