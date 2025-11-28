#' get_var_tcn_baf
#' @keywords internal
#' @export
get_var_tcn_baf = function(LRR_raw,BAF_raw,gt,sl=1000){
  TCN = 2*2^(LRR_raw);
  l = length(TCN)/sl;v=c()
  for(i in 1:l){m2 = mean2(TCN[1:sl+sl*(i-1)]);v[i]=mean2((TCN[1:sl+sl*(i-1)]-m2)^2)}
  var_tcn = median(v)

  betaT = BAF_raw
  l = length(TCN)/sl;v=c()
  id=which(gt==.5);l=length(id)/sl;v=c()
  for(i in 1:l) v[i]=var(abs(.5-betaT[id[1:sl+sl*(i-1)]]),na.rm=TRUE)
  var_baf = median(v)
  return(c(var_tcn,var_baf))
}
