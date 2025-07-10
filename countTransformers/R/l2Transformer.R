# modified  on Feb. 12, 2019
#  (1) rename log2TransMat to l2Transformer
#
# modified  on Jan 14, 2019
#  (1) Zeyu sent to me the R code revised on Feb. 25, 2018
#
# modified on Jan. 12, 2019
#  (1) mov 'getmm2sum' to a separate file
#
# v2 created on Feb. 13, 2018
#  (1) use more efficient R code
#  (2) rename 'logTransX' to 'log2Trans'
#  (2) rename 'logTransMatX' to 'log2TransMat'
#
#
#getmm2sum<-function(mat)
#{
#  md=apply(mat, 2, median, na.rm=TRUE)
#  me=apply(mat, 2, mean, na.rm=TRUE)
#  nm2sum = sum((md-me)^2, na.rm=TRUE)
#  return(mm2sum)
#
#}
#

l2Transformer.default<-function(delta,mat)
{
  aa=log2(mat+(1/delta))
  res=getmm2sum(aa)
  return(res)
}

l2Transformer=function(mat, low=0.0001, upp=1000)
{
  
  res.delta=optimize(l2Transformer.default, mat=mat,lower=low, upper=upp)
  
  mat2=log2(mat+(1/res.delta$minimum))
  rownames(mat2)=rownames(mat)
  colnames(mat2)=colnames(mat)

  res=list(res.delta=res.delta, delta=res.delta$minimum,  mat2=mat2)
  invisible(res)
}


