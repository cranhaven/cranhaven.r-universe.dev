# modified on Feb. 12, 2019
#  rename logTransMat to lTransformer
# v6 created on Feb. 13, 2018
#  (1) added 'na.rm=TRUE'
#  (2) simplify R code
#

lTransformer.default<-function(delta,vec)
{
  aa=log2(vec+(1/delta))
  res=(mean(aa, na.rm=TRUE)-median(aa, na.rm=TRUE))^2
  return(res)
}

lTransformer=function(mat, low=0.0001, upp=100)
{
  
  vec=c(mat)

  res.delta=optimize(lTransformer.default, vec=vec,lower=low, upper=upp)
  
  y=log2(vec+(1/res.delta$minimum))

  # convert back to matrix
  mat2=matrix(y, ncol=ncol(mat))
  rownames(mat2)=rownames(mat)
  colnames(mat2)=colnames(mat)

  res=list(res.delta=res.delta, delta = res.delta$minimum, mat2=mat2)
  invisible(res)
}


