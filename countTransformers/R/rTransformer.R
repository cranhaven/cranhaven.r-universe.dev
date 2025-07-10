# modified on Feb. 12, 2019
#  (1) rename rootTransMat to rTransformer
#
# v6 created on Feb. 13, 2018
#  (1) add 'na.rm=TRUE'
#  (2) simplify R code

rTransformer.default<-function(eta,vec)
{
  aa=(vec^(1/eta)/(1/eta))
  res=(mean(aa, na.rm=TRUE)-median(aa, na.rm=TRUE))^2
  return(res)
}

rTransformer=function(mat, low=0.0001, upp=100)
{
  
  vec=c(mat)

  res.eta=optimize(rTransformer.default, vec=vec,lower=low, upper=upp)
  
  y=vec^(1/res.eta$minimum)/(1/res.eta$minimum)

  # convert back to matrix
  mat2=matrix(y, ncol=ncol(mat))
  rownames(mat2)=rownames(mat)
  colnames(mat2)=colnames(mat)
  
  res=list(res.eta=res.eta, eta=res.eta$minimum, mat2=mat2)
  invisible(res)
}

