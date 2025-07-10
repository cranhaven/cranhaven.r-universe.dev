# modified  on Feb. 12, 2019
#  (1) rename root2TransMat to r2Transformer
#
# modified  on Jan. 12, 2019
#  (1) move 'getmm2sum' to a separate file
#
# v2 created  on Feb. 13, 2018
#  (1) use more efficient R code
#  (2) rename 'rootTransX' to 'root2Trans'
#  (3) rename 'rootTransMatX' to 'root2TransMat'

#getmm2sum<-function(mat)
#{
#  md=apply(mat, 2, median, na.rm=TRUE)
#  me=apply(mat, 2, mean, na.rm=TRUE)
#  mm2sum=sum((md-me)^2, na.rm=TRUE)
#  return(mm2sum)
#}
#

r2Transformer.default<-function(eta,mat)
{
  aa=(mat^(1/eta)/(1/eta))
  res=getmm2sum(aa)
  return(res)
}

#getRootTransX=function(mat, low=0.0001, upp=1000)
r2Transformer=function(mat, low=0.0001, upp=1000)
{
  
  res.eta=optimize(r2Transformer.default, mat=mat,lower=low, upper=upp)
  
  mat2=mat^(1/res.eta$minimum)/(1/res.eta$minimum)
  rownames(mat2)=rownames(mat)
  colnames(mat2) = colnames(mat)

  res=list(res.eta=res.eta, eta=res.eta$minimum, mat2=mat2)
  invisible(res)
}


