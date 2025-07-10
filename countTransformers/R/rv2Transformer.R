# modified  on Feb 12, 2019
#  rename rootV2TransMat to rv2Transformer
#
# modified  on Jan 14, 2019
#  (1) based on the R code sent by Zeyu on Jan. 14, 2019
#
# created  on Jan 12, 2019
#  (1) root + Voom transformation minimizing sum of subject-specific
#      squared difference between sample mean and sample median

# v3 created on Feb. 25, 2018
#  (1) first obtain cpm (counts per million) for each column, then do log transformation
#  (2) combine new V transformtion and X transformation
#

## sum of subject-specific squared difference between sample mean
## and sample median
#getmm2sum<-function(mat)
#{
#  md=apply(mat, 2, median, na.rm=TRUE)
#  me=apply(mat, 2, mean, na.rm=TRUE)
#  mm2sum=sum((md-me)^2, na.rm=TRUE)
#  return(mm2sum)
#}
#

rv2Transformer.default<-function(eta,mat, lib.size=NULL)
{
  if(is.null(lib.size))
  {
    lib.size=colSums(mat)
  }
  tt <- t(mat + 0.5)/(lib.size + 1) * 1e+06
  mat2 <- t(tt^(1/eta)/(1/eta))
  md=apply(mat2, 2, median, na.rm=TRUE)
  me=apply(mat2, 2, mean, na.rm=TRUE)
  res=sum((md-me)^2, na.rm=TRUE)
  return(res)

  return(res)
}

#getRootTransX=function(mat, low=0.0001, upp=1000)
rv2Transformer=function(mat, low=0.0001, upp=1000, lib.size=NULL)
{
  
  res.eta=optimize(rv2Transformer.default, mat=mat,lower=low, upper=upp,
		   lib.size=lib.size)
  
  #mat2=mat^(1/res.eta$minimum)/(1/res.eta$minimum)
  eta.optim=res.eta$minimum

  if(is.null(lib.size))
  {
    lib.size=colSums(mat)
  }

  tt <- t(mat + 0.5)/(lib.size + 1) * 1e+06
  mat2 <- t(tt^(1/eta.optim)/(1/eta.optim))
  rownames(mat2)=rownames(mat)
  colnames(mat2)=colnames(mat)

  res=list(res.eta=res.eta, eta=eta.optim, mat2=mat2)
  invisible(res)
}


