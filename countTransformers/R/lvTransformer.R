# modified on Feb. 12, 2019
#  (1) rename logVTransMat to lvTransformer
#
# v3 created on Feb. 25, 2018
#  (1) first obtain cpm (counts per million) for each column, then do log transformation
#
# v2 created on Feb. 13, 2018
#  (1)  fixed a bug: 'aa' should be 'vec'
#  (2) fixed a bug in 'getVLogTransMat': 
#  mat2 <- t(log2(t(mat2 + 1/delta)/(lib.size + 1) * 1e+06))
#  should be
#  mat2 <- t(log2(t(mat + 1/delta)/(lib.size + 1) * 1e+06))

# created on Feb. 13, 2018
# v4 created on Feb. 13, 2018
#  (1) added 'na.rm=TRUE'
#  (2) simplified R code


lvTransformer.default<-function(delta, mat, lib.size=NULL)
{
  if(is.null(lib.size))
  {
    lib.size=colSums(mat)
  }

  tt <- t(mat + 0.5)/(lib.size + 1) * 1e+06
  mat2 <- t(log2(tt+1/delta))
  vec = c(mat2) 
  res=(mean(vec, na.rm=TRUE)-median(vec, na.rm=TRUE))^2

  return(res)
}

lvTransformer=function(mat, lib.size=NULL, low=0.001, upp=1000)
{
  
  res.delta=optimize(lvTransformer.default, mat=mat, lib.size=lib.size,
    lower=low, upper=upp)

  delta = res.delta$minimum
  if(is.null(lib.size))
  {
    lib.size=colSums(mat)
  }

  tt <- t(mat + 0.5)/(lib.size + 1) * 1e+06
  mat2 <- t(log2(tt+1/delta))
 
  res=list(res.delta=res.delta, delta=delta, mat2=mat2)
  invisible(res)
}
