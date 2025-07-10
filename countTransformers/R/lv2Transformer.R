# modified  on Feb 12, 2019
#  (1) rename logV2TransMat to lv2Transformer
#
# modified  on Jan 14, 2019
#  (1) Zeyu sent to me the R code revised on Feb. 25, 2018
#
# created  on Jan 12, 2019
#  (1) log + Voom transformation minimizing sum of subject-specific
#      squared difference between sample mean and sample media
#
# v3 created on Feb. 25, 2018
#  (1) first obtain cpm (counts per million) for each column, then do log transformation
#  (2) combine new V transformtion and X transformation
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


lv2Transformer.default<-function(delta, mat, lib.size=NULL)
{
  if(is.null(lib.size))
  {
    lib.size=colSums(mat)
  }

  tt <- t(mat + 0.5)/(lib.size + 1) * 1e+06
  mat2 <- t(log2(tt+1/delta))

  md=apply(mat2, 2, median, na.rm=TRUE)
  me=apply(mat2, 2, mean, na.rm=TRUE)
  res=sum((md-me)^2, na.rm=TRUE)

  return(res)

}

lv2Transformer=function(mat, lib.size=NULL, low=0.001, upp=1000)
{
  
  res.delta=optimize(lv2Transformer.default, mat=mat, lib.size=lib.size,
    lower=low, upper=upp)

  delta.optim = res.delta$minimum
  if(is.null(lib.size))
  {
    lib.size=colSums(mat)
  }

  tt <- t(mat + 0.5)/(lib.size + 1) * 1e+06
  mat2 <- t(log2(tt+1/delta.optim))

  res=list(res.delta=res.delta, delta=delta.optim, mat2=mat2)
  invisible(res)
}

