# modified on Feb. 12, 2019
#  (1) rename rootVTransMat to rvTransformer
#
# modified on Jan. 14, 2019
#  (1) revised based on the R code Zeyu sent to me on Jan. 14, 2019
#
# v3 created on Feb. 25, 2018
#  (1) first obtain cpm (counts per million) for each column, then do log transformation
#
# v2 created on Feb. 13, 2018
#  (1) fixed a bug: there are two 'rootVTransMat' functions
#  (2) fixed a bug: 'aa' should be 'vec'

# v4 created on Feb. 13, 2018
#  (1) correct the formula: the current formula 
#      is the same as 'root transformation'

rvTransformer.default<-function(eta, mat,lib.size=NULL)
{
  if(is.null(lib.size))
  {
    lib.size=colSums(mat)
  }

  tt <- t(mat + 0.5)/(lib.size + 1) * 1e+06
  mat2 <- t(tt^(1/eta)/(1/eta))
  vec = c(mat2) 
  res=(mean(vec, na.rm=TRUE)-median(vec, na.rm=TRUE))^2
  return(res)

}

rvTransformer=function(mat, lib.size=NULL, low=0.001, upp=1000)
{
  res.eta=optimize(rvTransformer.default, mat=mat, lib.size=lib.size,
    lower=low, upper=upp)

  eta.optim=res.eta$minimum
  if(is.null(lib.size))
  {
    lib.size=colSums(mat)
  }

  tt <- t(mat + 0.5)/(lib.size + 1) * 1e+06
  mat2 <- t(tt^(1/eta.optim)/(1/eta.optim))
  rownames(mat2)=rownames(mat)
  colnames(mat2)=colnames(mat)

  res = list(res.eta=res.eta, eta=eta.optim, mat2=mat2)
  invisible(res)
}


