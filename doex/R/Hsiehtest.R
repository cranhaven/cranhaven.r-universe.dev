HS=function(data,group){

  n     <- tapply(data, group, length)
  k     <- length(tapply(data, group, length))
  gmin  <- tapply(data, group, min)
  sj    <- tapply(data, group, function(x) sum(x-min(x)))
  wj    <- n*(gmin-min(gmin))

  T <- -2*sum(n*log(sj/(sj+wj)))

  pvalue <- 1 - pchisq(T,2*k-2)
  result=matrix(c(round(T,digits=4),round(2*k-2),round(pvalue,digits=4)))
  rownames(result)=c("Test Statistic","df","p-value")
  colnames(result)=c("Hsieh test")
  return(t(result))
}
