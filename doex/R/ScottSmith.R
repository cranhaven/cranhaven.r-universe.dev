SS=function(data,group){

  n=tapply(data, group, length)
  k=length(tapply(data, group, length))
  xbar=tapply(data, group, mean)
  var=tapply(data, group, var)
  var.=var*((n-1)/(n-3));

  SS=sum((n*(xbar-mean(data))^2)/var.);

  pvalue=1-pchisq(SS,k);
  result=matrix(c(round(SS,digits=4),round(k),round(pvalue,digits=4)))
  rownames(result)=c("Test Statistic","df","p-value")
  colnames(result)=c("Scott-Smith")
  return(t(result))
}
