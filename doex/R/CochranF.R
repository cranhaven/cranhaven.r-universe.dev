CF=function(data,group){

  n=tapply(data, group, length)
  k=length(tapply(data, group, length))
  xbar=tapply(data, group, mean)
  var=tapply(data, group, var)
  w=n/var;
  h=w/sum(w);

  C=sum(w*(xbar-sum(h*xbar))^2);

  pvalue=1-pchisq(C,k-1);
  result=matrix(c(round(C,digits=4),round(k-1),round(pvalue,digits=4)))
  rownames(result)=c("Test Statistic","df","p-value")
  colnames(result)=c("Cochran F")
  return(t(result))
}
