WA=function(data,group){

  n=tapply(data, group, length)
  k=length(tapply(data, group, length))
  xbar=tapply(data, group, mean)
  var=tapply(data, group, var)

  w=(n/var)/sum(n/var);
  genelxbar=sum(w*xbar);
  t=(xbar-genelxbar)/sqrt(var/n);
  v=n-1;
  a=sum((1-w)^2/v);

  WA=(sum(t^2)/(k-1))/(1+((2*k-2)/(k^2-1))*a);

  pvalue=1-pf(WA,k-1,(k^2-1)/(3*a));
  result=matrix(c(round(WA,digits=4),round((k^2-1)/(3*a)),round(pvalue,digits=4)))
  rownames(result)=c("Test Statistic","df","p-value")
  colnames(result)=c("Welch-Aspin")
  return(t(result))
}
