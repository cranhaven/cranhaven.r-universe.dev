B2=function(alpha,data,group){

  n=tapply(data, group, length)
  k=length(tapply(data, group, length))
  xbar=tapply(data, group, mean)
  var=tapply(data, group, var)

  w=(n/var)/sum(n/var);
  mu=sum(w*xbar);
  t=(xbar-mu)/sqrt(var/n);
  v=n-1;
  zc=qnorm(1-alpha/2);
  c=(4*v^2+((10*zc^2+15)/(24)))/((4*v^2)+v+((4*zc^2+9)/(12)))*sqrt(v)
  z=c*sqrt(log(1+(t^2/v)));

  BK=sum(z^2);

  pvalue=1-pchisq(BK,k-1);
  result=matrix(c(round(BK,digits=4),round(k-1),round(pvalue,digits=4)))
  rownames(result)=c("Test Statistic","df","p-value")
  colnames(result)=c("B2")
  return(t(result))
}
