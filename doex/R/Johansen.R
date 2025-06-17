JF=function(data,group){

  n=tapply(data, group, length)
  k=length(tapply(data, group, length))
  xbar=tapply(data, group, mean)
  var=tapply(data, group, var)
  w=n/var;
  h=w/sum(w);
  xbar.=sum(xbar*w)/sum(w);

  A=sum((1-w/sum(w))^2/(n-1));
  c=(k-1)+2*A-(6*A/(k+1));
  v=(k-1)*(k+1)/(3*A);

  T=sum(w*(xbar-sum(h*xbar))^2);

  pvalue=1-pf(T/c,k-1,v);
  result=matrix(c(round(T,digits=4),round(k-1),round(v),round(pvalue,digits=4)))
  rownames(result)=c("Test Statistic","df1","df2","p-value")
  colnames(result)=c("Johansen F")
  return(t(result))
}
