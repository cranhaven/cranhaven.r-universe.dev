MW=function(data,group){

  n=tapply(data, group, length)
  k=length(tapply(data, group, length))
  xbar=tapply(data, group, mean)
  var=tapply(data, group, var)
  c=(n-1)/(n-3);
  w=n/(c*var);
  h=w/sum(w);


  AWT1=sum(w*(xbar-sum(h*xbar))^2);
  AWT2=(k-1)+((2*((k-2)/(k+1))*sum((1-h)^2/(n-1))));
  AWT=AWT1/AWT2;

  df=((k^2-1)/3)/sum((1-h)^2/(n-1));

  pvalue=1-pf(AWT,k-1,df);
  result=matrix(c(round(AWT,digits=4),round(df),round(pvalue,digits=4)))
  rownames(result)=c("Test Statistic","df","p-value")
  colnames(result)=c("Modified Welch")

  return(t(result))
}
