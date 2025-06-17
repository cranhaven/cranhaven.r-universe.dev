MBF=function(data,group){

  n=tapply(data, group, length)
  k=length(tapply(data, group, length))
  xbar=tapply(data, group, mean)
  var=tapply(data, group, var)
  N=sum(n);

  B=sum(n*(xbar-mean(data))^2)/sum((1-(n/sum(n)))*var);
  df11=(sum((1-(n-sum(n)))*var)^2);
  df12=sum(((1-(n-sum(n)))^2*(var)^2)/(n-1));
  df2=(df11/df12);
  df1=((sum((1-n/N)*var))^2)/((sum(var^2))+(sum(n*var/N))^2-(2*sum(n*var^2/N)));

  pvalue=1-pf(B,df1,df2);
  result=matrix(c(round(B,digits=4),round(df1),round(df2),round(pvalue,digits=4)))
  rownames(result)=c("Test Statistic","df1","df2","p-value")
  colnames(result)=c("Modified Welch")
  return(t(result))
}
