AF=function(data,group){

  n=tapply(data, group, length)
  k=length(tapply(data, group, length))
  xbar=tapply(data, group, mean)
  var=tapply(data, group, var)
  N=sum(n);

  AF=((N-k)/(k-1))*((sum(n*(xbar-mean(xbar))^2))/(sum((n-1)*var)));
  c=((N-k)/(N*(k-1)))*(sum((N-n)*var)/sum((n-1)*var));

  df1=((sum((1-n/N)*var))^2)/((sum(var^2))+(sum(n*var/N))^2-(2*sum(n*var^2/N)));
  df2=(sum((n-1)*var))^2/(sum((n-1)*var^2));

  pvalue=1-pf(AF/c,df1,df2);
  result=matrix(c(round(AF,digits=4),round(df1),round(df2),round(pvalue,digits=4)))
  rownames(result)=c("Test Statistic","df1","df2","p-value")
  colnames(result)=c("Approximate F")
  return(t(result))
}
