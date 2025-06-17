BX=function(data,group){

  n=tapply(data, group, length)
  k=length(tapply(data, group, length))
  xbar=tapply(data, group, mean)
  var=tapply(data, group, var)
  N=sum(n);
  mu=sum(n*xbar)/N;
  f=n/N;

  df1=(sum((1-f)*var))^2/((sum(var*f))^2+sum((var^2)*(1-2*f)));
  df2=((sum((1-f)*var))^2)/((sum(var^2*(1-f)^2))/sum(n-1));

  BF=(sum(n*((xbar-mu)^2)))/(sum((1-(n/N))*var));

  pvalue=1-pf(BF,df1,df2);
  result=matrix(c(round(BF,digits=4),round(df1),round(df2),round(pvalue,digits=4)))
  rownames(result)=c("Test Statistic","df1","df2","p-value")
  colnames(result)=c("Box F")
  return(t(result))
}
