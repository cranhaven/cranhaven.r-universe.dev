BF=function(data,group){

  n=tapply(data, group, length)
  k=length(tapply(data, group, length))
  xbar=tapply(data, group, mean)
  var=tapply(data, group, var)

  B=sum(n*(xbar-mean(data))^2)/sum((1-(n/sum(n)))*var);
  df1=sum((1-(n-sum(n)))*var)^2;
  df2=sum(((1-(n-sum(n)))^2*(var)^2)/(n-1));
  df=(df1/df2);

  pvalue=1-pf(B,k-1,df);
  result=matrix(c(round(B,digits=4),round(k-1),round(pvalue,digits=4)))
  rownames(result)=c("Test Statistic","df","p-value")
  colnames(result)=c("Brown-Forsythe")
  return(t(result))
}
