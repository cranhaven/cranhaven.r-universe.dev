WE=function(data,group){

  n=tapply(data, group, length)
  k=length(tapply(data, group, length))
  xbar=tapply(data, group, mean)
  var=tapply(data, group, var)
  weight1=n/var;
  weight2=weight1/sum(weight1);

  W1=sum(weight1*(xbar-sum(weight2*xbar))^2);
  W2=(k-1)+((2*((k-2)/(k+1))*sum((1-weight2)^2/(n-1))));
  W=W1/W2;

  df=((k^2-1)/3)/sum((1-weight2)^2/(n-1));

  pvalue=1-pf(W,k-1,df);
  result=matrix(c(round(W,digits=4),round(df),round(pvalue,digits=4)))
  rownames(result)=c("Test Statistic","df","p-value")
  colnames(result)=c("Welch")

  return(t(result))
}
