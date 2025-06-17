FA=function(data,group,rept=10000){

  n=tapply(data, group, length)
  k=length(tapply(data, group, length))
  xbar=tapply(data, group, mean)
  var=tapply(data, group, var)
  N=sum(n);

  ro=sum(n*xbar^2/var)-((sum(n*xbar/var))^2)/(sum(n/var));

  p=0;
  for(i in 1:rept){
    t=rt(k,n-1);
    rl=sum(t^2)-(((sum(sqrt(n)*t/sqrt(var)))^2)/(sum(n/var)));
    if(rl>=ro){p=p+1}
  }
  pvalue=p/rept;
  result=matrix(c(round(pvalue,digits=4)))
  rownames(result)=c("p-value")
  colnames(result)=c("Fiducial Approach")
  return(t(result))
}
