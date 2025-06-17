PB=function(data,group,rept=100000){

  n=tapply(data, group, length)
  k=length(tapply(data, group, length))
  xbar=tapply(data, group, mean)
  var=tapply(data, group, var)

  sb=sum(n*xbar^2/var)-((sum(n*xbar/var))^2)/(sum(n/var));

  p=0;
  for(i in 1:rept){
    z=rnorm(k);
    u=rchisq(k,n-1);
    PB=(sum(z^2*(n-1)/u))-(((sum((sqrt(n)*z*(n-1))/(sqrt(var)*u)))^2)/(sum(n*(n-1)/(var*u))));
    if(PB>sb){p=p+1}
  }
  pvalue=p/rept;
  result=matrix(c(round(pvalue,digits=4)))
  rownames(result)=c("p-value")
  colnames(result)=c("Parametric Bootstrap")
  return(t(result))
}
