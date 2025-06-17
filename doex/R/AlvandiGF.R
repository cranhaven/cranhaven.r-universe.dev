AGF=function(data,group,rept=100000){

  n=tapply(data, group, length)
  k=length(tapply(data, group, length))
  xbar=tapply(data, group, mean)
  var=tapply(data, group, var)
  w=n/var;
  q=sqrt(w/sum(w));

  T=sum(w*(xbar-sum(q*xbar))^2);

  p=0;
  for(i in 1:rept){
    z=rnorm(k);
    u=rchisq(k,n-1);
    y.=sum(q*z);
    t=sum(((n-1)/u)*((z-q*y.)^2));
    if(t>=T){p=p+1}
  }
  pvalue=p/rept;
  result=matrix(c(round(pvalue,digits=4)))
  rownames(result)=c("p-value")
  colnames(result)=c("Alvandi et al. Generalized F")
  return(t(result))
}
