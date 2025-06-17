AG=function(data,group){

  n=tapply(data, group, length)
  k=length(tapply(data, group, length))
  xbar=tapply(data, group, mean)
  var=tapply(data, group, var)

  w=(n/var)/sum(n/var);
  mu=sum(w*xbar);
  t=(xbar-mu)/sqrt(var/n);
  v=n-1;
  a=v-0.5;
  b=48*a^2;
  c=sqrt(a*log(1+(t^2/v)));
  z=c+(((c^3)+(3*c))/b)-((4*c^7+33*c^5+240*c^3+855*c)/(10*b^2+8*b*c^4+1000*b));

  AD=sum(z^2);

  pvalue=1-pchisq(AD,k-1);
  result=matrix(c(round(AD,digits=4),round(k-1),round(pvalue,digits=4)))
  rownames(result)=c("Test Statistic","df","p-value")
  colnames(result)=c("Alexander-Govern")
  return(t(result))
}
