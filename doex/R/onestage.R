OS=function(data,group,nout=1,rept=100000){

  n=tapply(data, group, length)
  k=length(tapply(data, group, length))
  xbar=tapply(data, group, mean)
  var=tapply(data, group, var)
  N=sum(n);

  vary=function(data){return(var(sample(data,length(data)-nout)))}
  var.=tapply(data, group, vary)

  maxvar=max(var.);
  maxvar.=max(var./n);

  U=(1/n)+(1/n*sqrt((1/(n-1))*((maxvar/var.)-1)))
  V=(1/n)-(1/n*sqrt(((n-1))*((maxvar/var.)-1)))

  a=1;b=0;top=0;data.=numeric(length(data));
  for(i in 1:k){
    b=b+n[i];
    data.[a:(b-1)]=data[a:(b-1)]*U[i];
    data.[b]=data[b]*V[i];
    a=b+1;
  }
  xbar.=tapply(data., group, sum)
  xbar..=sum(xbar.)/k;
  F=sum(((xbar.-xbar..)/sqrt(maxvar.))^2);

  p=0;
  for(i in 1:rept){
    t=rt(k,n-2);
    Q=sum((t-mean(t))^2);
    if(Q>F){p=p+1}
  }
  pvalue=p/rept;
  result=matrix(c(round(pvalue,digits=4)))
  rownames(result)=c("p-value")
  colnames(result)=c("One-Stage Test")
  return(t(result))
}
