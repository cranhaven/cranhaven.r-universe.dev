PF=function(data,group,rept=100000){

  n=tapply(data, group, length)
  k=length(tapply(data, group, length))
  xbar=tapply(data, group, mean)
  var=tapply(data, group, var)
  Fobs=as.numeric(summary(aov(data~as.factor(group)))[[1]][["F value"]][1]);
  Fstar=numeric(rept);

  for(i in 1:rept){
    perm.group=sample(group);
    Fstar[i]=as.numeric(summary(aov(data~as.factor(perm.group)))[[1]][["F value"]][1])
  }

  pvalue=mean(Fstar>=Fobs);
  result=matrix(c(round(pvalue,digits=4)))
  rownames(result)=c("p-value")
  colnames(result)=c("Permutation F")
  return(t(result))
}
