outly = function(ndata,noutlier,meand,vard,dif,alpha=0.05,normality.status=TRUE,skewn.status=0){

  repeat{
  data = rnorm(ndata-noutlier,meand,sqrt(vard))
  s=sd(data)

  lb=as.numeric(quantile(data)[2])-(1.5*IQR(data))
  ub=as.numeric(quantile(data)[4])+(1.5*IQR(data))

  lower_interval = seq(lb-s*dif,lb-s*(dif-1),length.out = 1000)
  higher_interval= seq(ub+s*(dif-1),ub+s*dif,length.out = 1000)

  if(skewn.status == 1){outliers=sample(higher_interval,noutlier)}
  if(skewn.status ==-1){outliers=sample(lower_interval,noutlier)}
  if(skewn.status == 0){outliers=c(sample(lower_interval,ceiling(noutlier/2)),sample(higher_interval,floor(noutlier/2)))}

  outdata = c(data,outliers)
  normaltest=shapiro.test(outdata)
  if(normality.status == FALSE){if(shapiro.test(outdata)$p.value < alpha){break}}
  if(normality.status == TRUE){if(shapiro.test(outdata)$p.value >= alpha){break}}
  }
  return(list(data=outdata,outliers=outliers,normality.test=normaltest))
}
