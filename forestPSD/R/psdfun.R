psdfun<-function(ax,a=100,b=6,index="Deevey2"){
  fun=list(A=ax~a+b*age,
           B=ax~a*exp(-b*age),
           C=ax~a*(age^-b))
  Npop<-Ntable(ax=ax)
  names(Npop)[1]<-"age"
  if(index=="Deevey1"){
    psdnls<-minpack.lm::nlsLM(data=Npop,formula=fun[[1]],start=c(a=a,b=b))
  }else if(index=="Deevey2"){
    psdnls<-minpack.lm::nlsLM(data=Npop,formula=fun[[2]],start=c(a=a,b=b))
  }else if(index=="Deevey3"){
    psdnls<-minpack.lm::nlsLM(data=Npop,formula=fun[[3]],start=c(a=a,b=b))
  }
  Npop$predict<-stats::fitted(psdnls)
  list(Summary=summary(psdnls),
       Goodness=goodness(model=psdnls,data=Npop),
       Data=Npop)
}
