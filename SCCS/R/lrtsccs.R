lrtsccs<-function(model1,model2){
  test<-as.numeric((model1$logtest[1]-model2$logtest[1]))
  test<-signif(ifelse(test>0,test,-test), 4)
  df<-as.numeric(model1$logtest[2]-model2$logtest[2])
  df<-ifelse(df>0,df,-df)
  pvalue<-signif(pchisq(test,df,lower.tail=F), 4)
 # print(data.frame(test, df, pvalue), row.names=F)
 yy <- data.frame(test, df, pvalue)
 row.names(yy) <- " "
 return(yy)
}
