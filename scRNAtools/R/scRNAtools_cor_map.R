scRNAtools_cor_map <-
function(exam1,types_all,type,methods){
PerformanceAnalytics::chart.Correlation
corrplot::corrplot
rownames(exam1)<-exam1[,1]
type1<-types_all[which(types_all[,1]%in%type),2]
mydata<-exam1[,which(exam1[1,]%in%type1)]
mydata1<-mydata[-1,]
mydata2<-apply(mydata1,1,mean)
mydata3<-mydata1[-which(mydata2%in%0),]
expdata1<-t(mydata3)
corr<-cor(expdata1,method=methods)
corrplot(corr,method="color",tl.cex=0.5)
return(corr)
}
