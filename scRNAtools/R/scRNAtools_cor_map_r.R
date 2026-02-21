scRNAtools_cor_map_r <-
function(exam1,types_all,type,methods){

PerformanceAnalytics::chart.Correlation
corrplot::corrplot
Hmisc::rcorr
flattenCorrMatrix <- function(cormat, pmat)
{
ut <- upper.tri(cormat)
data.frame( row = rownames(cormat)[row(cormat)[ut]],
column = rownames(cormat)[col(cormat)[ut]], cor =(cormat)[ut], p = pmat[ut] )
}
rownames(exam1)<-exam1[,1]
type1<-types_all[which(types_all[,1]%in%type),2]
mydata<-exam1[,which(exam1[1,]%in%type1)]
mydata1<-mydata[-1,]
mydata2<-apply(mydata1,1,mean)
mydata3<-mydata1[-which(mydata2%in%0),]
expdata1<-t(mydata3)
res3 <- rcorr(expdata1,type=methods)##pearson,spearman
flattenCorrMatrix(res3$r, res3$P)
chart.Correlation(expdata1, histogram=FALSE, pch=22)
return(res3)
}
