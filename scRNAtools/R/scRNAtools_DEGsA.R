scRNAtools_DEGsA <-
function(example,types_all,type1,type2,num)
{
edgeR::cpm
n<-ncol(example)
data1<-example[,-1]
zcpm<-cpm(data1)
keep<-rowSums(zcpm>1)>=((n-1)*num)###delete the genes which expression is 0 in half of all the samples.
zset<-example[keep,]
subtype1<-types_all[which(types_all[,1]%in%type1),2]
subtype2<-types_all[which(types_all[,1]%in%type2),2]
type1_exp<-zset[,which(as.numeric(zset[1,])%in%subtype1)]
type2_exp<-zset[,which(as.numeric(zset[1,])%in%subtype2)]
group1<-apply(type1_exp[,-1],1, mean)
group2<-apply(type2_exp[,-1],1, mean)
group1<-as.matrix(group1)
group2<-as.matrix(group2)
FC1<-group2/group1

FC2<-cbind(as.matrix(zset[,1]),FC1)
FC2<-FC2[-1,]
FC2[which(FC2[,2]%in%"Inf"),2]<-5
colnames(FC2)<-c("Gene_symbol","Fold_change")
up<-FC2[which(FC2[,2]>2),]
down<-FC2[which(FC2[,2]<0.5),]
no_diff1<-FC2[-which(FC2[,1]%in%up[,1]),]
no_diff2<-no_diff1[-which(no_diff1[,1]%in%down[,1]),]

main=paste("Differentially expressed genes between",type1,"and",type2)
max_v<-as.numeric(max(FC2[,2]))
max_n<-max(nrow(up),nrow(down),nrow(no_diff2))

p<-plot(1:nrow(up),up[,2],main=main,ylim=c(0,max_v),xlim=c(0,max_n),xlab = "Gene number",ylab=paste("log2 (",type1,"/",type2,")"),col="red",pch=19,lwd=2)
lines(1:nrow(down),down[,2],type="p",ylim=c(0,max_v),xlab = "Gene number",ylab=paste("log2(",type1,"/",type2,")"),col="green",pch=19,lwd=2)
lines(1:nrow(no_diff2),no_diff2[,2],type="p",ylim=c(0,max_v),xlab = "Gene number",ylab=paste("log2(",type1,"/",type2,")"),col="gray",pch=19,lwd=2)
abline(h = c(0.5, 2),lwd=0.5,lty=3)
text((max_n-5),0.2,"log2(FC)=0.5")
text((max_n-5),2.2,"log2(FC)=2")
legend((max_n-20),(max_v-0.2),c("Up-regulated genes","Not DEGs","Down-regulated genes"),col=c("red","gray","green"),text.col=c("red","gray","green"),pch=19,cex=0.7)
return(FC2)
}
