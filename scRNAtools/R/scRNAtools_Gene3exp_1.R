scRNAtools_Gene3exp_1 <-
function(example,types_all,gene1,gene2,gene3,n,col_1,col_2,col_3,pch,lwd)
{
type=types_all[n,]
example<-as.matrix(example)
gene1<-as.matrix(gene1)
gene2<-as.matrix(gene2)
gene3<-as.matrix(gene3)
exp1<-example[which(example[,1]%in%gene1),]
exp2<-example[which(example[,1]%in%gene2),]
exp3<-example[which(example[,1]%in%gene3),]
exp1<-as.matrix(exp1)
subtype1<-as.matrix(example[1,])
exp11<-cbind(example[1,],exp1)
colnames(exp11)<-exp11[1,]
exp11<-exp11[-1,]
eee1<-as.numeric(exp11[,1])
exp12<-exp11[which(eee1%in%as.numeric(type[,2])),]
num_type<-type[which(type[,2]%in%unique(eee1)),]
geneexp1<-as.numeric(exp12[,2])
exp2<-as.matrix(exp2)
subtype2<-as.matrix(example[1,])
exp21<-cbind(example[1,],exp2)
colnames(exp21)<-exp21[1,]
exp21<-exp21[-1,]
eee2<-as.numeric(exp21[,1])
exp22<-exp21[which(eee2%in%as.numeric(type[,2])),]
num_type<-type[which(type[,2]%in%unique(eee2)),]
geneexp2<-as.numeric(exp22[,2])
exp3<-as.matrix(exp3)
subtype3<-as.matrix(example[1,])
exp31<-cbind(example[1,],exp3)
colnames(exp31)<-exp31[1,]
exp31<-exp31[-1,]
eee3<-as.numeric(exp31[,1])
exp32<-exp31[which(eee3%in%as.numeric(type[,2])),]
num_type<-type[which(type[,2]%in%unique(eee3)),]
geneexp3<-as.numeric(exp32[,2])
pdf(file=file.path(tempdir(), "three-genes expression.pdf"))
main = paste("Gene expression in",type[,1],"cells")
max_v<-as.numeric(max(geneexp1,geneexp2,geneexp3))
plot(1:nrow(exp12),geneexp1,type="o",main=main,ylim=c(0,max_v),xlab = paste(type[,1],"cells"),ylab="Gene expression",col=col_1,pch=pch,lwd=lwd)
lines(1:nrow(exp22),geneexp2,type="o",ylim=c(0,max_v),xlab = paste(type[,1],"cells"),ylab="Gene expression",col=col_2,pch=pch,lwd=lwd)
lines(1:nrow(exp32),geneexp3,type="o",ylim=c(0,max_v),xlab = paste(type[,1],"cells"),ylab="Gene expression",col=col_3,pch=pch,lwd=lwd)
dev.off()
}
