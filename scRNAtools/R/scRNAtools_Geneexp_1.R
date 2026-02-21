scRNAtools_Geneexp_1 <-
function(example,gene,types_all,n,col,pch,lwd)
{
type=types_all[n,]
example<-as.matrix(example)
gene<-as.matrix(gene)
exp<-example[which(example[,1]%in%gene),]
exp<-as.matrix(exp)
subtype<-as.matrix(example[1,])
exp1<-cbind(example[1,],exp)
colnames(exp1)<-exp1[1,]
exp1<-exp1[-1,]
eee<-as.numeric(exp1[,1])
exp2<-exp1[which(eee%in%as.numeric(type[,2])),]
num_type<-type[which(type[,2]%in%unique(eee)),]
geneexp<-as.numeric(exp2[,2])
pdf(file=file.path(tempdir(), "Gene expression_1.pdf"))
main = paste(gene,"expression in",type[,1],"cells")
plot(1:nrow(exp2),geneexp,main=main ,xlab = paste(type[,1],"cells"),ylab="Gene expression",col=col,pch=pch)
par(new=TRUE)
plot(1:nrow(exp2),geneexp,main=main ,xlab = paste(type[,1],"cells"),ylab="Gene expression",col=col,type="l",lwd=lwd)
dev.off()
}
