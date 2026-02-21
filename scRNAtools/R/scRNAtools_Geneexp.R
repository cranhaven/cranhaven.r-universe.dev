scRNAtools_Geneexp <-
function(example,types,num){
exp_1<-cbind(example[,1],example[,which(example[1,]%in%types)])
edgeR::cpm
n<-ncol(exp_1)
data1<-exp_1[,-1]
zcpm<-cpm(data1)
keep<-rowSums(zcpm>1)>=((n-1)*num)###delete the genes which expression is 0 in half of all the samples.
zset<-exp_1[keep,]
return(zset)
}
