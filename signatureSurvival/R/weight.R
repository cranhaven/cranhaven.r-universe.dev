weight <-
function(results,signature){
fn<-length(results)  
for (i in seq(fn)) {
colname<-colnames(results[[i]])

ind<-is.element(colname,c("HR","hr","beta","coefficient","hazard.rate",
                          "Hazard.rate","Hazard.ratio","hazard.ratio"))
j<-which(ind==TRUE)

if(length(j)==1){
  colname[j]<-"HR"
}else{
  stop("no HR column in the results")
}
ind<-is.element(colname,c("pv","p-value","pvalue","p_value","P-value","P_value",
                          "p.value","P.value"))
j<-which(ind==TRUE)
if(length(j)==1){
  colname[j]<-"pvalue"
}else{
  stop("no pvalue column in the result")
}
ind<-is.element(colname,c("Gene","gene","gene_id","gene_ID","symbol","Gene_ID"))
j<-which(ind==TRUE)
if(length(j)==1){
  colname[j]<-"gene"
}else{
  stop("no gene column in the result")
}
colnames(results[[i]])<-colname
}
if(length(dim(signature))==2){
  sn<-nrow(signature)
}else{
sn<-length(signature)
}
weights<-matrix(NA,nrow =sn ,ncol = fn)

for (i in seq(fn)) {
result<-results[[i]]  
pv<-na.omit(result$pvalue)
lgp<-(-log2(pv))
sumlgp<-sum(lgp)
weight<-lgp/sumlgp
gene<-result$gene
gn<-length(gene)
for (j in seq(sn)) {
  for (k in seq(gn)) {
  if(length(dim(signature))==2){
    if(identical(signature[j,1],gene[k])||identical(signature[j,2],gene[k])){
      weights[j,i]<-weight[k]
    }
  }else{    
   if(identical(signature[j],gene[k])){
     weights[j,i]<-weight[k]
         }
       }
     }
   }
}

WX<-apply(weights,1,mean,na.rm=TRUE)
WX<-cbind(as.data.frame(signature),WX)
if(length(dim(signature))==2){
colnames(WX)<-c(colnames(signature),"weight")  
}else{
colnames(WX)<-c("signature","weight")
}
return(WX)
}
