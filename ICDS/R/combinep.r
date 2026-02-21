#' @title coverp2zscore
#' @description `coverp2zscore` calculate z-scores for p-values
#' @param pdata A numeric vector of p-values or corrected p-values
#' @return A numeric vector of z_scores
#' @export
#' @examples
#' exp.p<-GetExampleData("exp.p")
#' meth.p<-GetExampleData("meth.p")
#' cnv.p<-GetExampleData("cnv.p")
#' coverp2zscore(exp.p)
#' coverp2zscore(meth.p)
#' coverp2zscore(cnv.p)
coverp2zscore<-function(pdata){
  gene<-pdata[,1]
  pdata<-pdata[,-1]
  rownames(pdata)<-NULL
  pdata<-as.matrix(pdata)
  pdata<-as.numeric(pdata)
  pdata<-as.matrix(as.numeric(pdata))
  s.matrix<-matrix(nrow = nrow(pdata),ncol = 1)
  for(i in 1:nrow(pdata)){
    s.matrix[i,1] <--2*log(pdata[i,1])
  }
  prob<-pchisq(s.matrix, 2, ncp=0, lower.tail = FALSE, log.p = FALSE)
  z<-qnorm(prob,lower.tail = FALSE)
  fdr<-pdata[,1]
  z<-cbind(fdr,z)
  colnames(z)<-c("padjust","z_score")
  z<-as.data.frame(z)
  rownames(z)<-gene
  if (length(which(z[,1]==1))!=0) {
    z<-z[-which(z[,1]==1),]
  }
  z<-na.omit(z)
  return(z)
}


#' @title combinep_two
#' @description `combinep_two` combine two kinds of p-values,then,calculate z-score for them.
#' @param p1  A numeric vector of p-values or corrected p-values
#' @param p2  A numeric vector of p-values or corrected p-values
#' @return A numeric vector of z_scores
#' @importFrom metap sumlog
#' @export
#' @examples
#' exp.p<-GetExampleData("exp.p")
#' meth.p<-GetExampleData("meth.p")
#' combinep_two(exp.p,meth.p)

combinep_two<-function(p1,p2){

u_gene<-union(p1[,1],p2[,1])
u_gene<-as.data.frame(u_gene)
colnames(u_gene)<-"gene"
u_p<-merge(u_gene,p1,by.x="gene",by.y="gene",all.x=TRUE)
u_p<-merge(u_p,p2,by.x = "gene",by.y = "gene",all.x = TRUE)
colnames(u_p)<-c("gene","p1","p2")
u_p<-as.matrix(u_p)
u_p[is.na(u_p[,2]),2]<-1
u_p[is.na(u_p[,3]),3]<-1

gene<-u_p[,1]
u_p<-u_p[,-1]
u_p<-as.matrix(u_p)
rownames(u_p)<-gene

sumlog_p<-apply(apply(u_p, 2, as.numeric),1,sumlog)

res_sumlog_p<-matrix(nrow=length(sumlog_p),ncol=1)
for(i in 1:length(sumlog_p)){
  res_sumlog_p[i,1]<-sumlog_p[[i]]$p
}
rownames(res_sumlog_p)<-gene
z_score<-qnorm(res_sumlog_p,lower.tail = FALSE)
fdrzscore<-cbind(res_sumlog_p,z_score)
fdrzscore<-as.data.frame(fdrzscore)
colnames(fdrzscore)<-c("padjust","z_score")

if (length(which(fdrzscore[,1]==1))!=0) {
  fdrzscore<-fdrzscore[-which(fdrzscore[,1]==1),]
}

return(fdrzscore)
}

#' @title combinep_three
#' @description `combinep_three` combine three kinds of p-values,then,calculate z-score for them.
#' @param p1 the p-values or corrected p-values
#' @param p2 the p-values or corrected p-values
#' @param p3 the p-values or corrected p-values
#' @return A numeric vector of z_scores
#' @importFrom metap sumlog
#' @export
#' @examples
#' exp.p<-GetExampleData("exp.p")
#' meth.p<-GetExampleData("meth.p")
#' cnv.p<-GetExampleData("cnv.p")
#' \donttest{combinep_three(exp.p,meth.p,cnv.p)}
combinep_three <- function(p1,p2,p3) {
  u_gene<-union(p1[,1],p2[,1])
  u_gene<-union(u_gene,p3[,1])
  u_gene<-as.data.frame(u_gene)
  colnames(u_gene)<-"gene"

  u_p<-merge(u_gene,p1,by.x="gene",by.y="gene",all.x=TRUE,)
  u_p<-merge(u_p,p2,by.x = "gene",by.y = "gene",all.x = TRUE)
  u_p<-merge(u_p,p3,by.x = "gene",by.y = "gene",all.x = TRUE)
  colnames(u_p)<-c("gene","p1","p2","p3")
  u_p<-as.matrix(u_p)
  u_p[which(is.na(u_p[,2])),2]<-1
  u_p[which(is.na(u_p[,3])),3]<-1
  u_p[which(is.na(u_p[,4])),4]<-1
  gene<-u_p[,1]
  u_p<-u_p[,-1]

  sumlog_p<-apply(apply(u_p, 2, as.numeric),1,sumlog)

  res_sumlog_p<-matrix(nrow=length(sumlog_p),ncol=1)
  for(i in 1:length(sumlog_p)){
    res_sumlog_p[i,1]<-sumlog_p[[i]]$p
  }
  rownames(res_sumlog_p)<-gene
  z_score<-qnorm(res_sumlog_p,lower.tail = FALSE)
  fdrzscore<-cbind(res_sumlog_p,z_score,u_p)
  fdrzscore<-as.data.frame(fdrzscore)
  colnames(fdrzscore)<-c("padjust","z_score","exp.p","meth.p","cnv.p")
  if (length(which(fdrzscore[,1]==1))!=0) {
    fdrzscore<-fdrzscore[-which(fdrzscore[,1]==1),]
  }

  return(fdrzscore)
}





