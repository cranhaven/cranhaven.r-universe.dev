calT <- function(inData, classLabel) {
  m <- length(ind1 <- which(classLabel == 1))
  n <- length(ind2 <- which(classLabel == 0))
  inData1 <- inData[, ind1,drop=FALSE]
  inData2 <- inData[, ind2,drop=FALSE]
  rmean1 <- rowMeans(inData1)
  rmean2 <- rowMeans(inData2)
  ss1 <- rowSums((inData1 - rmean1)^2)
  ss2 <- rowSums((inData2 - rmean2)^2)
  tt <- (m + n - 2)^0.5 * (rmean2 - rmean1)/((1/m + 1/n) *                                             (ss1 + ss2))^0.5
  return(list(T = tt, df = m + n - 2))
}
rmnan<-function(x){
  n<-nrow(x)
  k<-NULL
  for(i in 1:n){
    if(any(is.nan(x[i,])))
      k<-c(k,i)
  }
  return(k)
}
#' @title getExpp
#' @description `getExpp` perform t-test on Expression profile data
#' @param exp_data A data frame, the expression profile to calculate p-value for each gene, the rownames should be the symbol of genes.
#' @param label A vector of 0/1s, indicating the class of samples in the expression profile, 0 represents case, 1 represents control.
#' @param p.adjust Logical,tell if returns corrected p-values
#' @param method Correction method,which can be one of "holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
#   "fdr", "none"
#' @details  For a given expression profile of two conditions, ICDS package provide t-test method to calculate p-values or corrected p-values(if p.adjust=TRUE,return corrected p-values,if p.adjust=FALSE,return p-values.) for each genes. The row of the expression profile should be gene symbols and the column of the expression profile should be names of samples. Samples should be under two conditions and the label should be given as 0 and 1.
#' @return A numeric vector of p-values or corrected p-values
#' @importFrom stats pt
#' @importFrom stats pchisq
#' @importFrom stats qnorm
#' @export
#' @examples
#' profile<-GetExampleData("exp_data")
#' label<-GetExampleData("label1")
#' getExpp(profile,label,p.adjust=FALSE)
getExpp <- function(exp_data,label,p.adjust=TRUE,method="fdr") {
  t<-calT(exp_data,label)
  p.t<-pt(abs(t$T),df = t$df,lower.tail = FALSE)
  if (p.adjust==TRUE) {
    p.t<-p.adjust(p.t,method)
    exp.p<-cbind(rownames(exp_data),p.t)
    colnames(exp.p)<-c("gene","fdr")
  }
  else {
    exp.p<-cbind(rownames(exp_data),p.t)
    colnames(exp.p)<-c("gene","p")
  }
  m<-as.numeric(exp.p[,2])
  m<-as.data.frame(m)
  k<-rmnan(m)
  exp.p<-as.data.frame(exp.p)
  if (!is.null(k)) {
    exp.p<-exp.p[-k,]
  }
  exp.p<-na.omit(exp.p)
  return(exp.p)
}
#' @title getMethp
#' @description `getMethp` perform t-test on Methylation profile data
#' @param meth_data A data frame, the Methylation profile to calculate p-value for each gene, the rownames should be the symbol of genes.
#' @param label label A vector of 0/1s, indicating the class of samples in the Methylation profile, 0 represents case, 1 represents control.
#' @param p.adjust Logical,tell if returns corrected p-values
#' @param method Correction method,which can be one of "holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
#   "fdr", "none".
#' @details For a given Methylation profile of two conditions, ICDS package provide t-test method to calculate p-values or corrected p-values(if p.adjust=TRUE,return corrected p-values,if p.adjust=FALSE,return p-values.) for each genes. The row of the Methylation profile should be gene symbols and the column of the Methylation profile should be names of samples. Samples should be under two conditions and the label should be given as 0 and 1.
#' @return A numeric vector of p-values or corrected p-values
#' @export
#' @examples
#' profile<-GetExampleData("meth_data")
#' label<-GetExampleData("label2")
#' getMethp(profile,label,p.adjust=FALSE)
getMethp <- function(meth_data,label,p.adjust=TRUE,method="fdr") {
  t<-calT(meth_data,label)
  p.t<-pt(abs(t$T),df = t$df,lower.tail = FALSE)
  if (p.adjust==TRUE) {
    p.t<-p.adjust(p.t,method)
    meth.p<-cbind(rownames(meth_data),p.t)
    colnames(meth.p)<-c("gene","fdr")
  }else {
    meth.p<-cbind(rownames(meth_data),p.t)
    colnames(meth.p)<-c("gene","p")
  }
  m<-as.numeric(meth.p[,2])
  m<-as.data.frame(m)
  k<-rmnan(m)
  meth.p<-as.data.frame(meth.p)
  if (!is.null(k)) {
    meth.p<-meth.p[-k,]
  }
  meth.p<-na.omit(meth.p)
  return(meth.p)
}
#' @title getCnvp
#' @description `getCnvp` perform t-test on copy number variation data
#' @param exp_data A data frame
#' @param cnv_data Copy number variation data
#' @param amp_gene A vector of strings, the IDs of amplified genes.
#' @param del_gene A vector of strings, the IDs of deleted genes.
#' @param p.adjust Logical,tell if returns corrected p-values
#' @param method Correction method,which can be one of "holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
#   "fdr", "none".
#' @details cnv_data is TCGA level4 data.if p.adjust=TRUE,return corrected p-values,if p.adjust=FALSE,return p-values
#' @return A numeric vector of p-values or corrected p-values
#' @export
#' @examples
#' exp_data<-GetExampleData("exp_data")
#' meth_data<-GetExampleData("meth_data")
#' cnv_data<-GetExampleData("cnv_data")
#' amp_gene<-GetExampleData("amp_gene")
#' del_gene<-GetExampleData(("del_gene"))
#' getCnvp(exp_data,cnv_data,amp_gene,del_gene,p.adjust=FALSE,method="fdr")
getCnvp <- function(exp_data,cnv_data,amp_gene,del_gene,p.adjust=TRUE,method="fdr") {
  exp_sample<-colnames(exp_data)
  cnv_sample<-colnames(cnv_data)
  tumer2<-intersect(exp_sample,cnv_sample)
  cnv_tumer<-cnv_data[,tumer2]

  CNdata<-cnv_tumer
  Gdata<-exp_data[,tumer2]

  CNdata<-CNdata[which(rownames(CNdata) %in% rownames(Gdata)),]
  Gdata<-Gdata[which(rownames(Gdata) %in% rownames(CNdata)),]

  Gdata<-Gdata[as.vector(order(rownames(Gdata))),]
  CNdata<-CNdata[as.vector(order(rownames(CNdata))),]

  ana_p<-c()
  for(i in 1:nrow(CNdata)){

    ana_mat<-matrix(nrow=2,ncol=ncol(CNdata))
    ana_mat[1,]<-as.numeric(CNdata[i,])
    ana_mat[2,]<-as.numeric(Gdata[which(rownames(Gdata)==rownames(CNdata)[i]),])
    ana_mat[1,which(ana_mat[1,]>=1)]<-1
    ana_mat[1,which(ana_mat[1,]<=0)]<-0

    if(length(which(ana_mat[1,]==1))>1){

      m<-length(which(ana_mat[1,]==1))
      n<-length(which(ana_mat[1,]==0))
      case_rowmean<-mean(ana_mat[2,which(ana_mat[1,]==1)])
      control_rowmean<-mean(ana_mat[2,which(ana_mat[1,]==0)])
      case_ss<-sum((ana_mat[2,which(ana_mat[1,]==1)]-case_rowmean)^2)
      control_ss<-sum((ana_mat[2,which(ana_mat[1,]==0)]-control_rowmean)^2)
      ana_t<-(m+n-2)^0.5*(case_rowmean-control_rowmean)/((1/m + 1/n)*(case_ss+control_ss))^0.5
      ana_p[i]<-pt(abs(ana_t),lower.tail=F,df=m+n-2)
    }
    else{
      ana_p[i]<-1
    }
  }

  names(ana_p)<-rownames(CNdata)
  ana_biger_p<-ana_p
  ana_biger_p<-as.matrix(ana_biger_p)
  ana_P<-c()
  for(i in 1:nrow(CNdata)){
    ana_MAT<-matrix(nrow=2,ncol=ncol(CNdata))
    ana_MAT[1,]<-as.numeric(CNdata[i,])
    ana_MAT[2,]<-as.numeric(Gdata[which(rownames(Gdata)==rownames(CNdata)[i]),])
    ana_MAT[1,which(ana_MAT[1,]>=0)]<-0
    ana_MAT[1,which(ana_MAT[1,]<=-1)]<-1

    if(length(which(ana_MAT[1,]==1))>1){

      m<-length(which(ana_MAT[1,]==1))
      n<-length(which(ana_MAT[1,]==0))
      case_rowmean<-mean(ana_MAT[2,which(ana_MAT[1,]==1)])
      control_rowmean<-mean(ana_MAT[2,which(ana_MAT[1,]==0)])
      case_ss<-sum((ana_MAT[2,which(ana_MAT[1,]==1)]-case_rowmean)^2)
      control_ss<-sum((ana_MAT[2,which(ana_MAT[1,]==0)]-control_rowmean)^2)
      ana_t<-(m+n-2)^0.5*(case_rowmean-control_rowmean)/((1/m + 1/n)*(case_ss+control_ss))^0.5

      ana_P[i]<-pt(abs(ana_t),lower.tail=F,df=m+n-2)
    }
    else{
      ana_P[i]<-1
    }
  }
  names(ana_P)<-rownames(CNdata)
  ana_smaller_P<-ana_P
  ana_smaller_P<-as.matrix(ana_smaller_P)

  amp_gene<-unlist(amp_gene)
  del_gene<-unlist(del_gene)
  cnv.p<-cbind(names(ana_p),1)
  for(i in 1:nrow(cnv.p)){
    if((cnv.p[i,1]%in%amp_gene) & (cnv.p[i,1]%in%del_gene)){
      cnv.p[i,2]<-1
    }else if (cnv.p[i,1]%in%amp_gene) {
      cnv.p[i,2]<-ana_biger_p[cnv.p[i,1],1]
    }else if(cnv.p[i,1]%in%del_gene){
      cnv.p[i,2]<-ana_smaller_P[cnv.p[i,1],1]
    }else{
      cnv.p[i,2]<-1
    }
  }
  gene<-cnv.p[,1]
  cnv.p<-cnv.p[,-1]
  cnv.p<-as.numeric(cnv.p)
  cnv.p<-as.matrix(cnv.p)
  rownames(cnv.p)<-gene
  if (p.adjust==TRUE) {
    cnv.p<-p.adjust(cnv.p,method)
    cnv.p<-cbind(gene,cnv.p)
    colnames(cnv.p)<-c("gene","fdr")
  }else {
    cnv.p<-cbind(gene,cnv.p)
    colnames(cnv.p)<-c("gene","p")
  }
  m<-as.numeric(cnv.p[,2])
  m<-as.data.frame(m)
  k<-rmnan(m)
  cnv.p<-as.data.frame(cnv.p)
  if (!is.null(k)) {
    cnv.p<-cnv.p[-k,]
  }
  cnv.p<-na.omit(cnv.p)
  return(cnv.p)
}
