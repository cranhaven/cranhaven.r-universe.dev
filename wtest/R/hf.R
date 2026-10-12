Mean.Variance.calculation.by.K<-function(k,data,w.order){
  df.column<-w.order+2
  x2.column<-df.column-1
  mean.mv<-mean(data[which(data[,df.column]==k),x2.column])
  var.mv<-var(data[which(data[,df.column]==k),x2.column])
  var.mv<-ifelse(is.na(var.mv),0,var.mv)
  return(array(c(mean.mv,var.mv),dim=c(1,2)))
}

W.null.calculate.for.hf<-function(w.order,n.sample,n.marker,data){
  if(n.sample < nrow(data)){
    sample.select<-sample(nrow(data),n.sample)
    data<-data[sample.select,]
  }
  if(n.marker < ncol(data)){
    snp.select<-sample(ncol(data),n.marker)
    data<-data[,snp.select]
  }
  y<-sample(0:1,n.sample,replace=T)
  w.order<-unlist(w.order)
  set<-apply(t(combn(n.marker,w.order)),1,list)
  result<-lapply(set,x2.high,data,y,w.order)
  result.all<-do.call(rbind,result)
  k.row<-3^w.order-1
  mean.variance<-array(0,dim=c(k.row,2))
  df.column<-w.order+2
  k.min<-min(result.all[,df.column])
  k.max<-max(result.all[,df.column])
  for(i in k.min:k.max){
    mean.variance[i,]=Mean.Variance.calculation.by.K(i,result.all,w.order)
  }
  if(0 %in% mean.variance[,2]){
    mean.variance[which(mean.variance[,2]==0),1]=0
  }
  return(mean.variance)
}

#' Patameter Estimation for W-test Probability Distribution
#'
#' @description Estimate parameters (\emph{h} and \emph{f}) for \code{W-test}.
#' @param B a numeric number specifying the number of replicates. Default is 400.
#' @param data a data frame or matrix containing genotypes in the columns and subjects in the rows. Genotypes should be coded as (0, 1, 2) or (0, 1).
#' @param w.order a numeric number. \code{w.order} = 1 gives main effect calculation. \code{w.order} = 2 gives pairwise interaction calculation. \code{w.order} > 2 gives high order interaction calculation.
#' @param n.sample a numeric number specifying the number of samples to be used for estimating parameters. Default is the total number of samples in the data.
#' @param n.marker a numeric value, the number of biomarkers to include in bootstrapping. For \code{order} = 1, the default = min(P, 1000), and for order = 2, default = min(P, 50). P is the total number of markers in the data.
#' @return a set of \emph{h} and \emph{f} values indexed by \emph{k}, estimated automatically. For main effect, \emph{k} is the number of levels of a predictor variable. For interactions, \emph{k} is the number of categorical combinations of a variable pair.
#' @examples
#' data(diabetes.geno)
#'
#' # Please note that parameter B is recommended to be greater than 400.
#' # For high order interaction analysis (w.order > 2), it is recommended to use default n.sample.
#' hf1 <- hf(data = diabetes.geno, w.order = 1, B = 100)
#' hf2 <- hf(data = diabetes.geno, w.order = 2, B = 80)
#' @export
#' @author Rui Sun, Maggie Haitian Wang
#' @references Maggie Haitian Wang, Rui Sun, Junfeng Guo, Haoyi Weng, Jack Lee, Inchi Hu, Pak Sham and Benny C.Y. Zee (2016). A fast and powerful W-test for pairwise epistasis testing. Nucleic Acids Research. doi:10.1093/nar/gkw347.
#' @seealso \code{\link{wtest}}, \code{\link{w.diagnosis}}, \code{\link{w.qqplot}}
#' @importFrom utils combn
#' @importFrom stats var

hf<-function(data,w.order,B=400,n.sample=nrow(data),n.marker="default.nmarker"){
  suppressWarnings(if(typeof(n.marker)=="character") n.marker<-ifelse(w.order==1,1000,50))
  n.marker<-min(ncol(data),n.marker)
  if(is.data.frame(data))
    data<-as.matrix(data)
  if(any(is.na(data)))
    stop("NA occurs in data")
  if(!all(data %in% c(0,1,2)))
    stop("all the genotypes in 'data' must be 0, 1 or 2")
  if(!is.numeric(B))
    B<-as.numeric(B)
  set<-apply(array(w.order,dim=c(B,1)),1,list)
  result<-lapply(set,W.null.calculate.for.hf,n.sample,n.marker,data)
  result<-apply(simplify2array(result),c(1,2),sum)
  result<-result/B
  h<-result[,1]*2/result[,2]
  f<-result[,1]*h
  hf.result<-cbind(h,f)
  if(1 %in% is.na(hf.result)){
    k<-which(is.na(hf.result[,1]))
    hf.result[k,1]<-k/(k+1)
    hf.result[k,2]<-k
  }
  k<-c(2:(nrow(hf.result)+1))
  hf.result<-cbind(k,hf.result)
  return(hf.result)
}
