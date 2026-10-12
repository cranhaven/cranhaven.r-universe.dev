#' W P-values Diagnosis by Q-Q Plot
#' @description Draw a Q-Q plot for W-test
#' @param data a data frame or matrix containing genotypes in the columns. Genotypes should be coded as (0, 1, 2) or (0, 1).
#' @param y a numeric vector of 0 or 1.
#' @param w.order a numeric number taking values 1 or 2. \code{w.order} = 1 gives main effect Q-Q plot. \code{w.order} = 2 gives interaction Q-Q plot.
#' @param hf1 \emph{h} and \emph{f} values to calculate main effect, organized as a matrix, with columns (\emph{k}, \emph{h}, \emph{f}), \emph{k} = 2 to 3. Needed when \code{w.order} = 1.
#' @param hf2 \emph{h} and \emph{f} values to calculate interaction associations, organized as a matrix, with columns (\emph{k}, \emph{h}, \emph{f}), \emph{k} = 2 to 9. Needed when \code{w.order} = 2.
#' @param input.poolsize a numeric number; The maximum number of SNPs to calculate the Q-Q plot. Default is 200. The \code{input.poolsize} is suggested to set as 1000 for \code{w.order} = 1, and 200 for \code{w.order} = 2.
#' @param ... graphical parameters.
#' @return Q-Q plot
#' @details
#' With a given data and y, the p-value of W-test is calculated at given \emph{h} and \emph{f} values, which are plotted against the theoretical distribution.
#'
#' @examples
#' data(diabetes.geno)
#' data(phenotype1)
#' ## Step 1. HF Calculation
#' # Please note that parameter B is recommended to be greater than 400.
#' hf1<-hf(data = diabetes.geno, w.order = 1, B = 200)
#'
#' ## Step 2. Q-Q Plot
#' w.qqplot(data = diabetes.geno, y = phenotype1, w.order = 1, hf1 = hf1, cex =.5)
#' abline(0,1)
#' @export
#' @importFrom utils combn
#' @importFrom stats pchisq runif qqplot
#'
w.qqplot<-function(data, y, w.order=c(1,2), input.poolsize=200, hf1="default.hf1", hf2="default.hf2",
                   ...){
  suppressWarnings(if(typeof(hf1)=="character"){hf1=array(c(0.5,0.667,1,2),dim=c(2,2))}else{hf1=hf1[,2:3]})
  suppressWarnings(if(typeof(hf2)=="character"){hf2=array(c(0.5,0.667,0.75,0.8,0.833,0.857,0.875,0.889,1:8),dim=c(8,2))}else{hf2=hf2[,2:3]})
  if(is.data.frame(data))
    data<-as.matrix(data)
  if(any(is.na(data)))
    stop("NA occurs in data")
  if(!all(data %in% c(0,1,2)))
    stop("all the genotypes in 'data' must be 0, 1 or 2")
  if(any(is.na(y)))
    stop("NA occurs in y")
  if(!all(y %in% c(0,1)))
    stop("all the genotypes in 'y' must be 0 or 1")
  n.snp<-ncol(data)
  if(w.order==1){
    set<-lapply(1:n.snp,list)
  }else if(w.order==2){
    if(n.snp<=input.poolsize){
      set<-apply(t(combn(n.snp,2)),1,list)
    }else{
      l.select<-sample(1:n.snp,input.poolsize,replace=F)
      set<-apply(t(combn(l.select,2)),1,list)
    }
  }
  result<-lapply(set,x2,data,y,w.order)
  result.all<-do.call(rbind,result)
  x2.column<-ifelse(w.order==1,2,3)
  if(w.order==1){
    hf<-hf1
  }else {
    hf<-hf2
  }
  df.column<-x2.column+1
  pval.column<-x2.column+2
  w.value<-result.all[,x2.column]*hf[result.all[,df.column],1]
  p.value.observed<-pchisq(w.value,df=hf[result.all[,df.column],2],lower.tail=F)
  o = -log10(sort(p.value.observed,decreasing=F))
  e = -log10(1:length(o)/length(o))
  qqplot(e,o, xlab = paste0("Expected -log(p-value)"), ylab = "Observed -log(p-value)", ...)
}
