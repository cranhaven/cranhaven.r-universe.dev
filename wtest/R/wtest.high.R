#' W-test for High Order Interaction Analysis
#'
#' @description {This function performs the \code{W-test} to calculate high-order interactions in case-control studies
#' for categorical data sets. The test measures target variables' distributional difference between cases and controls via a combined
#' log of odds ratio. It follows a Chi-squared probability distribution with data-adaptive degrees of freedom. For high-order interaction
#' calculation, the user has 3 options: (1) calculate W-test of a set of SNPs, (2) calculate high-order interaction for a list of variables,
#' which p-values are smaller than a threshold (\code{input.pval}); (3) calculate high-order interaction exhaustively for all variables.
#' Output can be filtered by p-values, such that only sets with smaller p-value than a threshold (\code{output.pval}) will be returned.}
#' @param data a data frame or matrix containing genotypes in the columns. Genotypes should be coded as (0, 1, 2) or (0, 1).
#' @param y a numeric vector of 0 or 1.
#' @param w.order an integer value, indicating the order of high-way interactions. For example, \code{w.order} = 3 for three-way interaction analysis.
#' @param hf1 \emph{h} and \emph{f} values to calculate main effect, organized as a matrix, with columns (\emph{k}, \emph{h}, \emph{f}), \emph{k} = 2 to 3.
#' @param hf.high.order \emph{h} and \emph{f} values to calculate high-order interactions, organized as a matrix, with columns (\emph{k}, \emph{h}, \emph{f}), where \emph{k} is the number of genotype combinations of a set of SNPs.
#' @param which.marker a numeric vector indicating the column index of a set of SNPs to calculate. Default \code{which.marker} = NULL gives an exhaustively high-order interaction calculation.
#' @param output.pval a p-value threshold for filtering the output. If NULL, all the results will be listed; otherwise, the function will only output the results with p-values smaller than the \code{output.pval}.
#' @param sort a logical value indicating whether or not to sort the output by p-values in ascending order. Default = TRUE.
#' @param input.pval a p-value threshold to select markers for high-order interaction calculation, used only when \code{w.order} > 2. When specified, only markers with main effect p-value smaller than \code{input.pval} will be passed to interaction effect calculation. Default = 0.10. Set \code{input.pval} = NULL or 1 for exhaustive calculation.
#' @param input.poolsize an integer, with value less than the number of input variables. It is an optional filter to control the maximum number of variables to include in high-order interaction calculation, used only when \code{w.order} > 2. When specified, the function selects top \code{input.poolsize} number of variables to calculate interactions. It can be used separately or jointly with \code{input.pval}, whichever gives smaller input pool size. Default = 10. Set \code{input.poolsize} = NULL for exhaustive calculation. It can be useful for data exploration, when there are a large number of variables with extremely small main effect p-values.
#' @return An object \code{"wtest"} containing:
#'
#' \item{order}{the "w.order" specified.}
#'
#' \item{results}{When order > 2 and which.marker = NULL, the test results include: (information of a set) [SNPs name, W-value, k, p-value]; (Information of the first variable in the set) [W-value, k, p-value]; (Information of the second variable in the set) [W-value, k, p-value] ...}
#'
#' \item{hf1}{The \emph{h} and \emph{f} values used in main effect calculation.}
#'
#' \item{hf2}{The \emph{h} and \emph{f} values used in high-order interaction calculation.}
#'
#' @details {W-test is a model-free statistical test orginally proposed to measure main effect or pairwise interactions in case-control studies with categorical variables.
#' It can be extended to high-order interaction detection by the \emph{wtest.high()} function. Theoretically, the test statistic follows a Chi-squared distribution with \emph{f} degrees of freedom. The data-adaptive degree of freedom \emph{f},
#' and a scalar \emph{h} in the test statistics allow the W-test to correct for distributional bias due to sparse data and small sample size.
#' Let \emph{k} be the number of columns of the 2 by \emph{k} contingency table formed by a single variable or a variable pair.
#' When the sample size is large and there is no population stratification, the \emph{h} and \emph{f} will approximate well to the theoretical
#' value \emph{h} = (\emph{k}-1)/\emph{k}, and \emph{f} = \emph{k}-1. When sample size is small and there is population stratification, the \emph{h} and
#' \emph{f} will vary to correct for distributional bias caused by the data structure.}
#'
#' {When \code{w.order} > 2, the \code{wtest()} will automatically calculate the main effect first and then do a pre-filter before calculating interactions.
#' This filtering is to avoid overloading the memory before having a better understanding of the data. User can specify a smaller input.pval such as 0.05 or 0.001
#' for less output, or \code{input.pval}=1 or NULL for exhaustive high-order interaction calculation. Another optional filter is \code{input.poolsize}. It will select the top \code{input.poolsize}
#' number of variables, ranked by p-values, to calculate high-order interactions. When used together with \code{input.pval}, the algorithm selects the smaller set in the high-order calculation.}
#'
#' @examples
#' data(diabetes.geno)
#' data(phenotype1)
#'
#' ## Step 1. HF Calculation
#' # Please note that parameter B is recommended to be greater than 400 for w.order = 1 or 2.
#' # For high order interaction analysis (w.order > 2), it is recommended to use default n.sample.
#' hf1 <- hf(data = diabetes.geno, w.order = 1, B = 100)
#' hf.high <- hf(data = diabetes.geno, w.order = 3, B = 30, n.marker = 10)
#'
#' ## Step 2. W-test Calculation
#' w1 <- wtest.high(diabetes.geno, phenotype1, w.order = 1, hf1 = hf1)
#' w3 <- wtest.high(diabetes.geno, phenotype1, w.order = 3, input.pval = 0.3,
#'             input.poolsize = 50, output.pval = 0.5, hf1 = hf1, hf.high.order = hf.high)
#' w.set <- wtest.high(diabetes.geno, phenotype1, w.order = 3, which.marker = c(10,13,20),
#'             hf.high.order = hf.high)
#' @export
#' @author Rui Sun, Maggie Haitian Wang
#' @references Maggie Haitian Wang, Rui Sun, Junfeng Guo, Haoyi Weng, Jack Lee, Inchi Hu, Pak Sham and Benny C.Y. Zee (2016). A fast and powerful W-test for pairwise epistasis testing. Nucleic Acids Research. doi:10.1093/nar/gkw347.
#' @seealso \code{\link{hf}}, \code{\link{w.diagnosis}}, \code{\link{w.qqplot}}
#' @importFrom utils combn
#' @importFrom stats pchisq

wtest.high<-function(data,y,w.order=3,hf1="default.hf1",hf.high.order="default.high",
                which.marker=NULL,output.pval=NULL,sort=TRUE,input.pval=0.10,input.poolsize=10){
  suppressWarnings(if(typeof(hf1)=="character"){
    hf1=array(c(0.5,0.667,1,2),dim=c(2,2))}else{hf1=hf1[,2:3]})
  suppressWarnings(if(typeof(hf.high.order)=="character"){
    hf.high.order=array(c(c(1:(3^w.order-1))/c(2:3^w.order),1:(3^w.order-1)),dim=c(3^w.order-1,2))
  }else{hf.high.order=hf.high.order[,2:3]})
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
  if(!is.null(which.marker) & length(which.marker)!=w.order)
    stop(gettextf("the length of 'which.marker' is %d, should equal to %d (the number of 'w.order' defined)",length(which.marker),w.order))
  if(length(y)!=nrow(data))
    stop("'data' and 'y' must have the same length")
  cl <- match.call()
  n.snp<-ncol(data)
  if(!is.null(which.marker)){
    set<-list(which.marker)
  }else if(w.order==1){
    set<-lapply(1:n.snp,list)
  }else if(w.order > 1){
    if(is.null(input.pval) & is.null(input.poolsize)){
      set<-apply(t(combn(n.snp,w.order)),1,list)
    }else{
      input.pval<-ifelse(is.null(input.pval),1,input.pval)
      input.poolsize<-ifelse(is.null(input.poolsize),n.snp,input.poolsize)
      set.order1<-lapply(1:n.snp,list)
      result.order1<-lapply(set.order1,x2,data,y,1)
      result.order1.all<-do.call(rbind,result.order1)
      w.value.order1<-result.order1.all[,2]*hf1[result.order1.all[,3],1]
      p.value.order1<-pchisq(w.value.order1,df=hf1[result.order1.all[,3],2],lower.tail=F)
      result.order1.all[,2]<-w.value.order1
      result.order1.all[,3]<-result.order1.all[,3]+1
      result.order1.all<-cbind(result.order1.all,p.value.order1)
      l.select<-which(result.order1.all[,4]<input.pval)
      if(length(l.select)>input.poolsize){
        result.order1.rank<-result.order1.all[order(result.order1.all[,4],decreasing=F),]
        l.select<-result.order1.rank[1:input.poolsize,1]
      }
      set<-apply(t(combn(l.select,w.order)),1,list)
    }
  }

  result<-lapply(set,x2.high,data,y,w.order)
  result.all<-do.call(rbind,result)
  if(w.order==1){
    hf<-hf1
  }else {
    hf<-hf.high.order
  }
  x2.column<-w.order+1
  df.column<-w.order+2
  pval.column<-w.order+3
  w.value<-result.all[,x2.column]*hf[result.all[,df.column],1]
  p.value<-pchisq(w.value,df=hf[result.all[,df.column],2],lower.tail=F)
  adjusted.p.value <- p.value*nrow(result.all)
  adjusted.p.value[adjusted.p.value>1] <- 1
  result.all[,x2.column]<-w.value
  result.all<-cbind(result.all,p.value,adjusted.p.value)
  k<-result.all[,df.column]+1
  result.all[,df.column]<-k
  marker.names<-colnames(data)
  result.all<-as.data.frame(result.all)
  if(w.order>1){
    snps.names<-paste0("marker",c(1:w.order))
    colnames.result.all<-c(snps.names,"w","k","p-value","adjusted.p-value")
    if(!(is.null(input.pval) & is.null(input.poolsize)) & is.null(which.marker)){
      for(i in 1:w.order){
        result.all<-cbind(result.all,result.order1.all[result.all[,i],2:4])
        main.snp.name<-c(paste0("marker",i,".w"),paste0("marker",i,".k"),paste0("marker",i,".p-value"))
        colnames.result.all<-c(colnames.result.all, main.snp.name)
      }
    }
    colnames(result.all)<-colnames.result.all
    for(j in 1:w.order){
      result.all[,j]<-marker.names[result.all[,j]]
    }
  }else{
    result.all[,1]<-marker.names[result.all[,1]]
    colnames(result.all)<-c("marker","w","k","p-value","adjusted.p-value")
  }
  if(!is.null(output.pval)){
    l.output.pval<-which(result.all[,pval.column]<output.pval)
    result.all<-result.all[l.output.pval,]
  }
  if(sort){
    l.order<-order(result.all[,pval.column],decreasing=F)
    result.all<-result.all[l.order,]
  }
  k<-c(2:(nrow(hf1)+1))
  hf1<-cbind(k,hf1)
  k<-c(2:(nrow(hf.high.order)+1))
  hf.high.order<-cbind(k,hf.high.order)
  if(!is.null(which.marker)){
    result.all <- result.all[,-ncol(result.all)]
  }
  return(list(call = cl, order=w.order, results = result.all))
}
