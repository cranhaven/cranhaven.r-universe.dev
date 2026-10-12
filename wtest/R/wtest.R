#' W-test
#'
#' @description {This function performs the \code{W-test} to calculate main effect or pairwise interactions in case-control studies
#' for categorical data sets. The test measures target variables' distributional difference between cases and controls via a combined
#' log of odds ratio. It follows a Chi-squared probability distribution with data-adaptive degrees of freedom. For pairwise interaction
#' calculation, the user has 3 options: (1) calculate a single pair's W-value, (2) calculate pairwise interaction for a list of variables,
#' which p-values are smaller than a threshold (\code{input.pval}); (3) calculate the pairwise interaction exhaustively for all variables.
#' For both main and interaction calculation, the output can be filtered by p-values, such that only sets with smaller p-value
#' than a threshold (\code{output.pval}) will be returned. An extension of the W-test for rare variant analysis is available in \code{zfa} package.}
#' @param data a data frame or matrix containing genotypes in the columns. Genotypes should be coded as (0, 1, 2) or (0, 1).
#' @param y a numeric vector of 0 or 1.
#' @param w.order an integer value of 0 or 1. \code{w.order} = 1 for main effect calculation; \code{w.order} = 2 for pairwise calculation.
#' @param hf1 \emph{h} and \emph{f} values to calculate main effect, organized as a matrix, with columns (\emph{k}, \emph{h}, \emph{f}), \emph{k} = 2 to 3. Needed when \code{w.order} = 1.
#' @param hf2 \emph{h} and \emph{f} values to calculate interaction associations, organized as a matrix, with columns (\emph{k}, \emph{h}, \emph{f}), \emph{k} = 2 to 9. Needed when \code{w.order} = 2.
#' @param which.marker a numeric vector, when \code{w.order} = 1, a single value indicating the column index of a SNP to calculate, when \code{w.order} = 2, a vector indicating the column index of a SNP-pair to calculate. Default \code{which.marker} = NULL means main or interaction effect will be calculated exhaustively.
#' @param output.pval a p-value threshold for filtering the output. If NULL, all the results will be listed; otherwise, the function will only output the results with p-values smaller than the \code{output.pval}.
#' @param sort a logical value indicating whether or not to sort the output by p-values in ascending order. Default = TRUE.
#' @param input.pval a p-value threshold to select markers for pairwise calculation, used only when \code{w.order} = 2. When specified, only markers with main effect p-value smaller than \code{input.pval} will be passed to interaction effect calculation. Default = 0.10. Set \code{input.pval} = NULL or 1 for exhaustive pairwise calculation.
#' @param input.poolsize an integer, with value less than the number of input variables. It is an optional filter to control the maximum number of variables to include in pairwise calculation, used only when \code{w.order} = 2. When specified, the function selects top \code{input.poolsize} number of variables to calculate pairwise interactions. It can be used separately or jointly with \code{input.pval}, whichever gives smaller input variable pool size. Default = 50. Set \code{input.poolsize} = NULL for exhaustive pairwise calculation. It can be useful for data exploration, when there are a large number of variables with extremely small main effect p-values.
#' @return An object \code{"wtest"} containing:
#'
#' \item{order}{the "w.order" specified.}
#'
#' \item{results}{When \code{w.order} = 1, the test results include: the ID of SNP, the W value, \emph{k}, and p-value. When \code{w.order} = 2 and \code{which.marker} = NULL, the test results include: (information of the pair, column 1-5) [SNP1 name, SNP2, name, W-value, k, p-value]; (Information of the first variable in the pair, column 6-8) [W-value, k, p-value]; (Information of the second variable in the pair, column 9-11) [W-value, k, p-value].}
#'
#' \item{hf1}{The \emph{h} and \emph{f} values used in main effect calculation.}
#'
#' \item{hf2}{The \emph{h} and \emph{f} values used in pairwise interaction calculation.}
#'
#' @details {W-test is a model-free statistical test to measure main effect or pairwise interactions in case-control studies with categorical variables.
#' Theoretically, the test statistic follows a Chi-squared distribution with \emph{f} degrees of freedom. The data-adaptive degree of freedom \emph{f},
#' and a scalar \emph{h} in the test statistics allow the W-test to correct for distributional bias due to sparse data and small sample size.
#' Let \emph{k} be the number of columns of the 2 by \emph{k} contingency table formed by a single variable or a variable pair.
#' When the sample size is large and there is no population stratification, the \emph{h} and \emph{f} will approximate well to the theoretical
#' value \emph{h} = (\emph{k}-1)/\emph{k}, and \emph{f} = \emph{k}-1. When sample size is small and there is population stratification, the \emph{h} and
#' \emph{f} will vary to correct for distributional bias caused by the data structure.}
#'
#' {When \code{w.order} =2, the \code{wtest()} will automatically calculate the main effect first and then do a pre-filter before calculating interactions.
#' This filtering is to avoid overloading the memory before having a better understanding of the data. User can specify a smaller input.pval such as 0.05 or 0.001
#' for less output, or \code{input.pval}=1 or NULL for exhaustive pairwise calculation. Another optional filter is \code{input.poolsize}. It will take the top \code{input.poolsize}
#' number of variables to calculated pairwise effect exhaustively, selected by smallest p-value; when used together with \code{input.pval}, the smaller set will be passed to pairwise calculation.}
#'
#' @examples
#' data(diabetes.geno)
#' data(phenotype1)
#'
#' ## Step 1. HF Calculation
#' # Please note that parameter B is recommended to be greater than 400.
#' hf1 <- hf(data = diabetes.geno, w.order = 1, B = 100)
#' hf2 <- hf(data = diabetes.geno, w.order = 2, B = 50)
#'
#' ## Step 2. W-test Calculation
#' w1 <- wtest(diabetes.geno, phenotype1, w.order = 1, hf1 = hf1)
#' w2 <- wtest(diabetes.geno, phenotype1, w.order = 2, input.pval = 0.3,
#'             input.poolsize = 50, output.pval = 0.01, hf1 = hf1, hf2 = hf2)
#' w.pair <- wtest(diabetes.geno, phenotype1, w.order = 2, which.marker = c(10,13), hf2 = hf2)
#' @export
#' @author Rui Sun, Maggie Haitian Wang
#' @references Maggie Haitian Wang, Rui Sun, Junfeng Guo, Haoyi Weng, Jack Lee, Inchi Hu, Pak Sham and Benny C.Y. Zee (2016). A fast and powerful W-test for pairwise epistasis testing. Nucleic Acids Research. doi:10.1093/nar/gkw347.
#' @references Maggie Haitian Wang, Haoyi Weng, Rui Sun, Jack Lee, William K.K. Wu, Ka Chun Chong, Benny C.Y. Zee. (2017). A Zoom-Focus algorithm (ZFA) to locate the optimal testing region for rare variant association tests. Bioinformatics, 33(15), 2330-2336.
#' @seealso \code{\link{hf}}, \code{\link{w.diagnosis}}, \code{\link{w.qqplot}}
#' @importFrom utils combn
#' @importFrom stats pchisq

wtest<-function(data, y, w.order = c(1,2), hf1="default.hf1", hf2="default.hf2",
                which.marker = NULL, output.pval = NULL, sort = TRUE, input.pval = 0.10, input.poolsize = 150){
  suppressWarnings(if(typeof(hf1) == "character"){
    hf1 = array(c(0.5,0.667,1,2), dim=c(2,2))
    }else{
      hf1 = hf1[,2:3]
      })
  suppressWarnings(if(typeof(hf2) == "character"){
    hf2 = array(c(0.5,0.667,0.75,0.8,0.833,0.857,0.875,0.889,1:8), dim=c(8,2))
    }else{
      hf2 = hf2[,2:3]
      })
  if(is.data.frame(data))
    data <- as.matrix(data)
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
  n.snp <- ncol(data)
  if(!is.null(which.marker)){
    set <- list(which.marker)
  }else if(w.order==1){
    set <- lapply(1:n.snp,list)
  }else if(w.order==2){
    if(is.null(input.pval) & is.null(input.poolsize)){
      set <- apply(t(combn(n.snp,2)), 1, list)
    }else{
      input.pval <- ifelse(is.null(input.pval), 1, input.pval)
      input.poolsize <- ifelse(is.null(input.poolsize), n.snp, input.poolsize)
      set.order1 <- lapply(1:n.snp, list)
      result.order1 <- lapply(set.order1, x2, data, y, 1)
      result.order1.all <- do.call(rbind, result.order1)
      w.value.order1 <- result.order1.all[,2]*hf1[result.order1.all[,3],1]
      p.value.order1 <- pchisq(w.value.order1, df = hf1[result.order1.all[,3],2], lower.tail = F)
      result.order1.all[,2] <- w.value.order1
      result.order1.all[,3] <- result.order1.all[,3] + 1
      result.order1.all <- cbind(result.order1.all, p.value.order1)
      l.select <- which(result.order1.all[,4] < input.pval)
      if(length(l.select) > input.poolsize){
        result.order1.rank <- result.order1.all[order(result.order1.all[,4], decreasing=F),]
        l.select <- result.order1.rank[1:input.poolsize,1]
      }
      set <- apply(t(combn(l.select,2)), 1, list)
    }
  }
  result <- lapply(set, x2, data, y, w.order)
  result.all <- do.call(rbind, result)
  x2.column <- ifelse(w.order == 1, 2, 3)
  if(w.order == 1){
    hf <- hf1
  }else {
    hf <- hf2
  }
  df.column <- x2.column + 1
  pval.column <- x2.column + 2
  w.value <- result.all[,x2.column] * hf[result.all[,df.column],1]
  p.value <- pchisq(w.value, df = hf[result.all[,df.column],2], lower.tail = F)
  result.all[,x2.column] <- w.value
  result.all <- cbind(result.all, p.value)
  k <- result.all[,df.column] + 1
  result.all[,df.column] <- k
  marker.names <- colnames(data)
  result.all <- as.data.frame(result.all)
  if(w.order == 2){
    colnames(result.all) <- c("marker1", "marker2", "w", "k", "p-value")
    if(!(is.null(input.pval) & is.null(input.poolsize)) & is.null(which.marker)){
      result.all <- cbind(result.all, result.order1.all[result.all[,1],2:4], result.order1.all[result.all[,2],2:4])
      colnames(result.all) <- c("marker1", "marker2", "w", "k", "pair.p-value", "marker1.w", "marker1.k", "marker1.p-value", "marker2.w", "marker2.k", "marker2.p-value")
    }
    result.all[,1] <- marker.names[result.all[,1]]
    result.all[,2] <- marker.names[result.all[,2]]
  }else{
    result.all[,1] <- marker.names[result.all[,1]]
    colnames(result.all) <- c("marker", "w", "k", "p-value")
  }
  if(!is.null(output.pval)){
    l.output.pval <- which(result.all[,pval.column] < output.pval)
    result.all <- result.all[l.output.pval,]
  }
  if(sort){
    l.order <- order(result.all[,pval.column], decreasing = F)
    result.all <- result.all[l.order,]
  }
  k <- c(2:(nrow(hf1) + 1))
  hf1 <- cbind(k, hf1)
  k <- c(2:(nrow(hf2) + 1))
  hf2 <- cbind(k, hf2)
  return(list(call = cl, order = w.order, results = result.all, hf1 = hf1, hf2 = hf2))
}
