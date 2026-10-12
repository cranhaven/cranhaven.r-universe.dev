#' Odds Ratio
#'
#' @description Calculate odds ratio for a single SNP or a pair of SNPs. Single marker odds ratio is computed by contigency table as the odds of disease at minor allele vs the odds of diseases at major allele. Odds ratio of a pair of SNPs is calculated by the Logistic Regression.
#' @param data a data frame or matrix containing genotypes in the columns. Genotypes should be coded as (0, 1, 2) or (0, 1), according to minor allele count.
#' @param y binary values.
#' @param w.order a numeric number taking values 1 or 2. If w.order = 1, odds ratio of main effect is calculated. If w.order = 2, odds ratio of pairwise interaction is calculated.
#' @param which.marker a numeric vector, when w.order = 1, a single value indicating the column index of the variable to calculate; when w.order = 2, a vector indicating the column index of a SNP-pair to calculate.
#' @return The odds ratio of a SNP or a SNP-pair.
#' @export
#' @examples
#' data(diabetes.geno)
#' data(phenotype1)
#' y <- as.numeric(phenotype1)
#' OR.snp4.snp8 <- odds.ratio(diabetes.geno, y, w.order=2, which.marker = c(4,8))
#' OR.snp4 <- odds.ratio(diabetes.geno, y, w.order = 1, which.marker = 4)
#' @importFrom stats glm binomial

odds.ratio<-function(data,y,w.order,which.marker){
  if(!is.data.frame(data))
    data<-as.data.frame(data)
  if(any(is.na(data)))
    stop("NA occurs in data")
  if(!all(as.matrix(data) %in% c(0,1,2)))
    stop("all the genotypes in 'data' must be 0, 1 or 2")
  if(any(is.na(y)))
    stop("NA occurs in y")
  if(!all(y %in% c(0,1)))
    stop("all the genotypes in 'y' must be 0 or 1")
  if(!is.null(which.marker) & length(which.marker)!=w.order)
    stop("the length of 'which.marker' is not equal to 'w.order'")
  if(length(y)!=nrow(data))
    stop("'data' and 'y' must have the same length")
  n.snp<-ncol(data)
  if(w.order==1){
    or.table<-table(as.matrix(data[,which.marker]),y)
    if(0 %in% or.table)
      or.table<-or.table+0.5
    if(nrow(or.table)==3)
      result.oddsratio<-(2*or.table[3,2]+or.table[2,2])*(2*or.table[1,1]+or.table[2,1])/(2*or.table[1,2]+or.table[2,2])/(2*or.table[3,1]+or.table[2,1])
    else
      result.oddsratio<-or.table[2,2]*(2*or.table[1,1]+or.table[2,1])/(2*or.table[1,2]+or.table[2,2])/or.table[2,1]
  }
  else if(w.order==2){
    result.glm<-glm(y~data[,which.marker[1]]*data[,which.marker[2]],family=binomial)$coefficients[4]
    result.oddsratio<-unname(exp(result.glm))
  }
  return(result.oddsratio)
}

