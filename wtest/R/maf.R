#' Minor Allele Frequency
#'
#' @description Calculate minor allele frequency.
#' @param data a data frame or matrix containing genotypes in the columns. Genotypes should be coded as (0, 1, 2) or (0, 1).
#' @param which.snp a numeric value, indicating which SNP to calculate. When which.snp = NULL, MAF of all the markers is calculated. Default is NULL.
#' @return The MAF of one marker.
#' @examples
#' data(diabetes.geno)
#' result <- maf(diabetes.geno, which.snp=10)
#' @export

maf <- function(data, which.snp = NULL){
  if(!is.null(which.snp) & !is.numeric(which.snp))
    stop("the 'which.snp' should be numeric!")
  if(!is.matrix(data))
    data <- as.matrix(data)
  if(any(is.na(data)))
    stop("NA occurs in data")
  if(!all(data %in% c(0,1,2)))
    stop("all the genotypes in data should be 0, 1 or 2")
  if(!is.null(which.snp)){
    result.maf <- mean(data[,which.snp])/2
  }else{
    result.maf <- colMeans(data)/2
  }
  return(result.maf)
}
