
##' @description Calculate the difference of correlation coefficents between cases and controls,
##' conduct \eqn{Z} test for the differences (values) and choose variant pairs with the significance below the given threshold for output.
##' @title Parallelized calculation of the difference of correlation coefficients and compute \eqn{Z} test with one genotype input
##' @export
##' @param geno is the normalized genotype data. It can be a matrix or a dataframe, or a big.matrix object (from \pkg{bigmemory}. 
##' The columns contain the information of variables and the rows contain the information of samples. 
##' @param pheno a vector containing the binary phenotype information (case/control). The values are either 0 (control) or 1 (case). 
##' @param zpthres is the significance threshold to select variant pairs for output. Default is 1e-6.
##' @param chunk is the number of variants in each chunk. Default: 1000.
##' @param outfile is the base of out filename. Default: 'NONE'.
##' @param suffix is the suffix of out filename. Default: '.txt'.
##' @param ... not used.
##' @return null
##' @author Beibei Jiang \email{beibei_jiang@@psych.mpg.de} and Benno Pütz \email{Benno Pütz \email{puetz@@psych.mpg.de}
##' @examples 
##' # simulate some data
#' set.seed(123)
#' geno1 <- matrix(sample(0:2, size = 1000, replace = TRUE, prob = c(0.5, 0.3, 0.2)), ncol = 10)
#' dimnames(geno1) <- list(row = paste0("IND", 1:nrow(geno1)), col = paste0("rs", 1:ncol(geno1)))
#' p1 <- c(rep(0, 60), rep(1, 40))
#' 
#' # normalized data
#' geno1 <- scale(geno1)
#' 
#' # one genotype with case-control phenotype
#' epiblaster1geno(geno = geno1, 
#' pheno = p1,
#' outfile = "episcan_1geno_cc", 
#' suffix = ".txt", 
#' zpthres = 0.9, 
#' chunk = 10)
#' 
#' # take a look at the result
#' res <- read.table("episcan_1geno_cc.txt", 
#' header = TRUE, 
#' stringsAsFactors = FALSE)
#' head(res)
epiblaster1geno <- function(geno,
                            pheno,
                            chunk = 1000,
                            zpthres = 10e-06,
                            outfile = "NONE",
                            suffix = ".txt",
                            ...){
  zthres <- abs(qnorm(zpthres/2))
  ## output head
  # check whether output file exisit or not; if yes, delete
  OUT <- paste0(outfile, suffix)
  if(file.exists(OUT)) file.remove(OUT)
  cat(paste("SNP1", "SNP2", "Zscore", "ZP",
            sep = " "),
      "\n",
      file = OUT,
      append = TRUE)   # why append? file was removed had it been present ...
  
  nSNP <- ncol(geno)
  nsplits <- ceiling(nSNP / chunk)  # number of splits (i.e., of size chunk, possibly incomplete at end)
  
  is.case <- pheno == 1
  control <- as.matrix(geno[!is.case, ])    # ctrl.data
  case <- as.matrix(geno[is.case, ])        # case.data
  rm(is.case)
  sd_tot <- sqrt(1/(nrow(control)-1) + 1/(nrow(case)-1))
  gc()
  
  ############## calculation ##################
  for ( i in 1:nsplits)
  {
    print(paste(i, "chunk loop:", date()))
    for (j in i:nsplits)
    {
      ztest <- (getcor(A = as.matrix(case[, ithChunk(i, nSNP, chunk), drop = FALSE]),
                       B = as.matrix(case[, ithChunk(j, nSNP, chunk), drop = FALSE]),
                       method = "pearson")
                -
                  getcor(A = as.matrix(control[, ithChunk(i, nSNP, chunk), drop = FALSE]),
                         B = as.matrix(control[, ithChunk(j, nSNP, chunk), drop = FALSE]),
                         method = "pearson") )  /  sd_tot
      
      index <- which(abs(ztest) >= zthres, arr.ind = TRUE)
      
      ifelse(i==j,
             WriteSnpPairs_sym,
             WriteSnpPairs)(Zmatrix = ztest, indexArr = index,
                            outfile = OUT)
      rm(list = c("ztest", "index"))
      gc()
    }
  }
  
  
 
  print("epiblaster calculation is over!")
  print(date())
  
}



##' @description Calculate the difference of correlation coeficents between cases and controls,
##' conduct Z test for the differences (values) and choose variant pairs with the significance below the given threshold for output.
##' @title Parallelized calculattion of the difference of correlation coefficients and compute Z test with two genotype inputs
##' @export
##' @param geno1 is the first normalized genotype data. It can be a matrix or a dataframe, or a big.matrix object from \pkg{bigmemory}. 
##' The columns contain the information of variables and the rows contain the information of samples. 
##' @param geno2 is the second normalized genotype data. It can be a matrix or a dataframe, or a big.matrix object from \pkg{bigmemory}. 
##' The columns contain the information of variables and the rows contain the information of samples. 
##' @param pheno a vector containing the binary phenotype information (case/control). The values are either 0 (control) or 1 (case).
##' @param zpthres is the significance threshold to select variant pairs for output. Default is 1e-6.
##' @param chunk is the number of variants in each chunk.
##' @param outfile is the prefix of out filename.
##' @param suffix is the suffix of out filename.
##' @param ... not used.
##' @return null
##' @author Beibei Jiang \email{beibei_jiang@@psych.mpg.de} and Benno Pütz \email{Benno Pütz \email{puetz@@psych.mpg.de}
##' @examples 
##' # simulate some data
#' set.seed(123)
#' geno1 <- matrix(sample(0:2, size = 1000, replace = TRUE, prob = c(0.5, 0.3, 0.2)), ncol = 10)
#' geno2 <- matrix(sample(0:2, size = 2000, replace = TRUE, prob = c(0.4, 0.3, 0.3)), ncol = 20)
#' dimnames(geno1) <- list(row = paste0("IND", 1:nrow(geno1)), col = paste0("rs", 1:ncol(geno1)))
#' dimnames(geno2) <- list(row = paste0("IND", 1:nrow(geno2)), col = paste0("exm", 1:ncol(geno2)))
#' p1 <- c(rep(0, 60), rep(1, 40))
#' 
#' # normalized data
#' geno1 <- scale(geno1)
#' geno2 <- scale(geno2)
#' 
#' # two genotypes with quantitative phenotype
#' epiblaster2genos(geno1 = geno1, 
#' geno2 = geno2, 
#' pheno = p1, outfile = "episcan_2geno_cc", 
#' suffix = ".txt", 
#' zpthres = 0.9, 
#' chunk = 10)
#' 
#' # take a look at the result
#' res <- read.table("episcan_2geno_cc.txt", 
#' header = TRUE, 
#' stringsAsFactors = FALSE)
#' head(res)
epiblaster2genos <- function(geno1,
                             geno2,
                             pheno,
                             chunk = 1000,
                             zpthres = 10e-06,
                             outfile = "NONE",
                             suffix = ".txt",
                             ...){
  zthres <- abs(qnorm(zpthres/2))
  ## output head
  # check whether output file exisit or not; if yes, delete
  OUT <- paste(outfile, suffix, sep = "")
  if(file.exists(OUT)) file.remove(OUT)
  cat(paste("SNP1", "SNP2", "Zscore", "ZP",
            sep = " "),
      "\n",
      file = OUT,
      append = TRUE)
  
  nSNP1 <- ncol(geno1)
  nsplits1 <- ceiling(nSNP1/chunk)  # this will include "incomplete" chunk at end if nSNP1%%chunk != 0
  
  nSNP2 <- ncol(geno2)
  nsplits2 <- ceiling(nSNP2/chunk)
  
  is.case <- pheno == 1
  control1 <- as.matrix(geno1[!is.case, ])    # ctrl.data1
  case1 <- as.matrix(geno1[is.case, ])        # case.data1
  control2 <- as.matrix(geno2[!is.case, ])    # ctrl.data2
  case2 <- as.matrix(geno2[is.case, ])       # case.data2
  
  sd_tot <- sqrt(1/(sum(!is.case)-1) + 1/(sum(is.case)-1))
  #sd_tot <- sqrt(1/(nrow(control1)-1) + 1/(nrow(case1)-1))
  rm(is.case)
  gc()
  
  for ( i in 1:nsplits1)
  {
    print(paste(i, "chunk loop:", date()))
    for (j in 1:nsplits2)
    {
      ztest <- (getcor(A = as.matrix(case1[, ithChunk(i, nSNP1, chunk), drop = FALSE]),
                       B = as.matrix(case2[, ithChunk(j, nSNP2, chunk), drop = FALSE]),
                       method = "pearson")
                -
                  getcor(A = as.matrix(control1[, ithChunk(i, nSNP1, chunk), drop = FALSE]),
                         B = as.matrix(control2[, ithChunk(j, nSNP2, chunk), drop = FALSE]),
                         method = "pearson") )  /  sd_tot
      
      index <- which(abs(ztest) >= zthres, arr.ind = TRUE)
      
      
      WriteSnpPairs(Zmatrix = ztest, 
                    indexArr = index,
                    outfile = OUT)
      rm(list = c("ztest", "index"))
      gc()
    }
  }
  
  
  print("epiblaster calculation is over!")
  print(date())
}





##' @description Fast calculation of correlation matrix on CPU
##' (the idea is from \pkg{WGCNA} fast function for pearson correlations)
##' @title Get correlation matrix
##' @export
##' @param A is a matrix or data.frame.
##' @param B is a matrix or data.frame.
##' @param method a character string indicating which correlation coefficient is to be computed.
##' Current version only supports "pearson" correlation.
##' @param ... not used.
##' @return correlation matrix
##' @author Beibei Jiang \email{beibei_jiang@@psych.mpg.de}
##' @examples 
##' set.seed(123)
##' A <- matrix(rnorm(100, mean = 5, sd = 10), ncol = 10)
##' B <- matrix(rnorm(200, mean = 10, sd = 100), ncol = 20)
##' C <- getcor(A, B)
getcor <- function(A = NULL,
                   B = NULL,
                   method = "pearson",
                   ...){
  method <- match.arg(method)
  if(nrow(A) != nrow(B)) stop("Matrices A and B have different row numbers!")
  Abar <- scale(A) * sqrt(1/(nrow(A)-1))
  Bbar <- scale(B) * sqrt(1/(nrow(B)-1))
  return(crossprod(Abar, Bbar))
}


