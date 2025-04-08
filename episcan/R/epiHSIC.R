
##' @description Calculate the significance of epistasis according the definition of HSIC, conduct Z test for HSIC values and 
##' choose variant pairs with the significance below the given threshold for output.
##' @title Calculate epistasis using HSIC with two genotype inputs
##' @export
##' @param geno1 is the first normalized genotype data. It can be a matrix or a dataframe, or a big.matrix object from \pkg{bigmemory}. 
##' The columns contain the information of variables and the rows contain the information of samples. 
##' @param geno2 is the second normalized genotype data. It can be a matrix or a dataframe, or a big.matrix object from \pkg{bigmemory}.
##' The columns contain the information of variables and the rows contain the information of samples. 
##' @param pheno is a vector containing the normalized phenotype information.
##' @param zpthres is the significance threshold for cut-off output of the variant pairs.
##' @param chunk is the number of variants in each chunk.
##' @param outfile is the basename of out filename.
##' @param suffix is the suffix of out filename.
##' @param ... not used
##' @return null
##' @author Beibei Jiang \email{beibei_jiang@@psych.mpg.de} and Benno Pütz \email{Benno Pütz \email{puetz@@psych.mpg.de}
##' @examples
##' # simulate some data
#' set.seed(123)
#' n1 <- 10; n2 <- 15; rows <- 10
#' geno1 <- matrix(sample(0:2, size = n1*rows, replace = TRUE, prob = c(0.5, 0.3, 0.2)), ncol = n1)
#' geno2 <- matrix(sample(0:2, size = n2*rows, replace = TRUE, prob = c(0.4, 0.3, 0.3)), ncol = n2)
#' dimnames(geno1) <- list(row = paste0("IND", 1:nrow(geno1)), col = paste0("rs", 1:ncol(geno1)))
#' dimnames(geno2) <- list(row = paste0("IND", 1:nrow(geno2)), col = paste0("exm", 1:ncol(geno2)))
#' p2 <- rnorm(rows, mean = 5, sd = 10)
#' 
#' # normalized data
#' geno1 <- scale(geno1)
#' geno2 <- scale(geno2)
#' p2 <- as.vector(unlist(scale(p2)))
#' 
##' # two genotypes with quantitative phenotype
#' epiHSIC2genos(geno1 = geno1, 
#' geno2 = geno2, 
#' pheno = p2, 
#' outfile = "episcan_2geno_quant", 
#' suffix = ".txt", 
#' zpthres = 0.9, 
#' chunk = 10)
#' 
#' # take a look at the result
#' res <- read.table("episcan_2geno_quant.txt", 
#' header = TRUE, 
#' stringsAsFactors = FALSE)
#' head(res)
epiHSIC2genos <- function(geno1 = NULL,
                          geno2 = NULL,
                          pheno = NULL,
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
  nsplits1 <- ceiling(nSNP1/chunk)
  
  nSNP2 <- ncol(geno2)
  nsplits2 <- ceiling(nSNP2/chunk)
  
  gc()
  # first loop for all the ones which is interger times than chunk
  for ( i in 1:nsplits1)
  {
    print(paste(i, "chunk loop:", date()))
    for (j in 1:nsplits2)
    {
      HSIC.Zmatrix <- epiHSIC(A = geno1[, ithChunk(i, nSNP1, chunk), drop = FALSE],
                              B = geno2[, ithChunk(j, nSNP2, chunk), drop = FALSE],
                              P = pheno)
      index <- which(abs(HSIC.Zmatrix) >= zthres, arr.ind = TRUE)
      WriteSnpPairs(Zmatrix = HSIC.Zmatrix, indexArr = index,
                    outfile = OUT)
      rm(list = c("HSIC.Zmatrix", "index"))
      gc()
    }
  }
  
  
  # finish
  print("epiHSIC calculation is over!")
  print(date())
}






##' @description Calculate HSIC values 
##' @title Calculate HSIC values
##' @export
##' @param A is one matrix.
##' @param B is one matrix.
##' @param P is "phenoype", a vector.
##' @param ... not used.
##' @return a matrix
##' @author Beibei Jiang \email{beibei_jiang@@psych.mpg.de}
##' @examples  
##' # simulate some data
##' set.seed(123)
#' geno1 <- matrix(sample(0:2, size = 1000, replace = TRUE, prob = c(0.5, 0.3, 0.2)), ncol = 10)
#' geno2 <- matrix(sample(0:2, size = 2000, replace = TRUE, prob = c(0.4, 0.3, 0.3)), ncol = 20)
#' dimnames(geno1) <- list(row = paste0("IND", 1:nrow(geno1)), col = paste0("rs", 1:ncol(geno1)))
#' dimnames(geno2) <- list(row = paste0("IND", 1:nrow(geno2)), col = paste0("exm", 1:ncol(geno2)))
#' 
#' epiHSIC(A = scale(geno1),
#' B = scale(geno2),
#' P = rnorm(100))
epiHSIC <- function(A = NULL,
                    B = NULL,
                    P = NULL,
                    ...)
{
  H.Zmatrix <- crossprod(A, P*B) / sqrt(length(P))
  rownames(H.Zmatrix) <- colnames(A)
  colnames(H.Zmatrix) <- colnames(B)
  return(H.Zmatrix)
}





##' @description Calculate the significance of epistasis according the definition of HSIC, conduct \eqn{Z} test for HSIC values and 
##' choose variant pairs with the significance below the given threshold for output.
##' @title Calculate epistasis using HSIC with one genotype input
##' @export
##' @param geno is the normalized genotype data. It can be a matrix or a dataframe, or a big.matrix object from \pkg{bigmemory}.
##' The columns contain the information of variables and the rows contain the information of samples. 
##' @param pheno is a vector containing the normalized phenotype information.
##' @param zpthres is is the significance threshold to select variant pairs for output. Default is 1e-6.
##' @param chunk is the number of variants in each chunk.
##' @param outfile is the basename of out filename.
##' @param suffix is the suffix of out filename.
##' @param ... not used.
##' @return null
##' @author Beibei Jiang \email{beibei_jiang@@psych.mpg.de}
##' @examples 
##' # simulate some data
#' set.seed(123)
#' geno1 <- matrix(sample(0:2, size = 1000, replace = TRUE, prob = c(0.5, 0.3, 0.2)), ncol = 10)
#' dimnames(geno1) <- list(row = paste0("IND", 1:nrow(geno1)), col = paste0("rs", 1:ncol(geno1)))
#' p2 <- rnorm(100, mean = 5, sd = 10)
#' 
#' # normalized data
#' geno1 <- scale(geno1)
#' p2 <- as.vector(unlist(scale(p2)))
#' 
#' # one genotypes with quantitative phenotype
#' epiHSIC1geno(geno = geno1,
#' pheno = p2, 
#' outfile = "episcan_1geno_quant", 
#' suffix = ".txt", 
#' zpthres = 0.9, 
#' chunk = 10)
#' 
#' # take a look at the result
#' res <- read.table("episcan_1geno_quant.txt", 
#' header = TRUE, 
#' stringsAsFactors = FALSE)
#' head(res)
epiHSIC1geno <- function(geno = NULL,
                         pheno,
                         chunk = 1000,
                         zpthres = 10e-06,
                         outfile = "NONE",
                         suffix = ".txt",
                         ...)
{
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
  
  nSNP <- ncol(geno)
  nsplits <- ceiling(nSNP/chunk)
  gc()
  
  # first loop for all complete chunks (i.e., of size chunk)
  for ( i in 1:nsplits)
  {
    print(paste(i, "chunk loop:", date()))
    for (j in i:nsplits)
    {
      HSIC.Zmatrix <- epiHSIC(A = as.matrix(geno[, ithChunk(i, nSNP, chunk), drop = FALSE]),
                              B = as.matrix(geno[, ithChunk(j, nSNP, chunk), drop = FALSE]),
                              P = pheno)
      index <- which(abs(HSIC.Zmatrix) >= zthres, arr.ind = TRUE)
      
      ifelse(i==j,
             WriteSnpPairs_sym,
             WriteSnpPairs)(Zmatrix = HSIC.Zmatrix, indexArr = index,
                            outfile = OUT)
      rm(list = c("HSIC.Zmatrix", "index"))
      gc()
    }
  }
  

  print("epiHSIC calculation is over!")
  print(date())
}


