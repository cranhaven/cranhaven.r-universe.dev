#' @description Genomic interaction analysis with EPIBLASTER or epistasis-oriented Hilbert–Schmidt Independence Criterion (HSIC).
#' @title Scan pairwise epistasis
#' @param geno1 a data.frame or matrix of the first genotype data. \code{big.matrix} object from \pkg{bigmemory} also works.
#' The columns contain the information of variables and the rows contain the information of samples. 
#' @param geno2 optional. A data.frame or matrix of the second genotype data. \code{big.matrix} object from \pkg{bigmemory} also works.
#' The columns contain the information of variables and the rows contain the information of samples.
#' @param pheno a vector (named or not). If not provided, the value of \code{geno2} will be used if it is a vector. The values is either case-control phenotype (0, 1) or quantitative phenotype.
#' @param phetype character string. Either "case-control" or "quantitative".
#' @param outfile output file name. Default is "episcan". 
#' @param suffix suffix for output file. Default is ".txt". The final result will be stored in \code{outfile}\code{suffix}.
#' @param zpthres is the significance threshold to select variant pairs for output. Default is 1e-6.
#' @param chunksize the number of variants in each chunk.
#' @param scale a logical value to define wheter the input data needs to be normalized. Default is TRUE which means, by default, 
#' all the genotype data will be normalized and if the phetype is "quantitative", the phenotype will also be normalized. 
#' @param ... not used.
#' @references Kam-Thong, T., D. Czamara, K. Tsuda, K. Borgwardt, C. M. Lewis, A. Erhardt-Lehmann, B. Hemmer, et al. 2011. 
#' "EPIBLASTER-Fast Exhaustive Two-Locus Epistasis Detection Strategy Using Graphical Processing Units." Journal Article. 
#' European Journal of Human Genetics 19 (4): 465–71. https://doi.org/10.1038/ejhg.2010.196. 
#' 
#' Kam-Thong, T., B. Pütz, N. Karbalai, B. Müller-Myhsok, and K. Borgwardt. 2011. "Epistasis Detection on Quantitative 
#' Phenotypes by Exhaustive Enumeration Using GPUs." Journal Article. Bioinformatics 27 (13): i214–21. https://doi.org/10.1093/bioinformatics/btr218.
#' @return null
#' @export
#' @author Beibei Jiang \email{beibei_jiang@@psych.mpg.de}
#' 
#' @examples
#' # simulate some data
#' set.seed(123)
#' geno1 <- matrix(sample(0:2, size = 1000, replace = TRUE, prob = c(0.5, 0.3, 0.2)), 
#' ncol = 10)
#' geno2 <- matrix(sample(0:2, size = 2000, replace = TRUE, prob = c(0.4, 0.3, 0.3)), 
#' ncol = 20)
#' dimnames(geno1) <- list(row = paste0("IND", 1:nrow(geno1)), 
#' col = paste0("rs", 1:ncol(geno1)))
#' dimnames(geno2) <- list(row = paste0("IND", 1:nrow(geno2)), 
#' col = paste0("exm", 1:ncol(geno2)))
#' p1 <- c(rep(0, 60), rep(1, 40))
#' p2 <- rnorm(100)
#' 
#' # one genotype with case-control phenotype
#' episcan(geno1 = geno1, 
#' geno2 = NULL, 
#' pheno = p1, 
#' phetype = "case-control",
#' outfile = "episcan_1geno_cc", 
#' suffix = ".txt", 
#' zpthres = 0.9, 
#' chunksize = 10, 
#' scale = TRUE)
#' 
#' # take a look at the result
#' res <- read.table("episcan_1geno_cc.txt", 
#' header = TRUE, 
#' stringsAsFactors = FALSE)
#' head(res)
#' 
#' # two genotypes with quantitative phenotype
#' episcan(geno1 = geno1, 
#' geno2 = geno2, 
#' pheno = p2, 
#' phetype = "quantitative",
#' outfile = "episcan_2geno_quant", 
#' suffix = ".txt", 
#' zpthres = 0.9, 
#' chunksize = 10, 
#' scale = TRUE)
episcan <- function(geno1, 
                    geno2 = NULL, 
                    pheno = NULL, 
                    phetype = c("case-control", "quantitative"),
                    outfile = "episcan",
                    suffix = ".txt",
                    zpthres = 1e-6, 
                    chunksize = 1000, 
                    scale = TRUE,
                    ...){
  if (is.null(geno2) && is.null(pheno) ){
    stop("neither geno2 nor pheno provided")
  }
  if (is.null(pheno)){
    if(is.vector(geno2)){
      pheno <- geno2
      geno2 <- NULL
    } else {
      stop("There is no given phenotype data!")
    }
  } else{
    if(!is.vector(pheno)){
      pheno <- as.vector(unlist(pheno))
      if(length(pheno) != nrow(geno1)){
        stop("There is something wrong in your phenotype data. Please check your input phenotype!")
      }
      
    }
  }
  phetype <- match.arg(phetype)
  
  if(is.null(geno1)){
    stop("There is no given genotype data!")
  }
  chunksize <- min(chunksize, ncol(geno1))
  
  if(length(phetype) != 1){
    stop("Please specify the type of phenotype (case-control or quantitative)! 
            EPIBLASTER will be applied to case-control data and epiHSIC will be applied to quantitative phenotype.")
  }
  cat(paste0("p-value threshold of Z test for output: ", zpthres), "\n")
  cat(paste0("set chunksize: ", chunksize), "\n")
  
  if(scale){
    geno1 <- scale(geno1)
    if(phetype == "quantitative"){
      pheno <- as.vector(unlist(scale(pheno)))
    }
    if(!is.null(geno2)){
      geno2 <- scale(geno2)
    }
  }
  gc()
  
  print("episcan starts:")
  print(date())
  if(phetype == "case-control"){
    if(is.null(geno2)){
      chunksize <- checkchunksize(chunksize, ncol(geno1))
      epiblaster1geno(geno = geno1,
                      pheno = pheno,
                      chunk = chunksize,
                      zpthres = zpthres,
                      outfile = outfile,
                      suffix = suffix)
    }else{
      chunksize <- checkchunksize(chunksize, ncol(geno1), ncol(geno2))
      epiblaster2genos(geno1 = geno1,
                       geno2 = geno2,
                       pheno = pheno,
                       chunk = chunksize,
                       zpthres = zpthres,
                       outfile = outfile,
                       suffix = suffix)
    }
  } else {
    if(is.null(geno2)){
      chunksize <- checkchunksize(chunksize, ncol(geno1))
      epiHSIC1geno(geno = geno1,
                   pheno = pheno,
                   chunk = chunksize,
                   zpthres = zpthres,
                   outfile = outfile,
                   suffix = suffix)
    }else{
      chunksize <- checkchunksize(chunksize, ncol(geno1), ncol(geno2))
      epiHSIC2genos(geno1 = geno1,
                    geno2 = geno2,
                    pheno = pheno,
                    chunk = chunksize,
                    zpthres = zpthres,
                    outfile = outfile,
                    suffix = suffix)
    }
  }
}






#' @title Check chunk size
#' @description Check the chunk size whether it is over the given number of variables(vaiants) in genotype data. 
#' If yes, reset the chunk size equal to the number of variables(vaiants).
#' @param c  an integer indicating the set chunk size.
#' @param m  an integer indicating the number of variables(vaiants) in \code{geno1} if there is only one genotype input.
#' @param n  an integer indicating the number of variables(vaiants) in \code{geno2} if there are two genotype inputs.
#' The default is NULL.
#' @param ... not used.
#'
#' @return an integer indicating the chunk size
#' @export
#'
#' @examples
#' set.seed(123)
#' geno1 <- matrix(sample(0:2, size = 1000, replace = TRUE, prob = c(0.5, 0.3, 0.2)), 
#' ncol = 10)
#' geno2 <- matrix(sample(0:2, size = 2000, replace = TRUE, prob = c(0.4, 0.3, 0.3)), 
#' ncol = 20)
#' 
#' # if chunk size is smaller, there is no problem
#' chunksize <- 10
#' checkchunksize(chunksize, ncol(geno1))
#' 
#' # if chunk size is bigger than the number of columns in genotype input, 
#' # set chunk size equal to the number of columns in genotype input
#' chuksize <- 12
#' checkchunksize(chunksize, ncol(geno1))
#' 
#' # if chunk size is bigger than the number of columns of geno1 and geno2, 
#' # set chunk size equal to the minima nunumber of columns of geno1 and geno2
#' chunksize <- 50
#' checkchunksize(chunksize, ncol(geno1), ncol(geno2))
checkchunksize <- function(c, m, n = NULL,
                      ...){
  if(is.null(n)){
    if(c > m){
      warning(paste0("chunksize is bigger than the total number of variables (variants) in geno1. Set chunksize equal to ", m))
      c <- m
    }
  }else{
    if(c > min(m, n)){
      warning(paste0("chunksize is bigger than the minimal number of variables (variants) in geno1 and geno2. Set chunksize equal to ",  min(m, n)))
      c <-  min(m, n)
    }
  }
  return(c)
}