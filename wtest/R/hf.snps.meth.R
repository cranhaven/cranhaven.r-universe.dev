W.null.calculate.for.hf.set<-function(w.order, y, n.sample, n.pair, data, data.methylation, set.all){
  y <- y[sample(1:length(y), length(y))]
  w.order <- unlist(w.order)
  set.random <- sample(1:nrow(set.all), n.pair)
  set <-set.all[set.random, ]
  set <- apply(set, 1, list)
  result <- lapply(set, x2.set, data, data.methylation, y)
  result.all <- do.call(rbind, result)
  k.row <- ifelse(w.order==1, 2, 8)
  mean.variance <- array(0, dim = c(k.row, 2))
  df.column <- ifelse(w.order == 1, 3, 4)
  k.min <- min(result.all[, df.column])
  k.max <- max(result.all[, df.column])
  for(i in k.min:k.max){
    mean.variance[i,] = Mean.Variance.calculation.by.K(i, result.all, w.order)
  }
  if(0 %in% mean.variance[,2]){
    mean.variance[which(mean.variance[,2] == 0), 1] = 0
  }
  return(mean.variance)
}

#' Parameter Estimation for W-test Probability Distribution in Gene-methylation Data
#'
#' @description Estimate parameters (\emph{h} and \emph{f}) for \code{W-test}.
#' @param B a numeric number specifying the number of bootstrapping times. Default is 400.
#' @param geno a data frame or matrix containing genotypes in the columns. Genotypes should be coded as (0, 1, 2) or (0, 1). SNP names should be stored as column names.
#' @param meth a data frame or matrix containing methylation data in the columns. Methylation data should be recoded as (0, 1, 2) or (0, 1). Names of CpG sites should be stored as column names.
#' @param y a numeric vector of 0 or 1.
#' @param geno.pos a data frame containing SNP names and positions in two columns.
#' @param meth.pos a data frame containing CpG names and positions in two columns.
#' @param window.size a numeric number specifying the size of genome distance. Interaction of the SNPs and CpG sites located within the size of genome distance will be evaluated exhaustively.
#' @param n.sample a numeric number specifying the number of samples to be included for estimating parameters. Default is the total number of samples.
#' @param n.pair a numeric value, the number of SNP-CpG pairs to use in bootstrapping. Default = min(P, 1000). P is the total number of pairs within the \code{window.size}.
#' @return a set of \emph{h} and \emph{f} values indexed by \emph{k}, estimated automatically. Variable \emph{k} is the number of categorical combinations of a variable pair.
#'
#' @examples
#' data(SNP.pos)
#' data(CpG.pos)
#' data(genotype)
#' data(methylation)
#' data(phenotype2)
#'
#' # Please note that parameter B is recommended to be greater than 400.
#' hf.pair <- hf.snps.meth(B = 80, geno = genotype, meth = methylation, y = phenotype2,
#'                         geno.pos = SNP.pos, meth.pos = CpG.pos, window.size = 1000)
#'
#' @export
#' @author Rui Sun, Maggie Haitian Wang
#' @references Maggie Haitian Wang, Rui Sun, Junfeng Guo, Haoyi Weng, Jack Lee, Inchi Hu, Pak Sham and Benny C.Y. Zee (2016). A fast and powerful W-test for pairwise epistasis testing. Nucleic Acids Research. doi:10.1093/nar/gkw347.
#' @importFrom utils combn
#' @importFrom stats var

hf.snps.meth<-function(B = 400, geno, meth, y, geno.pos, meth.pos, window.size, n.sample = nrow(geno), n.pair = 1000){
  if(is.data.frame(geno))
    geno <- as.matrix(geno)
  if(!all(geno %in% c(0,1,2)))
    stop("all the genotypes in 'data.genotype' must be 0, 1 or 2")
  if(!is.numeric(B))
    B <- as.numeric(B)
  if(any(is.na(y)))
    stop("NA occurs in y")
  if(!all(y %in% c(0,1)))
    stop("all the genotypes in 'y' must be 0 or 1")
  set <- apply(array(2, dim=c(B,1)), 1, list)

  snp.names <- colnames(geno)
  cpg.names <- colnames(meth)
  l1 <- match(snp.names, geno.pos[,1])
  l2 <- match(cpg.names, meth.pos[,1])
  if(any(is.na(l1)))
    stop("missing SNP position exists")
  if(any(is.na(l2)))
    stop("missing CpG position exists")
  geno.pos <- geno.pos[l1,]
  meth.pos <- meth.pos[l2,]

  index.set <- data.frame()
  for(i in 1:nrow(geno.pos)){
    index <- which(abs(geno.pos[,2][i] - meth.pos[,2]) <= window.size)
    if(length(index)){
      index.i <- cbind(i, index)
      index.set <- rbind(index.set, index.i)
    }
  }
  n.pair <- min(nrow(index.set), n.pair)

  result <- lapply(set, W.null.calculate.for.hf.set, y, n.sample, n.pair, geno, meth, index.set)
  result <- apply(simplify2array(result), c(1,2), sum)
  result <- result/B
  h <- result[,1] * 2/result[,2]
  f <- result[,1] * h
  hf.result <- cbind(h,f)
  if(1 %in% is.na(hf.result)){
    k <- which(is.na(hf.result[,1]))
    hf.result[k,1] <- k/(k+1)
    hf.result[k,2] <- k
  }
  k <- c(2:(nrow(hf.result) + 1))
  hf.result <- cbind(k, hf.result)
  return(hf.result)
}
