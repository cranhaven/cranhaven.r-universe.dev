#' @title gs_t
#'
#' @description rank the significant of each genes for each cell population by their t-statistic of the t-test (unequal variance assumption)
#'
#' @param dat A data frame of cell type label in the first column and a gene expression matrix where each row is a cell and each column is a gene in training data
#' @param cellnames names of each cell population
#'
#' @return A matrix which contain gene names that are sorted by p-value of Wilcoxon Rank Sum Test from smallest to largest for each cell population (column)
#' @noRd
#'
gs_t=function(dat,cellnames){
  numgn = ncol(dat)-1
  pv <- matrix(NA, numgn, length(cellnames))
  rownames(pv) <- colnames(dat[,-1])
  colnames(pv) <- cellnames
  pv.out=matrix(NA, numgn, length(cellnames))
  colnames(pv.out)=cellnames

  for(jj in 1:length(cellnames)){
    #split two matrix: selected cell type verses others
    select.matrix=dat[dat[,1]==cellnames[jj],-1]
    other.matrix=dat[dat[,1]!=cellnames[jj],-1]

    mean.x1=colMeans(select.matrix)
    mean.x2=colMeans(other.matrix)

    n1=nrow(select.matrix)
    n2=nrow(other.matrix)

    var1=colSums(t(t(select.matrix)-mean.x1)^2)/(n1-1)
    var2=colSums(t(t(other.matrix)-mean.x2)^2)/(n2-1)

    t.stat=(mean.x1-mean.x2)/sqrt(var1/n1+var2/n2)

    t.stat[is.na(t.stat)] <- 0 # if both var1 and var2=0, the t-stat = NaN, replace it with 0
    pv[,jj]=abs(t.stat)

    pv.out[,jj]=names(sort(pv[,jj],decreasing = TRUE))
  }

  return(pv.out)
}
