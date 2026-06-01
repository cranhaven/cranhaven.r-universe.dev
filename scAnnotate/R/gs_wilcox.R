#' @title gs_wilcox
#'
#' @description rank the significant of each genes for each cell population by their p-value of the Wilcoxon Rank Sum Test
#'
#' @param dat A data frame of cell type label in the first column and a gene expression matrix where each row is a cell and each column is a gene in training data
#' @param cellnames names of each cell population
#'
#' @return A matrix which contain gene names that are sorted by p-value of Wilcoxon Rank Sum Test from smallest to largest for each cell population (column)
#'
#' @importFrom stats wilcox.test
#' @noRd
#'
gs_wilcox=function(dat,cellnames){
  numgn = ncol(dat)-1
  pv <- matrix(NA, numgn, length(cellnames))
  rownames(pv) <- colnames(dat[,-1])
  colnames(pv) <- cellnames
  pv.out=matrix(NA, numgn, length(cellnames))
  colnames(pv.out)=cellnames

  t.list=list()
  w.list=list()

  for(jj in 1:length(cellnames)){
    t.list[[jj]]=(dat[,1]==cellnames[jj])
  }

  for (gn in 1:numgn) {
    pvs=vector()
    for(jj in 1:length(cellnames)){
      w.list[[jj]]=wilcox.test(dat[t.list[[jj]],gn+1],dat[!t.list[[jj]],gn+1],paired = FALSE)
      pvs=c(pvs,w.list[[jj]]$p.value)
    }
    pvs[is.na(pvs)] <- 1 # if p-value output is NaN, replace with 1
    pv[gn, ] <- c(pvs)
  }

  for(jj in 1:length(cellnames)){
    pv.out[,jj]=names(sort(pv[,jj],decreasing = FALSE))
  }

  return(pv.out)
}
