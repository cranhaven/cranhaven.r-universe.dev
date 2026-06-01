#' @title qjm
#'
#' @description Calculate the probability for each cell by mixture model with nonparametric depth function
#'
#' @param dat A data frame of cell type label in the first column and a gene expression matrix where each row is a cell and each column is a gene from training data
#' @param test A data matrix where each row is a cell and each column is a gene from test data
#'
#' @return A array contains probability for each cell (matrix) on each cell population (column) and each gene (row).
#'
#' @noRd
#'
qjm <- function(dat, test) {
  cellnames <- names(table(dat[,1]))
  totalgn <- ncol(dat)-1
  typecount <- length(cellnames)
  qjs <- array(NA, dim=c(totalgn, typecount,nrow(test)),dimnames = list(colnames(dat)[-1],cellnames))

  for(jj in 1:typecount){
    #1. estimated from training dataset:
    #matrix only contain the gene expression variables for sample which lable ==cellnames[jj]
    exp.matrix=dat[dat[,1]==cellnames[jj],-1]

    #calculate the number of genes expression ==0 and != 0 for each gene
    if(length(dim(exp.matrix))>1){
      #calculate the number of genes expression ==0 and != 0 for each gene
      zj=colSums(exp.matrix==0)
      nzj=colSums(exp.matrix!=0)
    }else{
      zj=sum(exp.matrix==0)
      nzj=sum(exp.matrix!=0)
    }

    # approximate p_j (proportion of zero count) for each gene
    p.j=zj/(zj+nzj)

    #extract the nonzero genes expression matrix
    exp.matrix[exp.matrix==0]=NA

    #2.estimated from testing datasets:
    for(nn in 1:totalgn){
      exp.g=as.vector(test[,nn])

      f0=vector()
      f0[exp.g==0]=1
      f0[exp.g!=0]=0

      if(totalgn>1){
        Fplus <- depthf(exp.matrix[,nn],exp.g,nzj[nn])
      }else{
        Fplus <- depthf(exp.matrix,exp.g,nzj)
      }

      qjs[nn,jj,] <- p.j[nn]*f0 + (1-p.j[nn])*Fplus

    }# out for(nn in 1:totalgn)

  }# out for(jj in 1:typecount)

  return(qjs)
}
