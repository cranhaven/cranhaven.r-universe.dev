#' @title gs_wilcox
#'
#' @description find the parameter of mixture model when normal distribution assumptions are meet
#'
#' @param dat A data frame of cell type label in the first column and a gene expression matrix where each row is a cell and each column is a gene from training data
#' @param cellnames names of each cell population
#'
#' @return A list contains multiple matrix. Each matrix contain the parameter of mixture model for each cell population
#'
#' @noRd
mix_ln_pt=function(dat,cellnames){
  numgenes=ncol(dat)-1

  #create the structure for each matrix
  matrix.list=list()
  for(ii in 1:length(cellnames)){
    matrix.list[[ii]]=matrix(rep(1,3*numgenes),numgenes, 3)
    colnames(matrix.list[[ii]])=c("p.j", "mean", "sd")
  }
  names(matrix.list)=cellnames


  for(jj in 1:length(cellnames)){
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


    # approximate p_j (proportion of zero count in a gene)
    p.j=zj/(zj+nzj)
    matrix.list[[jj]][,1]=p.j

    #parameter mean
    exp.matrix[exp.matrix==0]=NA #to excluded from mean calculation
    if(length(dim(exp.matrix))>1){
      p.mean=colMeans(exp.matrix,na.rm = TRUE)
    }else{
      p.mean=mean(exp.matrix,na.rm = TRUE)
    }


    #parameter sd
    p.sd=sqrt(colSums(t(t(exp.matrix)-p.mean)^2,na.rm = TRUE)/(nzj-1))
    #Notice: if nzj=1, p.sd=NAN, parameter sd will be  =1.


    #adjust the nan and 0 for mean and sd
    #p.mean =nan since p.j=1
    p.mean[is.na(p.mean)]=1
    matrix.list[[jj]][,2]=p.mean

    #p.sd = nan since nzj =1
    p.sd[is.na(p.sd)]=1
    #p.sd=0 since nzj=1
    p.sd[p.sd==0]=1
    matrix.list[[jj]][,3]=p.sd

  }# out for cell type jj
  return(matrix.list)
}
