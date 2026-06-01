#' @title probability_ln
#'
#' @description Calculate the probability for each cell by mixture model with normal distribution assumptions
#'
#' @param test A data matrix where each row is a cell and each column is a gene from test data
#' @param mix.d mixture model parameter on each gene for each cell population
#'
#' @return A array contains probability for each cell (matrix) on each cell population (column) and each gene (row).
#'
#' @noRd
probability_ln=function(test,mix.d){
  #####MAKE PREDICTION FROM TEST DATA#####
  numgenes=ncol(test)

  px <- array(NA,dim=c(numgenes,length(mix.d),nrow(test)),
              dimnames = list(colnames(test),names(mix.d)))

  #MLE: by density function for normal distribution
  #for each cell type
  for(jj in 1:length(mix.d)){
    #for each gene
    for(nn in 1:numgenes){
      exp.g=as.vector(test[,nn])

      f0=vector()
      f0[exp.g==0]=1
      f0[exp.g!=0]=0

      f.p=vector()
      f.p=(exp((((exp.g-mix.d[[jj]][nn,2])^2)/(-2*(mix.d[[jj]][nn,3]^2)))))/(mix.d[[jj]][nn,3]*sqrt(2*pi))

      px[nn,jj,]=mix.d[[jj]][nn,1]*f0+ (1-mix.d[[jj]][nn,1])*f.p
    }# out for(nn in 1:numgenes)
  }# out for (jj in 1:length(mix.d))

  #normalization
  for(kk in 1:nrow(test)){
    #in case all pro=0 cause rowSums =0, replace it with 1
    if(sum(rowSums(px[,,kk])==0)>0){
      sum.vecotr=rowSums(px[,,kk])
      sum.vecotr[which(sum.vecotr==0)]=1
      px[,,kk]=px[,,kk]/sum.vecotr
    }else{
      px[,,kk]=px[,,kk]/rowSums(px[,,kk])
    }

  }
  return(px)
}

