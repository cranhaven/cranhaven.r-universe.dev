portfolio.mean.size <- function( sort.output ){
  
  if (!is.list(sort.output$portfolios)){
    stop("Please input the portfolio list object from the output of one of the conditional or unconditional sorting functions")
  }
  
  
  
  means <- as.data.frame(matrix(data=NA,nrow=1,ncol=length(sort.output$portfolios[[1]])))
  for (i in 1:ncol(means)) {
    Hold <- lapply(sort.output$portfolios,"[[",i)
    Hold <- lapply(Hold, length)
    
    means[1,i] <- mean(unlist(Hold))
  }
  
  means = round(means,2)
  colnames(means) = as.character( seq(1,length(sort.output$portfolios[[1]]),1) )
  rownames(means) = c("Mean Size")
  
  return(means)
}