portfolio.frequency <- function(  sort.output,rank  ) {
  
  if (!is.list(sort.output$portfolios)){
    stop("Please input the portfolio list object from the sort.output of one of the conditional or unconditional sorting functions")
  }
  
  
  freq <- as.data.frame(matrix(data=NA,nrow=3,ncol=length(sort.output$portfolios[[1]])))
  
  for (i in 1:ncol(freq)){
    ports <- unlist(lapply(sort.output$portfolios,"[[",i))
    Hold <- rev(sort(table(ports)))[rank]
    idx <- as.numeric(names(Hold))
    occ <- as.numeric(Hold)
    freq[1,i] <-  as.character(  (sort.output$tickers[idx,1])  )
    freq[2,i] <- occ
    freq[3,i] <- round(occ/length(sort.output$portfolios), 3)
  }
  
  
  colnames(freq) = as.character( seq(1,length(sort.output$portfolios[[1]]),1) )
  rownames(freq) = c("Ticker","Count","Percentage")
  return(freq)
  
}




