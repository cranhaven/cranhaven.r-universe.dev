portfolio.turnover <- function( sort.output ){
  
  if (!is.list(sort.output$portfolios)){
    
    stop("Please input the portfolio list object from the output of one of the conditional or unconditional sorting functions")
  }
  
  Turnover.Out <- list()
  dims = length(sort.output$portfolios[[1]])
  len = length(sort.output$portfolios)
  TO = matrix( ncol = dims ,nrow = len )
  TO <- xts(x = TO, order.by = index(sort.output$returns))
  TO = as.xts(TO)
  
  for (i in 1:dims){
    for (t in 2:len){
      portfolio.0 <- sort.output$portfolios[[t-1]][[i]]
      portfolio.1 <- sort.output$portfolios[[t]][[i]]
      Hold.intersect <- length(intersect(portfolio.1,portfolio.0))
      Hold.length <- length(portfolio.1)
      if (Hold.intersect==0|Hold.length==0){
        TO[t,i] <- 1
      } else {
        TO[t,i] <- 1- (Hold.intersect/Hold.length)
      }
    }
  }
  
  TO [1,] <- 1
  colnames(TO) = as.character( seq(1,length(sort.output$portfolios[[1]]),1) )
  Turnover.Out$Turnover = TO
  # For some reason - this is not appeding to the list initialized above???
  # The output is just an xts matrix with the actual turnovers over time... any ideas?
  Turnover.Out$`Mean Turnover` = t(as.data.frame( colMeans(TO) ))
  
  colnames(Turnover.Out$`Mean Turnover`) = as.character( seq(1,length(sort.output$portfolios[[1]]),1) )
  rownames(Turnover.Out$`Mean Turnover`) = "Mean Turnover"
  return(Turnover.Out)
}



