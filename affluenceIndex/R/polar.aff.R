polar.aff <-
function(x, weight){
  n <- length(x)
  if(is.null(weight)) weight <- rep(1, n)
  
  ind <- ifelse(x <= weighted.median(x, weight), 1 ,0)
  TT <- 0.5 - sum(x*weight*ind)/sum(x*weight)
  Pw <- 2*(2*TT - gini.w(x, weight))*weighted.mean(x, weight)/weighted.median(x, weight) 
 return(list(Pw = Pw, TT = TT))
}
