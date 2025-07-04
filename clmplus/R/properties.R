rtt.input.env <- new.env()

# Input checks ----

rtt.input.env$properties.cpt <- function(cumulative.payments.triangle){
  
  if(sum(cumulative.payments.triangle<0, na.rm = T) !=0 ){
    stop('Please provide an input triangle without negative values')
  }
  
  temp = ChainLadder::cum2incr(cumulative.payments.triangle)
  
  if(sum(temp<0, na.rm = T) !=0 ){
    stop('Please provide an input triangle of cumulative payments.\n Payments recoveries are not allowed in our model framework.')
  }
  
}

