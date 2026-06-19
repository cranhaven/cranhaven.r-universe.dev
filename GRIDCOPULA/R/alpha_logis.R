alpha_logis <- function(x){
  value <- 1/(1+exp(-(x-1)))
  return(value)
}

#curve(alpha_logis(x), from = 0, to = 5,
#      ylim = c(0,1))
