tailU.grid <- function(gc){
  k <- gc$k
  m <- gc$m
  
  t <- min(c(m,k))
  
  value <- 2 - (1 - p.grid(U = (t-1)/t * (1 + 1e-3 * 1/t) , 
                           V = (t-1)/t * (1 + 1e-3 * 1/t), gc = gc)
  )/(1 - ((t-1)/t * (1 + 1e-3 * 1/t)))
  return(value)
}

