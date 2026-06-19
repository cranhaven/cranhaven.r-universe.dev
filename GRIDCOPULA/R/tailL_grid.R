
tailL.grid <- function(gc){
  k <- gc$k
  m <- gc$m
  
  t <- min(c(m,k))
  value <- p.grid(U = 1/t * (1 - 1e-3),
                  V = 1/t * (1 - 1e-3),
                  gc = gc)/(1/t * (1 - 1e-3))
  return(value)
}


