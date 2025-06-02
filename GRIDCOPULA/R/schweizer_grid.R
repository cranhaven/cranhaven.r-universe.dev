
schweizer.grid <- function(gc){
  int <- function(u,v,gc){
    value <- p.grid(U = as.numeric(u), V = as.numeric(v), gc = gc) -
      as.numeric(u) * as.numeric(v)
    value <- as.numeric(abs(value))
    return(value)
  }
  mg <- gc
  k <- mg$k
  m <- mg$m
  v.breaks <- seq(0, 1, length.out=k+1)
  u.breaks <- seq(0, 1, length.out=m+1)
  value <- 0
  for(i in 1:k) {
    for(j in 1:m) {
      value <- value + integral2(int, u.breaks[j], u.breaks[j+1],
                                 v.breaks[i], v.breaks[i+1],
                                 gc = mg)$Q
    }
  }
  return(12*value)
}

