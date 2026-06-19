
gini.grid <- function(gc){
  int1 <- function(u, gc){
    return(p.grid(u,1-u,gc = gc))
  }
  int2 <- function(u,gc){
    return(u - p.grid(u,u,gc))
  }
  gini <- 4*(integrate(int1,0,1, gc = gc)$value -
               integrate(int2,0,1, gc = gc)$value)
  return(gini)
}
