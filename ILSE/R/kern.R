## Kernel function ##
kern <- function(u, type='epk'){
  if(!is.element(type, c('epk', 'biweight', 'triangle',
                         'gaussian', 'triweight', 'tricube', 'cosine', 'uniform')))
    stop("type must belong to 'epk', 'biweight', 'triangle',
         'gaussian', 'triweight', 'tricube', 'cosine', 'uniform'!")
  if (type=="epk") f = 0.75*(1-u^2)*(u<=1& u>=-1)
  else if (type=='biweight') f = (15/16)*(1 - u^2)^2*(u<=1& u>=-1 )
  else if (type=='triangle') f = (1-abs(u))*(u<=1& u>=-1)
  else if (type=='gaussian') f = dnorm(u)
  else if (type=='triweight') f = 35/32*(1-u^2)^3*(u<=1& u>=-1)
  else if (type=='tricube') f = 70/81*(1-abs(u)^3)^3*(u<=1& u>=-1)
  else if (type=='cosine') f = pi/4*cos(pi*u/2)
  else if (type=='uniform') f = 1/2*(u<=1& u>=-1 )
  return(f)
}
