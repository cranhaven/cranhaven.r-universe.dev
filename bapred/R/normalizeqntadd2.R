normalizeqntadd2 <- 
function (x, qnt.scale) {

  ord = sort.list(x)
  xnorm = qnt.scale[sort.list(ord)]
  return(xnorm)

}
