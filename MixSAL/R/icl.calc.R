icl.calc <-
function(bic,wt){
  constraint <- sum(log(apply(wt,2,max)))
  val <- bic + constraint
  return(val)
}
