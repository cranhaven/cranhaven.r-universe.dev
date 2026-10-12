aitkens <-
function(lval,i,eps){
  if(i > 2){
    a <- (lval[i] - lval[i-1])/(lval[i-1]-lval[i-2]); #print(a)
    inf.l <- lval[i-1] + (1/(1-a))*(lval[i]-lval[i-1]);
    return(inf.l)
  } else {
    inf.l <- lval[i] + eps + 1
    return(inf.l)
  }
}
