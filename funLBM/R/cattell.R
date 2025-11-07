cattell <-
function(x,thd=0.01){
  sc = abs(diff(x))
  p = length(x)
  d = p-1
  for (j in 1:(p-2)){
    if (prod(sc[(j+1):length(sc)] < thd * max(sc))){
      d = j
      break
    }
  }
  d
}
