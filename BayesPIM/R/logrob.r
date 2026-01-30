logrob = function(x,tol){
  i = x==0
  x[i] = x[i]+tol
  log(x)
}