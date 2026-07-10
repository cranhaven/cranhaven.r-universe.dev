is.decreasing <-
function(fun, x.bound=c(-1,1), step=.01){
  x = seq(x.bound[1], x.bound[2], by=step)
  i = 1
  while(fun(x[i]) >= fun(x[i+1])){
    if (i < length(x)-1) {i <- i+1} else( return(TRUE) )
    }
  return(FALSE)
}
