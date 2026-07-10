is.increasing.on.x <-
function(fun, x.bound=c(-1,1), y.bound=c(-1,1), step=.01){
  y = seq(y.bound[1], y.bound[2], by=step)
  for(i in 1:length(y)){
    g = function(x) fun(x,y[i])
    if( is.increasing(g, x.bound, step) == FALSE )
       { return(FALSE) } 
    }
  return( TRUE )
}
