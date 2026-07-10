is.decreasing.on.y <-
function(fun, x.bound=c(-1,1), y.bound=c(-1,1), step=.01){
  x = seq(x.bound[1], x.bound[2], by=step)
  for(i in 1:length(x)){
    g = function(y) fun(x[i],y)
    if( is.decreasing(g, y.bound, step) == FALSE )
       { return(FALSE) } 
    }
  return( TRUE )
}
