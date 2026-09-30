normalizeqntval <- 
function(x){
      
      rows <- dim(x)[1]
      cols <- dim(x)[2]
      if (!is.matrix(x)) {
        stop("Matrix expected in normalize.quantiles")
      }
      
      x.order = apply(x,2,function(x) sort.list(x))
      x.sort  = sapply(1:cols,function(i) x[x.order[,i],i])
      mqnts   = rowMeans(x.sort) 
      x.norm  = sapply(1:cols,function(i) mqnts[sort.list(x.order[,i])])
      
      return(list(x=x.norm,mqnts=mqnts))
}
