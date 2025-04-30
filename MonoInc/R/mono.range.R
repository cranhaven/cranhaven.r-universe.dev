mono.range <-
function(data, data.r, tol, xr.col, x.col, y.col)
{
  ages <- sort(unique(data[,x.col]))
  mono.new <- foreach(i=1:length(ages), .combine='+') %do%
  {
    x <- subset(data, data[,x.col]==ages[i])
    y <- as.numeric(data.r[data.r[,xr.col]==ages[i],])
    
    count <- sum(x[,3] >= y[2]-tol & x[,3] <= y[3]-tol, na.rm=T)
    return(count)
  }
  i <- NULL; rm(i)
  return(mono.new/nrow(data))
}
