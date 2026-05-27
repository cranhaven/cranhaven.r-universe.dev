Local.Mode <-
function(x,y){
 yy <- c(0, y, 0)
 l <- length(yy)
 idx <- (yy[2:(l - 1)] > yy[1:(l - 2)]) & (yy[2:(l - 1)] > yy[3:l])
 sort(x[idx])
 }
