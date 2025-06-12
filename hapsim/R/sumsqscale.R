"sumsqscale" <-
function(mat){
   s <- sign(mat)
   sq <- mat^2
   ssq <- apply(sq, 2, sum)
   for (i in 1:ncol(mat)) sq[,i] <- sq[,i] / ssq[i]
   return (s*sqrt(sq))
}

