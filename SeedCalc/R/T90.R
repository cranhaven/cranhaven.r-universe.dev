T90 <- function(time, nger) {
 N <- max(nger)

 ni <- max(nger)
 ti <- max(time)

 nj <- min(nger)
 tj <- min(time)


  for (i in length(time):1) {
    if (ni > ((N)/(100/90))) {
      ni = nger[i]
      ti = time[i]
      }
  }

 for (j in 1:length(time)) {
   if (nj < ((N)/(100/90))) {
     nj = nger[j]
     tj = time[j]
   }
 }
 result <- ti + (((N)/(100/90) - ni)*(tj - ti))/(nj-ni)
 return(result)
}
