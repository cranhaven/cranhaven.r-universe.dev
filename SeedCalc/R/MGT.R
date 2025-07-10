
MGT <- function(time, nger) {

  sum_tn <- time[1]*nger[1]
  sum_n <- 0

     for (i in 2:length(time)) {
     sum_tn <- sum_tn + time[i]*(nger[i] - nger[i-1])
     sum_n <- nger[i]
   }

 result <- sum_tn/sum_n
 return(result)
}
