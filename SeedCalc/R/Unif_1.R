unif_1 <- function(lengths) {

  lengths_total <- lengths[[3]] + lengths[[4]]

  lengths_corr <- c()

  for (i in 1:length(lengths_total)) {
    if (lengths_total[i] > 0){
      lengths_corr <- c(lengths_corr, lengths_total[i])
    }
  }

  ndeads <- length(lengths_total) - length(lengths_corr)
  desv <- 0


  for (i in 1:length(lengths_corr)) {
    desv <- desv + abs((lengths_corr[i]-mean(lengths_total)))
  }

  penalty <- ndeads*(50/length(lengths_total))

  result <- (1 - desv/(length(lengths_corr)*mean(lengths_corr)))*1000 - penalty
  return(result)
}
