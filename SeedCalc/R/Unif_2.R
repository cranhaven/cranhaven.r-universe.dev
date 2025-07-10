unif_2 <- function(lengths) {

  lengths_total <- lengths[[3]] + lengths[[4]]


  desvPA <- (sd(lengths[[3]]))
  desvPR <- (sd(lengths[[4]]))
  desvCT <- (sd(lengths_total))


  razao <- c()

  for (i in 1:length(lengths[[3]])) {
    if (lengths[[3]][i] > 0) {
      razao <- c(razao, lengths[[4]][i]/lengths[[3]][i])
    }
    else {razao <- c(razao, 0)}
  }



  desvRA <- sd(razao)


  result <- (1000-(0.75*desvPA + 0.5*desvPR + 2.5*desvCT + 50*desvRA))
  return(result)

}
