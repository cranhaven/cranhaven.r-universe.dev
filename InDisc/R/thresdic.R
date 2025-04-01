thresdic <- function(T){

  n <- size(T)[2]

  term1 <- 0

  for (i in 1:n){
    if (T[i] == 0){
      term1 <- term1 + 1
    }
  }

  if (term1 == 0){ term1 <- .5; n <- n + 1}

  term1 <- term1 / n

  b <- qnorm(term1)

}
