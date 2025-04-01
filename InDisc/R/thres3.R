thres3 <- function(T){

  n <- size(T)[2]

  b <- matrix(0,2,1)

  term1 <- 0
  term2 <- 0

  for (i in 1:n){
    if (T[i] == 1){
      term1 <- term1 + 1
    }
    if (T[i] ==2){
      term2 <- term2 + 1
    }

  }
  if (term1 == 0){ term1 <- .5; n <- n + 1}
  if (term2 == 0){ term2 <- .5; n <- n + 1}
  if (term1+term2 == n){ term2 = term2 - 0.5; n <- n + 1}

  term1 <- term1 / n
  term2 <- term2 / n

  b[1,1] <- qnorm(term1)
  b[2,1] <- qnorm(term1+term2)

}
