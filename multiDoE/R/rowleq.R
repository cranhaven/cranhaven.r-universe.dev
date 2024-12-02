# check if row A is less than or equal to row B

# function res = rowleq( A, B )
# i = 1;
# while (A(i) == B(i)) && (i < length(A))
# i = i + 1;
# end
#
# res = (A(i) <= B(i));

rowleq <- function(A, B) {
  i = 1
  while (A[i] == B[i] & i < length(A)) {
    i = i + 1
  }
  res <- A[i] <= B[i]
  return(res)
}


# può andare bene questa?
rowleq2 <- function(A, B) {
  argmin = which.min(A==B)   # restituisce il primo FALSE (0)
  return(A[argmin] <= B[argmin])
}

# a <- c(1,2,2,5)
# b <- c(1,2,2,4)
