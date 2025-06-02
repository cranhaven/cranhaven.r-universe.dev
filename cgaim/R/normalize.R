normalize <- function(alpha, type){
  anorm <- norm(matrix(alpha, ncol = 1), type)
  if (!(is.na(anorm) || all(alpha == 0))) alpha <- alpha / anorm
  attr(alpha, "norm") <- anorm
  return(alpha)
}
