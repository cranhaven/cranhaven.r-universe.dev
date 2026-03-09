ECconversion3=function (x, A, B, method="linear")
{
  if(method=="linear"){
    .value <- A + x * B
    .grad <- array(0, c(length(.value), 2L), list(NULL, c("A", "B")))
    .grad[, "A"] <- 1
    .grad[, "B"] <- x
    attr(.value, "gradient") <- .grad
    .value}
  else if(method=="power"){
    .expr1 <- x^B
    .value <- A * .expr1
    .grad <- array(0, c(length(.value), 2L), list(NULL, c("A", "B")))
    .grad[, "A"] <- .expr1
    .grad[, "B"] <- A * (.expr1 * log(x))
    attr(.value, "gradient") <- .grad
    .value
  }
  else if (method=="exponential"){
    .expr2 <- exp(x * B)
    .value <- A * .expr2
    .grad <- array(0, c(length(.value), 2L), list(NULL, c("A","B")))
    .grad[, "A"] <- .expr2
    .grad[, "B"] <- A * (.expr2 * x)
    attr(.value, "gradient") <- .grad
    .value
  }
  else if (method=="log"){
    .expr1 <- x * B
    .expr2 <- log(.expr1)
    .value <- A * .expr2
    .grad <- array(0, c(length(.value), 2L), list(NULL, c("A", "B")))
    .grad[, "A"] <- .expr2
    .grad[, "B"] <- A * (x/.expr1)
    attr(.value, "gradient") <- .grad
    .value
  }
}
