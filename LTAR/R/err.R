err <- function(true_tensor, forecast_tensor){
# determines the difference in Frobenius norms

  modes1 <- true_tensor@modes
  n1 <- modes1[1]
  n2 <- modes1[2]
  n3 <- modes1[3]

  modes2 <- forecast_tensor@modes
  m1 <- modes2[1]
  m2 <- modes2[2]
  m3 <- modes2[3]

  if ((n1 != m1) && (n2 != m2) && (n3 !=m3))
    stop("Tensors are not the same size")

  if (is.na(n3)) {
    errors <- as.data.frame(matrix(NA,nrow=1,ncol=1))
  } else {
  errors <- as.data.frame(matrix(NA,nrow=n2,ncol=1))
  }
  colnames(errors) <- c("Error")
  pos <- 1

  if (is.na(n3)) {
    errors <- fnorm(forecast_tensor-true_tensor)/fnorm(true_tensor)*100
  } else {
    for (i in n2:1){
    errors[pos,] <- fnorm(forecast_tensor[,i,]-true_tensor[,i,])/
      fnorm(true_tensor[,i,])*100
    pos <- pos+1
    }
  }
  return(invisible(errors))
}
