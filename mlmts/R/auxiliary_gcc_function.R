


auxiliary_gcc_function_1 <- function(X, Y, desired_lag = 1){

  X <- as.vector(scale(X))
  Y <- as.vector(scale(Y))


  # Computing Rxx


  acfx <- stats::acf(X, lag.max = desired_lag, plot = F)$acf[,,1][-1]
  Rxx <- matrix(0, nrow = desired_lag + 1, ncol = desired_lag + 1)

  for (i in 2 : (desired_lag + 1)) {
    Rxx[(i-1), i : (desired_lag + 1)] <- acfx[1 : (desired_lag + 2 - i)]
  }

  for (i in 1 : (desired_lag + 1)) {
    for (j in 1 : (i - 1)) {
      Rxx[i, j] <- Rxx[j, i]
    }
  }

  diag(Rxx) <- 1


  # Computing Ryy


  acfy <- stats::acf(Y, lag.max = desired_lag, plot = F)$acf[,,1][-1]
  Ryy <- matrix(0, nrow = desired_lag + 1, ncol = desired_lag + 1)

  for (i in 2 : (desired_lag + 1)) {
    Ryy[(i-1), i : (desired_lag + 1)] <- acfy[1 : (desired_lag + 2 - i)]
  }

  for (i in 1 : (desired_lag + 1)) {
    for (j in 1 : (i - 1)) {
      Ryy[i, j] <- Ryy[j, i]
    }
  }

  diag(Ryy) <- 1


  # Computing Cxy

  ccfxy <- stats::ccf(X, Y, lag.max = desired_lag, plot = F)$acf[,,1]
  Cxy <- matrix(0, nrow = desired_lag + 1, ncol = desired_lag + 1)

  for (i in 1 : (desired_lag + 1)) {

    Cxy[i,] <- rev(ccfxy[i : (desired_lag + i)])
  }


  # Computing the generalize cross correlation based distance function

  detn <- det(Ryy - Cxy %*% solve(Rxx)  %*% t(Cxy))
  detd <- det(Ryy)
  distance <- 1 - (detn^(1/(desired_lag + 1)))/(detd^(1/(desired_lag + 1)))

  if (is.na(distance)){
    0
  } else {
    distance
  }

}

# Generalized cross correlation based distance function between multivariate time series. Extracting features
# Input parameters:
# X: list of mts
# k: desired lag


auxiliary_gcc_function_2 <- function(X, lag_max = 1){

  c <- ncol(X)

  features <- numeric(c*(c-1)/2)

  count <- 1
  for(k in 1 : lag_max) {
   for (i in 1 : c) {
    for (j in ((i + 1) : c)){

      if (i < c) {


      features[count] <- auxiliary_gcc_function_1(X[,i],
                                                  X[,j], desired_lag = k)
      count <- count + 1

       }

      }


   }
  }

  as.vector(features)

}







