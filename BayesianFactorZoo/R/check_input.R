# Check whether the inputs are proper

check_input <- function(f, R , intercept, type, prior){

  k <- dim(f)[2]   # the number of factors
  t_f <- dim(f)[1]   # the number of time periods in factor
  N <- dim(R)[2]   # the number of test assets
  t_asset <- dim(R)[1] # the number of time periods in test assets

  # Check whether the prerequisite conditions satisfy
  # The time periods of factors and assets should be equal
  if (t_f!=t_asset) {
    stop("Error: the time periods of factor should be equal to the time periods of assets.")
  }

  # When the intercept is included, it should be the case that N>k
  if ((intercept==TRUE)&(k>=N)) {
    stop("Error: the number of test assets should be larger (>) than the number of factors becasue of the cross-sectional regression requirement.")
  }

  # When the intercept is not included, it should be the case that N>=k
  if ((intercept==FALSE)&(k>N)) {
    stop("Error: the number of test assets should be larger (>=) than the number of factors because of the cross-sectional regression requirement.")
  }

  # The type should be 'OLS' or 'GLS'
  if ((type!='OLS')&(type!='GLS')) {
    stop("Error: the type should be 'OLS' or 'GLS'.")
  }

  # The prior should be 'flat' or 'normal'
  if ((prior!='Flat')&(prior!='Spike-and-Slab')&(prior!='Normal')) {
    stop("Error: the prior should be 'Flat' or 'Spike-and-Slab' or 'Normal'.")
  }

}


check_input2 <- function(f, R){

  k <- dim(f)[2]   # the number of factors
  t_f <- dim(f)[1]   # the number of time periods in factor
  N <- dim(R)[2]   # the number of test assets
  t_asset <- dim(R)[1] # the number of time periods in test assets

  # Check whether the prerequisite conditions satisfy
  # The time periods of factors and assets should be equal
  if (t_f!=t_asset) {
    stop("Error: the time periods of factor should be equal to the time periods of assets.")
  }

  # The number of test asset should be larger than factors
  if (k>=N) {
    stop("Error: the number of test assets should be larger (>) than the number of factors becasue of the cross-sectional regression requirement.")
  }

}




