
Beta_LowerBound <- function() {
  return(-0.9999)
}

Beta_UpperBound <- function() {
  return(0.9999)
}

Alpha_UpperBound <- function() {
  return(10)
}

Alpha_LowerBound <- function() {
  return(-10)
}

Lower_Fun <- function() {

    LB = c(beta = Beta_LowerBound(),
      alpha = Alpha_LowerBound(),
      phi = Beta_LowerBound(),
      gamma = Alpha_LowerBound())

  return(LB)

}

Upper_Fun <- function() {

    UB = c(beta = Beta_UpperBound(),
           alpha = Alpha_UpperBound(),
           phi = Beta_UpperBound(),
           gamma = Alpha_UpperBound())

  return(UB)

}

