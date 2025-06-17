sv <-
function(data, lower1, upper1, lower2, upper2) {
  F <- prepare_data(data, lower1, upper1, lower2, upper2)
  S <- starting_values(F)
  return(list(mu1=S[1], mu2=S[2], var1=exp(S[3]), cor=S[4], var2=exp(S[5])))
}




starting_values <-
function(F) {
    # Generate starting values for the first and second moments using
    # complete case analysis.
    #
    # Result:
    #  The return value is a vector with the elements (mean of Y1, mean of
    #  Y2, variance of Y1, covariance between Y1 and Y2, variance of
    #  Y2).

    ii = intersect(F$Obs[[1]], F$Obs[[2]])

    Y1 <- F$Y1[ii,1]
    Y2 <- F$Y2[ii,1]

    m1 <- mean(Y1)
    m2 <- mean(Y2)
    v1 <- log(var(Y1))
    v2 <- log(var(Y2))
    c <- cov(Y1, Y2)
    
    sv <- c(m1, m2, v1, c, v2)
    return(sv)
}


