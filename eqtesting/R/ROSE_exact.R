#ROSE_exact: Function that determines the exact distance a TOST test-statistic must sit above zero to be significant with a specified power level
#df: Numeric greater than zero
#alpha: Numeric strictly between zero and one
#power_target: Numeric strictly between zero and one

ROSE_exact = function(df, alpha, power_target) {
  #Define the power function
  power = function(epsilon, alpha, df) {
    return(pt(epsilon, ncp = qt(p = 1 - alpha, df = df), df = df) + pt(-epsilon, ncp = qt(p = 1 - alpha, df = df), df = df))
  }
  #Define the difference between the power function and the power target
  power_diff = function(epsilon) {
    return(power(epsilon, alpha, df) - power_target)
  }
  #Obtain the zero of the previous function on the range between zero and an initial value guaranteed to overshoot the power target
  return(uniroot(power_diff, c(0, qt(p = power_target, ncp = qt(p = 1 - alpha, df = df), df = df)))$root)
}
