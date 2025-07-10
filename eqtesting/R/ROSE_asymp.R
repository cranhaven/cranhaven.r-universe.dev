#ROSE_asymp: Function that determines the asymptotically approximate distance a TOST test-statistic must sit above zero to be significant with a specified power level
#df: Numeric greater than zero
#alpha: Numeric strictly between zero and one
#power_target: Numeric strictly between zero and one

ROSE_asymp = function(alpha, power_target) {
  #Define the power function
  power = function(epsilon, alpha) {
    return(pnorm(epsilon, mean = qnorm(p = 1 - alpha)) + pnorm(-epsilon, mean = qnorm(p = 1 - alpha)))
  }
  #Define the difference between the power function and the power target
  power_diff = function(epsilon) {
    return(power(epsilon, alpha) - power_target)
  }
  #Obtain the zero of the previous function on the range between zero and an initial value guaranteed to overshoot the power target
  return(uniroot(power_diff, c(0, qnorm(p = power_target, mean = qnorm(p = 1 - alpha))))$root)
}
