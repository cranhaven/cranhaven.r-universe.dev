rbinorm <-
function(n, mean1, mean2, sd1, sd2, prop){
  if(prop>1 || prop< 0){stop("proportion should be between 0 and 1")}
  if(n<1){stop("n must be greater than or equal to 1")}
  if(sd1 < 0 || sd2 < 0){stop("standard deviations must be non-negative")}
  z <- rbinom(n,size=1,prob=prop)
  z * rnorm(n, mean=mean1, sd=sd1) + (1-z)*rnorm(n, mean=mean2, sd=sd2)
}
