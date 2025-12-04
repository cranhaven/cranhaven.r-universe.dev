
# Update 2019-10-27: for t-distribution, sd != scale. The 'scale' parameter
#   was incorrectly named 'sd'. Also mean is undefined for df <= 1.
# *t3 group added, which does correctly implement sd.

# The functions 'dt', 'pt', etc in R calculate the (cumulative) probability
#   density for a t-distribution given the t-statistic and the number of
#   degrees of freedom (df). That means you have to calculate the t-stat first,
#   unlike 'dnorm'/'pnorm', where you put in the mean and sd.

# The function 'dt2' and 'pt2' calculate these values with given location, scale,
#   and df, just like 'dnorm'/'dnorm' with the addition of 'df'.

dt2 <- function(x, location, scale, df) {
  checkScaleDf(scale=scale, df=df)
  tstat <- (x - location)/scale
  return(dt(tstat, df)/scale)
}

pt2 <- function(x, location, scale, df, lower.tail=TRUE, log.p=FALSE) {
  checkScaleDf(scale=scale, df=df)
  tstat <- (x - location)/scale
  return(pt(tstat, df, lower.tail=lower.tail, log.p=log.p))
}

qt2 <- function(p, location, scale, df, lower.tail=TRUE, log.p=FALSE) {
  checkScaleDf(scale=scale, df=df)
  tstat <- qt(p, df, lower.tail=lower.tail, log.p=log.p)
  return(tstat * scale + location)
}

rt2 <- function(n, location, scale, df) {
  checkScaleDf(scale=scale, df=df)
  tstat <- rt(n, df)
  return(tstat * scale + location)
}

checkScaleDf <- function(scale, df) {
  if(scale < 0)
    stop("'scale' must be non-negative.", call.=FALSE)
  if(df <= 0)
    stop("'df' must be greater than zero.", call.=FALSE)
  if(df <= 2) {
    warning("sd is only defined for df > 2", call.=FALSE)
  }else{
    sd <- scale * sqrt(df / (df-2))
    warning("sd is ", sd, call.=FALSE)
  }
}
# .............................................................................

# The function 'dt3' etc calculate these values with given mean, sd,
#   and df, just like 'dnorm'/'dnorm' with the addition of 'df'.

dt3 <- function(x, mean, sd, df) {
  scale <- checkSdDf(sd=sd, df=df)
  tstat <- (x - mean)/scale
  return(dt(tstat, df)/scale)
}

pt3 <- function(x, mean, sd, df, lower.tail=TRUE, log.p=FALSE) {
  scale <- checkSdDf(sd=sd, df=df)
  tstat <- (x - mean)/scale
  return(pt(tstat, df, lower.tail=lower.tail, log.p=log.p))
}

qt3 <- function(p, mean, sd, df, lower.tail=TRUE, log.p=FALSE) {
  scale <- checkSdDf(sd=sd, df=df)
  tstat <- qt(p, df, lower.tail=lower.tail, log.p=log.p)
  return(tstat * scale + mean)
}

rt3 <- function(n, mean, sd, df) {
  scale <- checkSdDf(sd=sd, df=df)
  tstat <- rt(n, df)
  return(tstat * scale + mean)
}

checkSdDf <- function(sd, df) {
  if(sd < 0)
    stop("'sd' must be non-negative.", call.=FALSE)
  if(df <= 2)
    stop("sd is only defined for df > 2", call.=FALSE)
  sd / sqrt(df / (df-2)) # calculate and return the 'scale' parameter
}


