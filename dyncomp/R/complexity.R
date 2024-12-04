# Dynamic Complexity Functions - Tim Kaiser, August 2018


require(zoo) # for rollapply

complexity <- function(x, scaleMin = min(x, na.rm = T), scaleMax = max(x, na.rm = T), width = 7, measure = "complexity", rescale = FALSE) {



if(!is.numeric(x)){stop("Please provide a numeric vector.")}

# RMSSD function taken from the psych library ----
  rmssd <- function (x){
    result <- sum(diff(x, lag = 1, na.rm = T)^2, na.rm = T) / (sum(!is.na(x)) - 1)
    return(sqrt(result))
  }

adjust.to.scale <- function (x)
  {
  from <- range(x, na.rm = TRUE, finite = TRUE)
    (x - from[1])/diff(from) * diff(c(scaleMin, scaleMax)) + c(scaleMin, scaleMax)[1]
  }

# RMSSD of the segment, standardized by the maximum possible RMSSD ----
  fluctDegree <- function(x, scaleMin, scaleMax) {

    seq <- length(x)
    fmax <-scaleMax-scaleMin # Maximum possible MSSD.
    fobs <- rmssd(x)
    f <- fobs / fmax
    return(f)
    }

# Deviation from a hypothetical uniform distribution
  distDegree <- function(x, scaleMin, scaleMax) {
    uniform <- seq(from = scaleMin, to = scaleMax, length.out = length(x)) # Make a hypothetical distribution for the window
    empirical <- sort(x) # Rank-order observed values
    uni.diff <- diff(uniform) # Calculate differences in hypothetical distribution
    emp.diff <- diff(empirical) # Calculate differences in observed distribution
    deviation <- uni.diff - emp.diff# Calculate deviation
    dev.h <- deviation * (sign(deviation )+1)/2 # Apply a Heaviside step function to eliminate negatives
    div.diff <- dev.h / uni.diff
    #print(normed) # debug
    D <- 1 - mean(div.diff) # If there were no deviations from the uniform distribution, this would be 0
    return(D)}

  fluctuation <- rollapply(x, width = width, FUN = fluctDegree, scaleMin = scaleMin, scaleMax = scaleMax, partial = F, fill = NA, align = "right")
  distribution <- rollapply(x, width = width, FUN = distDegree, scaleMin = scaleMin, scaleMax = scaleMax, partial = F, fill = NA, align = "right")
  complexity <- fluctuation * distribution

  if(rescale){
    if(measure == "distribution"){return(adjust.to.scale(distribution))}
    if(measure == "fluctuation"){return(adjust.to.scale(fluctuation))}
    if(measure == "complexity"){return(adjust.to.scale(complexity))}
  }

  if(measure == "distribution"){return(distribution)}
  if(measure == "fluctuation"){return(fluctuation)}
  if(measure == "complexity"){return(complexity)}
}
