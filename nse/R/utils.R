
## Function to check if x is of good dimension
f.error.multivariate = function(x){
  if (!is.vector(x) && dim(x)[2] != 1) {
    stop("this function only handle univariate time-series")
  }
}

## Function to convert nse type into sandwich type
f.type.sandwich = function(type.in) {
  type.in = type.in[1]
  if (type.in == "bartlett") {
    type.out = "Bartlett"
  } else if (type.in == "parzen") {
    type.out = "Parzen"
  } else if (type.in == "tukey") {
    type.out = "Tukey-Hanning"
  } else if (type.in == "qs") {
    type.out = "Quadratic Spectral"
  } else if (type.in == "trunc") {
    type.out = "Truncated" 
  } else {
    stop("Invalid type : must be one of c('bartlett', 'parzen', 'tukey', 'qs', 'trunc')")
  }
  return(type.out)
}
