

# A more sensible version of signif
signifish <- function(x, digits=3)
  ifelse(x < 10^digits, signif(x, digits=digits), round(x))

