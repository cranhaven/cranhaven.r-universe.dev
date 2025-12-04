f.kernel_addon = function(type,
                          m = 200,
                          steep = FALSE,
                          y = NULL) {
  if (steep == TRUE) {
    if ((type == "parzen") || (type == "triangular") || (type == "qs")) {
      m = round(length(y) / 2) - 1
      alpha =  stats::ar.ols(x = y,
                             order.max = 1,
                             demean = FALSE)$ar
    } else {
      stop(
        "Invalid type for steep specification: steep may be used with 'parzen','triangular',and 'qs'"
      )
    }
  }
  k = kernel("daniell", m = m)
  x = (0:m) / m
  if ((type == "daniell") || (type == "modified.daniell")) {
    k = kernel("daniell", m = m)
    return(k)
  }
  if (type == "tukey-hanning") {
    k_temp = (1 + (cos(pi * (x)))) / 2
    k_temp[x > 1] = 0
  } else if (type == "parzen") {
    k_temp = x
    k_temp[x >= 0 &
             x <= 0.5] = 1 - 6 * x[x >= 0 &
                                     x <= 0.5] ^ 2 + 6 * x[x >= 0 & x <= 0.5] ^ 3
    k_temp[x > 0.5 & x <= 1] = 2 * (1 - x[x > 0.5 & x <= 1]) ^ 3
    k_temp[x > 1] = 0
    g = 6
  } else if (type == "triweight") {
    k_temp = x
    k_temp = 35 / 32 * (1 - k_temp[x >= 0 & x <= 1] ^ 2) ^ 3
    k_temp[x > 1] = 0
  } else if (type == "bartlett-priestley") {
    k_temp = x
    k_temp = 3 / (pi ^ 2 * x[x >= 0 &
                               x <= 1] ^ 2) * (sin(pi * x[x >= 0 &
                                                            x <= 1]) / (pi * x[x >= 0 & x <= 1]) - cos(pi * x[x >= 0 &
                                                                                                                x <= 1]))
    k_temp[1] = 1
    k_temp[x > 1] = 0
  } else if (type == "triangular") {
    k_temp = x
    k_temp = 1 - x[x >= 0 & x <= 1]
    k_temp[x > 1] = 0
    g = 1
  } else if (type == "qs") {
    k_temp = x
    k_temp = 25 / (12 * pi ^ 2 * x[x >= 0 &
                                     x <= 1] ^ 2) * ((sin((6 * pi * x[x >= 0 &
                                                                        x <= 1] / 5)) / (6 * pi * x[x >= 0 &
                                                                                                      x <= 1] / 5)) - cos((6 * pi * x[x >= 0 & x <= 1] / 5)))
    k_temp[x > 1] = 0
    k_temp[1] = 1
    g = 18 * pi ^ 2 / 125
  }
  
  if (isTRUE(steep)) {
    if (type == "triangular") {
      p = length(y) ^ (2 / 3) * ((1 - alpha ^ 2) ^ 2 / (4 * alpha ^ 2)) ^ (1 /
                                                                             3)
    } else{
      p = length(y) ^ (8 / 5) * g ^ -1 * (((sqrt(2 * pi)) * (1 - alpha) ^ 4) /
                                            (16 * alpha ^ 2)) ^ (2 / 5)
    }
    k_temp = k_temp ^ as.vector(p)
  }
  k_temp = k_temp / sum(k_temp)
  k$coef = k_temp
  return(k)
}

f.stuetzle = function(spectrumsmooth, spectrumh0) {
  len = length(spectrumsmooth$freq)
  SES = 1 / len * sum((spectrumsmooth$spec - spectrumh0$spec) ^ 2)
  return(SES)
}