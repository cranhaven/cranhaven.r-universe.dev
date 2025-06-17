cumskew <-
function(x) sapply(seq_along(x), function(k, z) skewness(z[1:k]), z = x)
