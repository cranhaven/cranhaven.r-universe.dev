cumkurt <-
function(x) sapply(seq_along(x), function(k, z) kurtosis(z[1:k]), z = x)
