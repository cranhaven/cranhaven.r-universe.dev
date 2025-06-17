cummedian <-
function(x) sapply(seq_along(x), function(k, z) median(z[1:k]), z = x)
