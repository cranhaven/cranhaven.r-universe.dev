cumvar <-
function(x) sapply(seq_along(x), function(k, z) var(z[1:k]), z = x)
