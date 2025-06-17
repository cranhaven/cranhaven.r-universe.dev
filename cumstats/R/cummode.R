cummode <-
function(x) sapply(seq_along(x), function(k, z) Mode(z[1:k])$Values, z = x)
