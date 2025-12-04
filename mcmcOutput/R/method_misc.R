
# misc S3 methods for mcmcOutput objects

head.mcmcOutput <- function(x, n=6L, ...)  {
  head(unclass(x), n=n, ...)
}

tail.mcmcOutput <- function(x, n=6L, ...)  {
  tail(unclass(x), n=n, ...)
}

str.mcmcOutput <- function(object, ...)  {
  str(unclass(object), ...)
}
# .........................................................

names.mcmcOutput <- function(x, ...)  {
  colnames(unclass(x), ...)
}
# .........................................................
