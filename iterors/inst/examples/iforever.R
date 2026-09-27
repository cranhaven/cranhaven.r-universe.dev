library(iterors)

# return an iterator that returns the specified value forever
iforever <- function(x) {
  nextOr_ <- function(or) x
  iteror(nextOr_)
}

# create an iterator that returns 42 forever
it <- iforever(42)

# call it three times
for (i in 1:3)
  print(nextElem(it))
