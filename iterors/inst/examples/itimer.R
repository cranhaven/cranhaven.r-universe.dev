library(iterors)

# Returns an iterator that limits another iterator based on time
itimer <- function(it, time) {
  it <- iteror(it)
  start <- proc.time()[[3]]

  nextOr <- function(or) {
    current <- proc.time()[[3]]
    if (current - start >= time)
      or
    else nextOr(it, or)
  }

  iteror(nextOr_)
}

# Create a iterator that counts for one second
it <- itimer(icount(Inf), 1)
repeat {
  print(nextOr(it, break))
}
cat('timer expired\n')
