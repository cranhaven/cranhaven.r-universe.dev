library(iterors)

# return an iterator that returns subvectors of a sequence
# of a specified length.
# can specify either "chunks" or "chunkSize" arguments
# since that is what the "idiv" function supports.
i_chunk <- function(n, ...) {
 i <- 1
 it <- idiv(n, ...)

 nextOr_ <- function(or) {
   n <- nextOr(it, return(or))
   x <- seq(i, length=n)
   i <<- i + n
   x
 }

 iteror(nextOr_)
}

# create a sequence iterator that returns three subvectors
it <- i_chunk(25, chunks=3)
print(as.list(it))

# create a sequence iterator that returns subvectors
# with a maximum length of 10
it <- i_chunk(25, chunkSize=10)
print(as.list(it))
