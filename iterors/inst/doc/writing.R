## ----echo=FALSE, results="hide"-----------------------------------------------
library(iterors)

## ----iterable1----------------------------------------------------------------
it <- iteror(list(1:2, 3:4))

## ----iterable2----------------------------------------------------------------
nextOr(it, NULL)
nextOr(it, NULL)
nextOr(it, NULL)

## ----consuming_iter_break-----------------------------------------------------
x <- icount(10)
repeat {
  print(nextOr(x, break))
}

## ----consuming_iter-----------------------------------------------------------
x <- icount(10)
while (!is.null(val <- nextOr(x, NULL))) {
  print(val)
}

## ----bad_iter-----------------------------------------------------------------
bad_values <- list(
  quote(.StopIteration),
  NA,
  NULL,
  list(),
  simpleError("StopIteration"),
  try(stop("StopIteration", call.=FALSE), silent=TRUE),
  "",
  numeric(0))

## ----enum---------------------------------------------------------------------
it <- iteror(bad_values)
while (!is.null(val <- nextOr(it, NULL))) {
  print(val)
}

## ----consuming_sentinel-------------------------------------------------------
end_sentinel <- new.env()
it <- iteror(bad_values)
repeat {
  val <- nextOr(it, end_sentinel)
  if (identical(val, end_sentinel)) break
  print(val)
}

## ----iter1--------------------------------------------------------------------
iforever <- function(x) {
  nextOr_ <- function(or) x
  iteror(nextOr_)
}

## ----runiter1-----------------------------------------------------------------
it <- iforever(42)
nextOr(it, NULL)
nextOr(it, NULL)

## ----runiter1.part2-----------------------------------------------------------
as.numeric(it, n=6)

## ----iter2--------------------------------------------------------------------
irep <- function(x, times) {
  nextOr_ <- function(or) {
    if (times > 0) {
      times <<- times - 1
      x
    } else {
      or
    }
  }

  iteror(nextOr_)
}

## ----runiter2-----------------------------------------------------------------
it <- irep(7, 6)
unlist(as.list(it))

## ----iter3--------------------------------------------------------------------
ivector <- function(x, ...) {
 i <- 1
 it <- idiv(length(x), ...)

 nextOr_ <- function(or) {
   n <- nextOr(it, return(or))
   ix <- seq(i, length=n)
   i <<- i + n
   x[ix]
 }

 iteror(nextOr_)
}

## ----runiter3-----------------------------------------------------------------
it <- ivector(1:25, chunks=3)
as.list(it)

## ----recyle-------------------------------------------------------------------
i_recycle <- function(it) {
  values <- as.list(iteror(it))
  i <- length(values)

  nextOr_ <- function(or) {
    i <<- i + 1
    if (i > length(values)) i <<- 1
    values[[i]]
  }

  iteror(nextOr_)
}

## ----recyleexample------------------------------------------------------------
it <- i_recycle(icount(3))
unlist(as.list(it, n=9))

## ----i_limit------------------------------------------------------------------
i_limit <- function(it, times) {
  it <- iteror(it)

  nextOr_ <- function(or) {
    if (times > 0) {
      times <<- times - 1
      nextOr(it, or)
    } else
      return(or)
  }

  iteror(nextOr_)
}

## ----irep2--------------------------------------------------------------------
irep2 <- function(x, times)
  i_limit(iforever(x), times)

## ----testirep2----------------------------------------------------------------
it <- irep2('foo', 3)
repeat {
  print(nextOr(it, break))
}

## ----testi_recycle------------------------------------------------------------
iterable <- 1:3
n <- 3
it <- i_limit(i_recycle(iterable), n * length(iterable))
as.numeric(it)

## ----rep----------------------------------------------------------------------
rep(iterable, n)

