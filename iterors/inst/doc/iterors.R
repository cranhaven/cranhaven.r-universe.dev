## ----ex1----------------------------------------------------------------------
library(iterors)
i1 <- iteror(1:10)
nextOr(i1)
nextOr(i1)

## ----ex2----------------------------------------------------------------------
istate <- iteror(state.x77, by='row')
nextOr(istate)
nextOr(istate)

## ----ex3----------------------------------------------------------------------
ifun <- iteror(function(or) sample(0:9, 4, replace=TRUE))
nextOr(ifun)
nextOr(ifun)

## ----ex5----------------------------------------------------------------------
library(iterors)
itrn <- irnorm(10)
nextOr(itrn)
nextOr(itrn)

## ----ex6----------------------------------------------------------------------
itru <- irunif(10)
nextOr(itru)
nextOr(itru)

## ----ex7----------------------------------------------------------------------
it <- icount(3)
nextOr(it, NULL)
nextOr(it, NULL)
nextOr(it, NULL)
nextOr(it, NULL) #is now ended

## ----ex8----------------------------------------------------------------------
t <- icount(10)
total <- 0
repeat {
  total <- total + nextOr(it, break)
}
total # sum of 1:10

