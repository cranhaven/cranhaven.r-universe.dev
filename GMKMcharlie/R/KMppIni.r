

KMppIni <- function(
  X,
  K,
  firstSelection = 1L,
  minkP = 2,
  stochastic = FALSE,
  seed = 123,
  maxCore = 7L,
  verbose = TRUE)
{
  KMppIniCpp(
    X,
    firstSelection,
    K,
    minkP,
    stochastic,
    seed,
    maxCore,
    verbose)
}




KMppIniSparse <- function(
  X,
  d,
  K,
  firstSelection = 1L,
  minkP = 2,
  stochastic = FALSE,
  seed = 123,
  maxCore = 7L,
  verbose = TRUE)
{
  KMppIniSparseCpp(
    X,
    d,
    firstSelection,
    K,
    minkP,
    stochastic,
    seed,
    maxCore,
    verbose)
}




























