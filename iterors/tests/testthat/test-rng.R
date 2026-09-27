`%is%` <- expect_equal

answer <- rbind(c(0, 1, 0, 0, 1),
                c(0, 1, 0, 0, 0),
                c(0, 0, 0, 1, 1))

test_that("converting seeds", {
  if (!exists('.Random.seed', where=.GlobalEnv, inherits=FALSE))
    set.seed(NULL)

  old.seed <- .Random.seed
  convertedSeed <- c(10407L, -1338817344L,  -129249855L,  2078773390L,
                 -1957431369L,  -846681972L,   797578269L)
  expect_equal(convseed(42069, "L'Ecuyer-CMRG"), convertedSeed)
  nextSeed <- c(10407L, 1962334501L, 1957360453L, 182261958L,
                 1552366930L,  -1175365792L,
                 -1765451585L)
  st <- iRNGStream(42069)
  expect_equal(nextOr(st), nextSeed)
  st <- iRNGStream(convertedSeed)
  expect_equal(nextOr(st), nextSeed)
  expect_equal(.Random.seed, old.seed)
  nextSubseed <- c(10407L, 80321366L, -2019582144L, -2075330821L,
                   -991763501L, -1016531363L, -1112286255L)
  sst <- iRNGSubStream(42069)
  expect_equal(nextOr(sst), nextSubseed)
  sst <- iRNGSubStream(convertedSeed)
  expect_equal(nextOr(sst), nextSubseed)

  set.seed(42069, kind="Mersenne-Twister")
  seed <- .Random.seed
  expect_error(sst <- iRNGSubStream(seed), "support")
  expect_error(sst <- iRNGStream(seed), "support")

})

test_that("RNG streams are reproducible", {

  global.seed <- .Random.seed
  rng.seeds <- iRNGStream(313)
  # create three pseudo-independent and
  # reproducible random number generators
  it1 <- isample(c(0, 1), 1, seed=nextOr(rng.seeds))
  it2 <- isample(c(0, 1), 1, seed=nextOr(rng.seeds))
  it3 <- isample(c(0, 1), 1, seed=nextOr(rng.seeds))

  expect_true(identical(.Random.seed, global.seed))
  take(it1, 5, "numeric") %is% answer[1,]
  take(it2, 5, "numeric") %is% answer[2,]
  take(it3, 5, "numeric") %is% answer[3,]

  # none of this affects the global seed
  expect_true(identical(.Random.seed, global.seed))
})

test_that("RNG streams operate independently", {

  rng.seeds <- iRNGStream(313)
  it1 <- isample(c(0, 1), 1, seed=nextOr(rng.seeds))
  it2 <- isample(c(0, 1), 1, seed=nextOr(rng.seeds))
  it3 <- isample(c(0, 1), 1, seed=nextOr(rng.seeds))

  # and this is independent of order in which
  # we call them...
  result <- take(i_roundrobin(it1, it2, it3), 15, "numeric")
  expect_true(identical(matrix(result, 3, 5), answer))

})

test_that("substream", {

  sst <- iRNGSubStream(42069)
  it <- isample(c(0, 1), 100,
                replace=TRUE, independent=TRUE, seed=sst())

  nextOr(it) %is% c(1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1,
                    0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1,
                    0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0,
                    0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0,
                    0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1,
                    1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0)
  rnorm(1)
  nextOr(it) %is% c(1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0,
                    1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1,
                    0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
                    1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0,
                    0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0,
                    1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0)

})


test_that("can use other generators", {

  set.seed(12, "Mersenne-Twister")
  MT1 <- runif(10)
  MTseed <- .Random.seed
  MT2 <- runif(10)
  set.seed(13, "Wichmann-Hill")
  WH1 <- runif(10)
  WHseed <- .Random.seed
  WH2 <- runif(10)

  iMT1 <- irunif(n=10, kind="Mersenne-Twister", seed=12)
  expect_equal(iMT1(), MT1)

  iWH2 <- irunif(n=10, seed=WHseed)
  expect_equal(iWH2(), WH2)

  iMT2 <- irunif(n=10, seed=MTseed)
  expect_equal(iMT2(), MT2)

  iWH1 <- irunif(n=10, kind="Wichmann-Hill", seed=13)
  expect_equal(iWH1(), WH1)

  expect_error(irunif(n=10, seed=MTseed[-4]), "length")

  expect_error(irunif(n=10, seed=1:10), "length")
  expect_error(irunif(10, seed=c(10402L, -1364902719L, 2066248591L, 1235L)), "length")
  expect_warning(expect_warning(irunif(n=10, seed=0:3), "buggy"),"uniform")
  expect_error(irunif(n=10, seed=c(10402, -1364902719, 2066248591)), "integer")

})

test_that("can save and restore random number generators", {

  it <- irunif(10, independent=TRUE)
  file1 <- tempfile(".Rdata")
  save(file=file1, it)
  first <- take(it, 5)
  file2 <- tempfile(".Rdata")
  save(file=file2, it)
  then <- take(it, 5)
  load(file2)
  expect_equal(take(it, 5), then)
  load(file1)
  expect_equal(take(it, 5), first)

})
