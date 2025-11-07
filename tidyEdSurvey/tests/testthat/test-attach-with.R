sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

context('attach and detach')
test_that("attach and detach",{
  attach(sdf)
  expect_true("sdf" %in% search())
  detach(sdf)
  expect_false("sdf" %in% search())
  expect_error(t2 <- table(b017451))
})

context('with')
test_that("with",{
  t1 <- table(sdf$b017451)
  t2 <- with(sdf,table(b017451))
  expect_equal(unname(t1),unname(t2))
})
