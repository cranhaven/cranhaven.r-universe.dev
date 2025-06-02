context("mpt_options()")

test_that("resetting works", {
  mpt_options("default")
  op <- mpt_options()


  mpt_options(Neff_min = 89L)

  expect_equal(mpt_options()$treebugs$Neff_min, 89L)

  mpt_options(bootstrap_samples = c(xx = 111))
  expect_equal(mpt_options()$mptinr$bootstrap_samples, 111L)

  mpt_options(max_ci_indiv = c(tt = .89))
  expect_equal(mpt_options()$max_ci_indiv, .89)

  expect_false(isTRUE(all.equal(op, mpt_options())))

  mpt_options(op)
  expect_equal(op, mpt_options())

})

test_that("convenience settings work", {
  mpt_options("default")
  op <- mpt_options()
  mpt_options("test")
  op2 <- mpt_options()
  expect_false(isTRUE(all.equal(op, op2)))
})
