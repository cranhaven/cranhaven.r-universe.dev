context("Miscellaneous tests")

test_that("eval_pedigree_ll works on a data set that previously caused an infinite loop in version 0.2.3", {
  dat <- readRDS("loadings-dat-v-0.2.2-fail.RDS")

  # remove the duplicated terms and use weights. This can be done more efficiently
  # and may not catch all duplicates
  dat_unqiue <- dat[!duplicated(dat)]
  length(dat_unqiue) # number of unique terms

  # get the weights. This can be written in a much more efficient way
  c_weights <- sapply(dat_unqiue, function(x)
    sum(sapply(dat, identical, y = x)))

  ll_terms <- pedigree_ll_terms_loadings(dat_unqiue, max_threads = 2L)

  fail_par <- c(`(Intercept)` = -1.76889452670283, Binary = 3.64100978576773,
                -14.7413890098337, -41.8830052810072, 1.2443889418422,
                8.30393479943144, 13.8020541268237, -1.50147960603758)

  set.seed(1)
  res <- eval_pedigree_ll(
    ptr = ll_terms, par = fail_par, maxvls = 5000L, minvls = 1000L,
    abs_eps = 0, rel_eps = 1e-3, n_threads = 2L, use_aprx = TRUE,
    cluster_weights = c_weights, vls_scales = sqrt(c_weights))

  expect_true(is.nan(res))
})
