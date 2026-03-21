#' @details If you want to test an MCMC sampler then the main function
#'     that you are going to need from this package are
#'     [expect_mcmc] and [expect_mcmc_reversible] which
#'     can be used as part of unit testing in the framework of the
#'     testthat package. They test if MCMC algorithms have the correct
#'     invariant distribution.
#'
#'     If you are testing iid samples then [expect_mc_iid_mean],
#'     [expect_mc_iid_ks], [expect_mc_iid_chisq] and [expect_mc_test]
#'     will be useful.
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
