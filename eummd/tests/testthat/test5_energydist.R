#context("Test  5: Energy Distance tests")

library(eummd)

test_that("Energy Distance test 1", {
              #X:
              #[[ 1.  2.]
              # [ 3.  4.]
              # [ 5.  6.]
              # [ 7.  8.]
              # [ 9. 10.]
              # [11. 12.]]
              #Y:
              #
              #[[13. 14.]
              # [15. 16.]
              # [17. 18.]
              # [19. 20.]]
              #pymmd lap:
              #0.6016073536188107
              X <- matrix(c(1:12), ncol=2, byrow=T)
              Y <- matrix(c(13:20), ncol=2, byrow=T)
              ansList <- energydist(X=X, Y=Y, pval=FALSE)
              ans_stat <- ansList$stat
              ans_pval <- ansList$pval

              soln_stat <- 16.970562748477143
              soln_pval <- NA

              expect_equal(ans_stat, soln_stat)
              expect_true(is.na(ans_pval))
})

