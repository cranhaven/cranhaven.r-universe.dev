#context("Test  2: MMD tests")

library(eummd)

test_that("MMD test 1, Laplacian kernel", {
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
              beta <- 0.1
              ansList <- mmd(X=X, Y=Y, beta=beta, pval=FALSE)
              #ansList <- mmd(X=X, Y=Y, beta=beta, pval=FALSE, kernel="Laplacian")
              ans_stat <- ansList$stat
              ans_pval <- ansList$pval
              ans_beta <- 0.1

              soln_stat <- 0.6016073536188107
              soln_pval <- NA
              soln_beta <- 0.1

              expect_equal(ans_stat, soln_stat)
              expect_true(is.na(ans_pval))
              expect_equal(ans_beta, soln_beta)
})


