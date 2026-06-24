test_that("eummd, univariate, no pval, positive beta", {
        X <- c(7.1, 1.2, 4.3, 0.4)
        Y <- c(5.5, 2.6, 8.7)
        beta <- 0.1
        seednum <- 1
        numperm <- 200
        ansList <- eummd(x=X, y=Y, beta=beta, pval=FALSE, seednum=seednum, numperm=numperm)
        ans_stat <- ansList$stat
        ans_pval <- ansList$pval
        ans_beta <- ansList$beta

        soln_stat <- -0.0594780368951533
        # pval will be different, because uses different permutations
        #soln_pval <- 0.6169154228855721
        soln_beta <- 0.1

        expect_equal(ans_stat, soln_stat)
        expect_true(is.na(ans_pval))
        expect_equal(ans_beta, soln_beta)
        })


test_that("eummd, univariate, no pval", {
        X <- c(7.1, 1.2, 4.3, 0.4)
        Y <- c(5.5, 2.6, 8.7)
        beta <- -0.1
        seednum <- 1
        numperm <- 200
        ansList <- eummd(x=X, y=Y, beta=beta, pval=FALSE, seednum=seednum, numperm=numperm)
        ans_stat <- ansList$stat
        ans_pval <- ansList$pval
        ans_beta <- ansList$beta

        soln_stat <- -0.129327129453085
        # pval will be different, because uses different permutations
        #soln_pval <- 0.6169154228855721
        soln_beta <- 1.0/3.2 

        expect_equal(ans_stat, soln_stat)
        expect_true(is.na(ans_pval))
        expect_equal(ans_beta, soln_beta)
        })



