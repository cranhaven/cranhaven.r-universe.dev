#context("Test  1: median difference and heuristic tests")

library(eummd)
library(Rcpp)

test_that("mediandiff, slow lap univ", {
        X <- c(7.1, 1.2, 4.3, 0.4)
        Y <- c(5.5, 2.6, 8.7)
        ans <- mediandiff(X, Y, kernel="Laplacian", fast=FALSE)
        soln <- 3.2
        expect_equal(ans, soln)
        })

test_that("mediandiff test, fast lap univ", {
        X <- c(7.1, 1.2, 4.3, 0.4)
        Y <- c(5.5, 2.6, 8.7)
        ans <- mediandiff(X, Y, kernel="Laplacian", fast=TRUE)
        soln <- 3.2
        expect_equal(ans, soln)
        })


test_that("mediandiff, lap default univ", {
        X <- c(7.1, 1.2, 4.3, 0.4)
        Y <- c(5.5, 2.6, 8.7)
        ans <- mediandiff(X, Y)
        soln <- 3.2
        expect_equal(ans, soln)
        })


test_that("mediandiff, lap default univ, fast", {
        X <- c(7.1, 1.2, 4.3, 0.4)
        Y <- c(5.5, 2.6, 8.7)
        ans <- mediandiff(X, Y, fast=TRUE)
        soln <- 3.2
        expect_equal(ans, soln)
        })


test_that("mediandiff, lap default univ, fast", {
        X <- c(15.0, 23.2, 37.3, 41.4)
        Y <- c(56.5, 68.6, 72.8)
        ans <- mediandiff(X, Y, fast=TRUE)
        soln <- 26.4
        expect_equal(ans, soln)
        })


test_that("mediandiff, lap default univ, slow", {
        X <- c(15.0, 23.2, 37.3, 41.4)
        Y <- c(56.5, 68.6, 72.8)
        ans <- mediandiff(X, Y, fast=FALSE)
        soln <- 26.4
        expect_equal(ans, soln)
        })


test_that("mediandiff, gau default univ, slow", {
        X <- c(7.1, 1.2, 4.3, 0.4)
        Y <- c(5.5, 2.6, 8.7)
        ans <- mediandiff(X, Y, kernel="Gaussian")
        soln <- 10.24
        expect_equal(ans, soln)
        })


test_that("mediandiff, gau default univ, fast", {
        X <- c(7.1, 1.2, 4.3, 0.4)
        Y <- c(5.5, 2.6, 8.7)
        ans <- mediandiff(X, Y, kernel="Gaussian", fast=TRUE)
        soln <- 10.24
        expect_equal(ans, soln)
        })


test_that("mediandiff, wrong kernel, throws error", {
        X <- c(7.1, 1.2, 4.3, 0.4)
        Y <- c(5.5, 2.6, 8.7)
        expect_error( mediandiff(X, Y, kernel="Blah") )
        })


test_that("medianheuristic, lap default univ, fast", {
        X <- c(7.1, 1.2, 4.3, 0.4)
        Y <- c(5.5, 2.6, 8.7)
        ans <- medianheuristic(X, Y, fast=TRUE)
        soln <- 1.0/3.2
        expect_equal(ans, soln)
        })


test_that("medianheuristic, gau default univ, fast", {
        X <- c(7.1, 1.2, 4.3, 0.4)
        Y <- c(5.5, 2.6, 8.7)
        ans <- medianheuristic(X, Y, kernel="Gaussian", fast=TRUE)
        soln <- 1.0/10.24
        expect_equal(ans, soln)
        })


test_that("mediandiff, naive bivariate, lap", {

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
              ans <- mediandiff(X, Y)
              soln <- 12
              expect_equal(ans, soln)

        })


test_that("mediandiff, naive bivariate, gau", {

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
              ans <- mediandiff(X, Y, kernel="Gaussian")
              soln <- 72
              expect_equal(ans, soln)

        })


test_that("mediandiff, naive bivariate, fast error", {

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
              expect_error( mediandiff(X, Y, fast=T) )
        })


test_that("mediandiff, naive bivariate, no Y, lap", {

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
              #X <- matrix(c(1:12), ncol=2, byrow=T)
              #Y <- matrix(c(13:20), ncol=2, byrow=T)
              Z <- matrix(c(1:20), ncol=2, byrow=T)
              ans <- mediandiff(Z)
              soln <- 12
              expect_equal(ans, soln)

        })


test_that("mediandiff, naive bivariate, no Y, gau", {

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
              #X <- matrix(c(1:12), ncol=2, byrow=T)
              #Y <- matrix(c(13:20), ncol=2, byrow=T)
              Z <- matrix(c(1:20), ncol=2, byrow=T)
              ans <- mediandiff(Z, kernel="Gaussian")
              soln <- 72
              expect_equal(ans, soln)

        })


test_that("medianheuristic, lap default univ, fast, no Y", {
        X <- c(7.1, 1.2, 4.3, 0.4, 5.5, 2.6, 8.7)
        ans <- medianheuristic(X, fast=TRUE)
        soln <- 1.0/3.2
        expect_equal(ans, soln)
        })


test_that("medianheuristic, gau default univ, fast, no Y", {
        X <- c(7.1, 1.2, 4.3, 0.4, 5.5, 2.6, 8.7)
        ans <- medianheuristic(X, kernel="Gaussian", fast=TRUE)
        soln <- 1.0/10.24
        expect_equal(ans, soln)
        })
