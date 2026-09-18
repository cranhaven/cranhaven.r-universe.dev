
context("hilde")

# a simple way to filter data, not very precise, but enough for test purposes
.convolve <- function(data, filter) {
  stats::filter(data, filter$kern, sides = 1)[-c(1:filter$len)] / sqrt(sum(filter$kern^2))
}

test_that("it works if q1, q2, lengths are given and that data and filter have to be given", {
  skip_on_cran()
  testdata <- rnorm(100)
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 0.1), sr = 1, len = 8L,
                              shift = 0.5)
  testq <- getCritVal(n = length(testdata), filter = testfilter, family = "jsmurfPS")
  expect_error(hilde(family = "jsmurfPS"))
  expect_error(hilde(family = "jsmurfPS", data = testdata))
  expect_error(hilde(family = "jsmurfPS", filter = testfilter))
  
  compare <- improveSmallScales(fit = jsmurf(family = "jsmurfPS", data = testdata, filter = testfilter, q = testq,
                                             locationCorrection = "none"),
                                data = testdata, filter = testfilter,
                                method = "LR", lengths = 1:20, q = rep(25, 20),
                                suppressWarningNoDeconvolution = TRUE)
  attr(compare, "q") <- NULL
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         suppressWarningNoDeconvolution = TRUE), compare)
  
  
  testq <- getCritVal(n = length(testdata), filter = testfilter, family = "hjsmurf")
  compare <- improveSmallScales(fit = jsmurf(family = "hjsmurf", data = testdata, filter = testfilter, q = testq,
                                             locationCorrection = "none"),
                                data = testdata, filter = testfilter,
                                method = "2Param", lengths = 1:65, q = rep(150, 65),
                                suppressWarningNoDeconvolution = TRUE)
  attr(compare, "q") <- NULL
  expect_identical(hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testq,
                         method = "2Param", lengths = 1:65, q2 = rep(150, 65),
                         suppressWarningNoDeconvolution = TRUE), compare)
})

test_that("output is tested and works", {
  testdata <- rnorm(100)
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 0.1), sr = 1, len = 8L,
                              shift = 0.5)
  testq <- getCritVal(n = length(testdata), filter = testfilter, family = "jsmurfPS")
  
  expect_error(suppressWarnings(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, q2 = rep(25, 20), output = 1)))
  expect_error(suppressWarnings(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, q2 = rep(25, 20),
                     output = c("only", "every"))))
  expect_error(suppressWarnings(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                     q2 = rep(25, 20), output = "aha")))
  expect_identical(suppressWarnings(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                         suppressWarningNoDeconvolution = TRUE, lengths = 1:20, q2 = rep(25, 20))),
                   suppressWarnings(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                         output = "only", suppressWarningNoDeconvolution = TRUE, lengths = 1:20, q2 = rep(25, 20))))
  
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, method = "LR",
                         lengths = 1:20, q2 = rep(25, 20), suppressWarningNoDeconvolution = TRUE),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, method = "LR",
                         lengths = 1:20, q2 = rep(25, 20), output = "each",
                         suppressWarningNoDeconvolution = TRUE)$idealization)
  comparesd <- stepR::sdrobnorm(testdata, lag = testfilter$len + 1)
  compareFit <- jsmurf(family = "jsmurfPS", data = testdata, filter = testfilter, q = testq, locationCorrection = "none")
  compare <- improveSmallScales(fit = compareFit, data = testdata, filter = testfilter,
                                method = "LR", lengths = 1:20, q = rep(25, 20),
                                suppressWarningNoDeconvolution = TRUE)
  compareq2 <- attr(compare, "q")
  attr(compare, "q") <- NULL
  compare <- list(idealization = compare, fit = compareFit, q1 = testq, q2 = compareq2,
                  filter = testfilter, sd =  comparesd)
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         output = "each", suppressWarningNoDeconvolution = TRUE), compare)
  
  compare <- hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                   method = "LR", lengths = 1:20, q2 = rep(25, 20),
                   output = "every", suppressWarningNoDeconvolution = TRUE)$idealization
  compare2 <- compare[[3]]
  attr(compare2, "noDeconvolution") <- attr(compare, "noDeconvolution")
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         suppressWarningNoDeconvolution = TRUE), compare2)
  compareFit <- jsmurf(family = "jsmurfPS", data = testdata, filter = testfilter, q = testq, 
                       locationCorrection = "none")
  compare <- improveSmallScales(fit = compareFit, data = testdata, filter = testfilter,
                                method = "LR", lengths = 1:20, q = rep(25, 20),
                                output = "every", suppressWarningNoDeconvolution = TRUE)
  compareq2 <- attr(compare, "q")
  attr(compare, "q") <- NULL
  compare <- list(idealization = compare, fit = compareFit, q1 = testq, q2 = compareq2,
                  filter = testfilter, sd = comparesd)
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         output = "every", suppressWarningNoDeconvolution = TRUE), compare)
  
  
  testq <- getCritVal(n = length(testdata), filter = testfilter, family = "hjsmurf")
  expect_error(hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testq, q2 = rep(150, 65), output = 1))
  expect_error(hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testq, q2 = rep(150, 65),
                     output = c("only", "every")))
  expect_error(hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testq, q2 = rep(150, 65),
                     output = "aha"))
  expect_identical(hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testq,
                         suppressWarningNoDeconvolution = TRUE, lengths = c(3, 5, 10, 20, 50), q2 = rep(150, 5)),
                   hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testq,
                         output = "only", suppressWarningNoDeconvolution = TRUE, lengths = c(3, 5, 10, 20, 50),
                         q2 = rep(150, 5)))
  
  expect_identical(hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testq, method = "2Param",
                         lengths = c(3, 5, 10, 20, 50), q2 = rep(150, 5), suppressWarningNoDeconvolution = TRUE),
                   hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testq, method = "2Param",
                         lengths = c(3, 5, 10, 20, 50), q2 = rep(150, 5), output = "each",
                         suppressWarningNoDeconvolution = TRUE)$idealization)
  compareFit <- jsmurf(family = "hjsmurf", data = testdata, filter = testfilter, q = testq, locationCorrection = "none")
  compare <- improveSmallScales(fit = compare, data = testdata, filter = testfilter,
                                method = "2Param", lengths = c(3, 5, 10, 20, 50), q = rep(150, 5),
                                suppressWarningNoDeconvolution = TRUE)
  compareq2 <- attr(compare, "q")
  attr(compare, "q") <- NULL
  compare <- list(idealization = compare, fit = compareFit, q1 = testq, q2 = compareq2, filter = testfilter)
  expect_identical(hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testq,
                         method = "2Param", lengths = c(3, 5, 10, 20, 50), q2 = rep(150, 5),
                         output = "each", suppressWarningNoDeconvolution = TRUE), compare)
  
  compare <- hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testq,
                   method = "2Param", lengths = c(3, 5, 10, 20, 50), q2 = rep(150, 5),
                   output = "every", suppressWarningNoDeconvolution = TRUE)$idealization
  compare2 <- compare[[3]]
  attr(compare2, "noDeconvolution") <- attr(compare, "noDeconvolution")
  expect_identical(hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testq,
                         method = "2Param", lengths = c(3, 5, 10, 20, 50), q2 = rep(150, 5),
                         suppressWarningNoDeconvolution = TRUE), compare2)
  compareFit <- jsmurf(family = "hjsmurf", data = testdata, filter = testfilter, q = testq,
                    locationCorrection = "none")
  compare <- improveSmallScales(fit = compareFit, data = testdata, filter = testfilter,
                                method = "2Param", lengths = c(3, 5, 10, 20, 50), q = rep(150, 5),
                                output = "every", suppressWarningNoDeconvolution = TRUE)
  compareq2 <- attr(compare, "q")
  attr(compare, "q") <- NULL
  compare <- list(idealization = compare, fit = compareFit, q1 = testq, q2 = compareq2, filter = testfilter)
  expect_identical(hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testq,
                         method = "2Param", lengths = c(3, 5, 10, 20, 50), q2 = rep(150, 5),
                         output = "every", suppressWarningNoDeconvolution = TRUE), compare)
})

test_that("more difficult scenarios work", {
  testdata <- c(rnorm(108, 0), rnorm(100, 10), rnorm(100, 0), rnorm(100, 10))
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 0.1), sr = 1, len = 8L,
                              shift = 0.5)
  testdata <- .convolve(testdata, testfilter)
  testq <- getCritVal(n = length(testdata), filter = testfilter, family = "jsmurfPS")
  
  compare <- improveSmallScales(fit = jsmurf(family = "jsmurfPS", data = testdata, filter = testfilter, q = testq,
                                             locationCorrection = "none"),
                                data = testdata, filter = testfilter,
                                method = "LR", lengths = 1:20, q = rep(25, 20),
                                suppressWarningNoDeconvolution = TRUE)
  attr(compare, "q") <- NULL
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         suppressWarningNoDeconvolution = TRUE), compare)
  
  testdata <- c(rnorm(100, 0), rnorm(5, 10), rnorm(100, 0))
  testdata <- .convolve(testdata, testfilter)
  compare <- improveSmallScales(fit = jsmurf(family = "jsmurfPS", data = testdata, filter = testfilter, q = testq,
                                             locationCorrection = "none"),
                                data = testdata, filter = testfilter,
                                method = "LR", lengths = 1:20, q = rep(25, 20),
                                suppressWarningNoDeconvolution = TRUE)
  attr(compare, "q") <- NULL
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         suppressWarningNoDeconvolution = TRUE), compare)
  
  testdata <- c(rnorm(100, 0), rnorm(5, 15), rnorm(5, 30), rnorm(5, 45), rnorm(100, 60))
  testdata <- .convolve(testdata, testfilter)
  compare <- improveSmallScales(fit = jsmurf(family = "jsmurfLR", data = testdata, filter = testfilter, q = testq,
                                             locationCorrection = "none"),
                                data = testdata, filter = testfilter,
                                method = "LR", lengths = 1:20, q = rep(25, 20),
                                suppressWarningNoDeconvolution = TRUE)
  attr(compare, "q") <- NULL
  expect_identical(hilde(family = "jsmurfLR", data = testdata, filter = testfilter, q1 = testq, 
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         suppressWarningNoDeconvolution = TRUE), compare)
})

test_that("argument data is tested", {
  testdata <- c(rnorm(108, 0), rnorm(100, 10), rnorm(100, 0), rnorm(100, 10))
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 0.1), sr = 1, len = 8L,
                              shift = 0.5)
  testdata <- .convolve(testdata, testfilter)
  testq <- getCritVal(n = length(testdata), filter = testfilter, family = "jsmurfPS")

  expect_error(hilde(family = "jsmurfPS", data = c(testdata, "s"), filter = testfilter, q1 = testq))
  expect_error(hilde(family = "jsmurfPS", data = c(testdata, Inf), filter = testfilter, q1 = testq))
  expect_error(hilde(family = "jsmurfPS", data = c(testdata, as.numeric(NA)), filter = testfilter, q1 = testq))
})

test_that("argument filter works and is tested", {
  testdata <- c(rnorm(108, 0), rnorm(100, 10), rnorm(100, 0), rnorm(100, 10))
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 0.1), sr = 1, len = 8L,
                              shift = 0.5)
  testdata <- .convolve(testdata, testfilter)
  
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = list(test = 1), q1 = testq))
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = unclass(testfilter), q1 = testq))

  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 5L, cutoff = 0.15), sr = 2143, len = 5L,
                              shift = 0.2)
  testq <- getCritVal(n = length(testdata), filter = testfilter, family = "jsmurf")
  compare <- improveSmallScales(fit = jsmurf(family = "jsmurf", data = testdata, filter = testfilter, q = testq,
                                             locationCorrection = "none"),
                                data = testdata, filter = testfilter,
                                method = "LR", lengths = 1:20, q = rep(25, 20),
                                suppressWarningNoDeconvolution = TRUE)
  attr(compare, "q") <- NULL
  expect_identical(hilde(family = "jsmurf", data = testdata, filter = testfilter, q1 = testq, 
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         suppressWarningNoDeconvolution = TRUE), compare)
})

test_that("argument family is tested", {
  testdata <- rnorm(100)
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 0.1), sr = 1, len = 8L,
                              shift = 0.5)
  testq <- getCritVal(n = length(testdata), filter = testfilter, family = "jsmurfPS")
  
  expect_error(hilde(family = "jsmurf2", data = testdata, filter = testfilter, q1 = testq, 
                     method = "LR", lengths = 1:20, q2 = rep(25, 20),
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "hjs", data = testdata, filter = testfilter, q1 = testq, 
                     method = "LR", lengths = 1:20, q2 = rep(25, 20),
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = c("hjsmurf", "hjsmurfSPS"), data = testdata, filter = testfilter, q1 = testq, 
                     method = "LR", lengths = 1:20, q2 = rep(25, 20),
                     suppressWarningNoDeconvolution = TRUE))
})

test_that("argument method is tested", {
  testdata <- rnorm(100)
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 0.1), sr = 1, len = 8L,
                              shift = 0.5)
  testq <- getCritVal(n = length(testdata), filter = testfilter, family = "jsmurf")
  
  expect_error(hilde(family = "jsmurf", data = testdata, filter = testfilter, q1 = testq, 
                     method = "LRs", lengths = 1:20, q2 = rep(25, 20),
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurf", data = testdata, filter = testfilter, q1 = testq, 
                     method = c("LR", "2Param"), lengths = 1:20, q2 = rep(25, 20),
                     suppressWarningNoDeconvolution = TRUE))
  
  testq <- getCritVal(n = length(testdata), filter = testfilter, family = "hjsmurf")
  expect_warning(hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testq, 
                       method = c("LR"), lengths = 1:20, q2 = rep(25, 20),
                       suppressWarningNoDeconvolution = TRUE))
  expect_warning(hilde(family = "hjsmurfSPS", data = testdata, filter = testfilter, q1 = testq, 
                       method = c("LR"), lengths = 1:20, q2 = rep(25, 20),
                       suppressWarningNoDeconvolution = TRUE))
  expect_warning(hilde(family = "hjsmurfLR", data = testdata, filter = testfilter, q1 = testq, 
                       method = c("LR"), lengths = 1:20, q2 = rep(25, 20),
                       suppressWarningNoDeconvolution = TRUE))
  testq <- getCritVal(n = length(testdata), filter = testfilter, family = "jsmurf")
  expect_warning(hilde(family = "jsmurf", data = testdata, filter = testfilter, q1 = testq, 
                       method = c("2Param"), lengths = 1:20, q2 = rep(25, 20),
                       suppressWarningNoDeconvolution = TRUE))
  expect_warning(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
                       method = c("2Param"), lengths = 1:20, q2 = rep(25, 20),
                       suppressWarningNoDeconvolution = TRUE))
  expect_warning(hilde(family = "jsmurfLR", data = testdata, filter = testfilter, q1 = testq, 
                       method = c("2Param"), lengths = 1:20, q2 = rep(25, 20),
                       suppressWarningNoDeconvolution = TRUE))
})

test_that("argument q1 works and is tested", {
  testdata <- c(rnorm(108, 0), rnorm(5, 10), rnorm(5, 20), rnorm(5, 30), rnorm(100, 40))
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 0.1), sr = 1, len = 8L,
                              shift = 0.5)
  testdata <- .convolve(testdata, testfilter)
  testq <- getCritVal(n = length(testdata), filter = testfilter, family = "jsmurf")
  
  expect_error(hilde(family = "jsmurf", data = testdata, filter = testfilter, q1 = "s", 
                     method = "LR", lengths = 1:20, q2 = rep(25, 20),
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurf", data = testdata, filter = testfilter, q1 = as.numeric(NA), 
                     method = "LR", lengths = 1:20, q2 = rep(25, 20),
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurf", data = testdata, filter = testfilter, q1 = Inf, 
                     method = "LR", lengths = 1:20, q2 = rep(25, 20),
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurf", data = testdata, filter = testfilter, q1 = c(1, 2), 
                     method = "LR", lengths = 1:20, q2 = rep(25, 20),
                     suppressWarningNoDeconvolution = TRUE))
  
  compare <- improveSmallScales(fit = jsmurf(family = "jsmurf", data = testdata, filter = testfilter, q = testq,
                                             locationCorrection = "none"),
                                data = testdata, filter = testfilter, 
                                method = "LR", lengths = 1:20, q = rep(25, 20),
                                suppressWarningNoDeconvolution = TRUE)
  attr(compare, "q") <- NULL
  expect_identical(hilde(family = "jsmurf", data = testdata, filter = testfilter, q1 = testq, 
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         suppressWarningNoDeconvolution = TRUE), compare)
})

test_that("argument alpha1 works and is tested", {
  testdata <- c(rnorm(108, 0), rnorm(5, 10), rnorm(5, 20), rnorm(5, 30), rnorm(100, 40))
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 0.1), sr = 1, len = 8L,
                              shift = 0.5)
  testdata <- .convolve(testdata, testfilter)
  teststat <- stepR::monteCarloSimulation(n = 215, r = 100, family = "jsmurfPS", filter = testfilter)
  
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                     alpha1 = "s", stat = teststat,
                     method = "LR", lengths = 1:20, q2 = rep(25, 20),
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                     alpha1 = as.numeric(NA), stat = teststat,
                     method = "LR", lengths = 1:20, q2 = rep(25, 20),
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                     alpha1 = NULL, stat = teststat,
                     method = "LR", lengths = 1:20, q2 = rep(25, 20),
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                     alpha1 = c(0.1, 0.2), stat = teststat,
                     method = "LR", lengths = 1:20, q2 = rep(25, 20),
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurf", data = testdata, filter = testfilter,
                     alpha1 = 0, stat = teststat,
                     method = "LR", lengths = 1:20, q2 = rep(25, 20),
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                     alpha1 = 1, stat = teststat,
                     method = "LR", lengths = 1:20, q2 = rep(25, 20),
                     suppressWarningNoDeconvolution = TRUE))
  
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                         stat = teststat, output = "every",
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         suppressWarningNoDeconvolution = TRUE),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                         alpha1 = 0.01, stat = teststat, output = "every",
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         suppressWarningNoDeconvolution = TRUE))

  testq <- getCritVal(family = "jsmurfPS", n = length(testdata), stat = teststat, filter = testfilter,
                      alpha = 0.01)
  compare <- improveSmallScales(fit = jsmurf(family = "jsmurfPS", data = testdata, filter = testfilter, q = testq,
                                             locationCorrection = "none"),
                                data = testdata, filter = testfilter, 
                                method = "LR", lengths = 1:20, q = rep(25, 20),
                                suppressWarningNoDeconvolution = TRUE)
  attr(compare, "q") <- NULL
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                         stat = teststat,
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         suppressWarningNoDeconvolution = TRUE), compare)
  
  testq <- getCritVal(family = "jsmurfPS", n = length(testdata), stat = teststat, filter = testfilter,
                      alpha = 0.04)
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                         stat = teststat, alpha1 = 0.04, output = "each",
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         suppressWarningNoDeconvolution = TRUE)$idealization,
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                         q1 = testq, method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         suppressWarningNoDeconvolution = TRUE))

  testq <- getCritVal(family = "jsmurfPS", n = length(testdata), stat = teststat, filter = testfilter,
                      alpha = 0.076)
  comparesd <- stepR::sdrobnorm(testdata, lag = testfilter$len + 1)
  compareFit <- jsmurf(family = "jsmurfPS", data = testdata, filter = testfilter,
                       q = testq, locationCorrection = "none")
  compare <- improveSmallScales(fit = compareFit, data = testdata, filter = testfilter,
                                method = "LR", lengths = 3:7, q = rep(25, 5),
                                suppressWarningNoDeconvolution = TRUE)
  compareq2 <- attr(compare, "q")
  attr(compare, "q") <- NULL
  compare <- list(idealization = compare, fit = compareFit, q1 = testq, q2 = compareq2,
                  filter = testfilter, sd =  comparesd)
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, alpha1 = 0.076,
                         method = "LR", lengths = 3:7, q2 = rep(25, 5), stat = teststat,
                         output = "each", suppressWarningNoDeconvolution = TRUE), compare)
  
  
  testq <- getCritVal(family = "jsmurfPS", n = length(testdata), stat = teststat, filter = testfilter,
                      alpha = 0.3)
  compare <- hilde(family = "jsmurfPS", data = testdata, filter = testfilter, alpha1 = 0.3, stat = teststat,
                   method = "LR", lengths = 1:20, q2 = rep(25, 20),
                   output = "every", suppressWarningNoDeconvolution = TRUE)$idealization
  compare2 <- compare[[3]]
  attr(compare2, "noDeconvolution") <- attr(compare, "noDeconvolution")
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         suppressWarningNoDeconvolution = TRUE), compare2)

  testq <- getCritVal(family = "jsmurfPS", n = length(testdata), stat = teststat, filter = testfilter,
                      alpha = 0.1)
  compareFit <- jsmurf(family = "jsmurfPS", data = testdata, filter = testfilter, q = testq, 
                       locationCorrection = "none")
  compare <- improveSmallScales(fit = compareFit, data = testdata, filter = testfilter,
                                method = "LR", lengths = 1:20, q = rep(25, 20),
                                output = "every", suppressWarningNoDeconvolution = TRUE)
  compareq2 <- attr(compare, "q")
  attr(compare, "q") <- NULL
  compare <- list(idealization = compare, fit = compareFit, q1 = testq, q2 = compareq2,
                  filter = testfilter, sd = comparesd)
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, alpha1 = 0.1,
                         method = "LR", lengths = 1:20, q2 = rep(25, 20), stat = teststat,
                         output = "every", suppressWarningNoDeconvolution = TRUE), compare)
})

test_that("argument q2 works and is tested", {
  testdata <- c(rnorm(108, 0), rnorm(5, 10), rnorm(5, 20), rnorm(5, 30), rnorm(100, 40))
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 0.1), sr = 1, len = 8L,
                              shift = 0.5)
  testdata <- .convolve(testdata, testfilter)
  testq <- getCritVal(n = length(testdata), filter = testfilter, family = "jsmurf")
  
  expect_error(hilde(family = "jsmurf", data = testdata, filter = testfilter, q1 = testq, 
                     method = "LR", lengths = 3, q2 = "s",
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurf", data = testdata, filter = testfilter, q1 = testq, 
                     method = "LR", lengths = 3, q2 = as.numeric(NA),
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurf", data = testdata, filter = testfilter, q1 = testq, 
                     method = "LR", lengths = 3, q2 = Inf,
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurf", data = testdata, filter = testfilter, q1 = testq, 
                     method = "LR", lengths = 3, q2 = c(1, 2),
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurf", data = testdata, filter = testfilter, q1 = testq, 
                     method = "LR", lengths = 1:4, q2 = c(1, 2, 3),
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurf", data = testdata, filter = testfilter, q1 = testq, 
                     method = "LR", lengths = c(3, 5), q2 = c(1, Inf),
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurf", data = testdata, filter = testfilter, q1 = testq, 
                     method = "LR", lengths = c(3, 5), q2 = 1,
                     suppressWarningNoDeconvolution = TRUE))
  
  expect_identical(hilde(family = "jsmurf", data = testdata, filter = testfilter, q1 = testq, 
                         method = "LR", lengths = c(3, 5), q2 = 1, penalty = "sqrt",
                         suppressWarningNoDeconvolution = TRUE, output = "every"),
                   hilde(family = "jsmurf", data = testdata, filter = testfilter, q1 = testq, 
                         method = "LR", lengths = c(3, 5),
                         q2 = stepR::critVal(q = 1, n = length(testdata), filter = testfilter,
                                             family = "LR", penalty = "sqrt", lengths = c(3, 5)),
                         suppressWarningNoDeconvolution = TRUE, output = "every"))
  expect_identical(hilde(family = "jsmurf", data = testdata, filter = testfilter, q1 = testq, 
                         method = "LR", lengths = c(3, 5), q2 = 3, penalty = "log", nq = 300L,
                         suppressWarningNoDeconvolution = TRUE, output = "every"),
                   hilde(family = "jsmurf", data = testdata, filter = testfilter, q1 = testq, 
                         method = "LR", lengths = c(3, 5),
                         q2 = stepR::critVal(q = 3, n = length(testdata), filter = testfilter, nq = 300L,
                                             family = "LR", penalty = "log", lengths = c(3, 5)),
                         suppressWarningNoDeconvolution = TRUE, output = "every"))
  expect_identical(hilde(family = "jsmurf", data = testdata, filter = testfilter, q1 = testq, 
                         method = "LR", lengths = c(3, 5), q2 = as.numeric(1:215),
                         suppressWarningNoDeconvolution = TRUE, output = "every"),
                   hilde(family = "jsmurf", data = testdata, filter = testfilter, q1 = testq, 
                         method = "LR", lengths = c(3, 5), q2 = c(3, 5),
                         suppressWarningNoDeconvolution = TRUE, output = "every"))
})



test_that("argument alpha2 works and is tested", {
  testdata <- c(rnorm(108, 0), rnorm(5, 10), rnorm(5, 20), rnorm(5, 30), rnorm(100, 40))
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 0.1), sr = 1, len = 8L,
                              shift = 0.5)
  testdata <- .convolve(testdata, testfilter)
  testq <- getCritVal(n = length(testdata), filter = testfilter, family = "jsmurfPS")
  teststat <- stepR::monteCarloSimulation(n = 215, r = 10, family = "LR", filter = testfilter)

  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                     q1 = testq, alpha2 = "s", stat = teststat,
                     method = "LR", lengths = 1:20,
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                     q1 = testq, alpha2 = as.numeric(NA), stat = teststat,
                     method = "LR", lengths = 1:20,
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                     q1 = testq, alpha2 = NULL, stat = teststat,
                     method = "LR", lengths = 1:20,
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                     q1 = testq, alpha2 = c(0.1, 0.2), stat = teststat,
                     method = "LR", lengths = 1:20,
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurf", data = testdata, filter = testfilter,
                     q1 = testq, alpha2 = 0, stat = teststat,
                     method = "LR", lengths = 1:20,
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                     q1 = testq, alpha2 = 1, stat = teststat,
                     method = "LR", lengths = 1:20,
                     suppressWarningNoDeconvolution = TRUE))
  
  
  teststat <- stepR::monteCarloSimulation(family = "LR", n = length(testdata), filter = testfilter, r = 2)
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                         q1 = testq, stat = teststat,
                         method = "LR", lengths = 1:20,
                         suppressWarningNoDeconvolution = TRUE, output = "every"),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                         q1 = testq, stat = teststat, alpha2 = 0.04,
                         method = "LR", lengths = 1:20,
                         suppressWarningNoDeconvolution = TRUE, output = "every"))
  
  testq2 <- getCritVal(family = "LR", n = length(testdata), stat = teststat,
                       filter = testfilter, lengths = c(1, 8), alpha = 0.123)
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                         q1 = testq, stat = teststat, alpha2 = 0.123,
                         method = "LR", lengths = c(1, 8),
                         suppressWarningNoDeconvolution = TRUE, output = "every"),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                         q1 = testq, stat = teststat, q2 = testq2,
                         method = "LR", lengths = c(1, 8),
                         suppressWarningNoDeconvolution = TRUE, output = "every"))
})

test_that("argument sd works and is tested", {
  testdata <- c(rnorm(108, 0), rnorm(5, 10), rnorm(5, 20), rnorm(5, 30), rnorm(100, 40))
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 0.1), sr = 1, len = 8L,
                              shift = 0.5)
  testdata <- .convolve(testdata, testfilter)
  testsd <- 0.5
  testq <- getCritVal(n = length(testdata), filter = testfilter, family = "jsmurfPS")

  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
                     method = "LR", lengths = 1:20, q2 = rep(25, 20), sd = "s",
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
                     method = "LR", lengths = 1:20, q2 = rep(25, 20), sd = Inf,
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
                     method = "LR", lengths = 1:20, q2 = rep(25, 20), sd = c(1, 0.5),
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
                     method = "LR", lengths = 1:20, q2 = rep(25, 20), sd = -1,
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
                     method = "LR", lengths = 1:20, q2 = rep(25, 20), sd = 0,
                     suppressWarningNoDeconvolution = TRUE))
  
  estsd <- stepR::sdrobnorm(testdata, lag = 9L)
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         suppressWarningNoDeconvolution = TRUE),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
                         method = "LR", lengths = 1:20, q2 = rep(25, 20), sd = estsd,
                         suppressWarningNoDeconvolution = TRUE))
  
  testq <- getCritVal(n = length(testdata), filter = testfilter, family = "hjsmurfLR")
  expect_warning(ret <- hilde(family = "hjsmurfLR", data = testdata, filter = testfilter, q1 = testq, 
                              method = "2Param", lengths = 1:20, q2 = rep(25, 20), sd = testsd,
                              suppressWarningNoDeconvolution = TRUE))
  expect_identical(ret, hilde(family = "hjsmurfLR", data = testdata, filter = testfilter, q1 = testq, 
                              method = "2Param", lengths = 1:20, q2 = rep(25, 20),
                              suppressWarningNoDeconvolution = TRUE))
  
  compare <- improveSmallScales(fit = jsmurf(family = "jsmurfPS", data = testdata, filter = testfilter, q = testq,
                                             locationCorrection = "none", sd = testsd),
                                data = testdata, filter = testfilter, 
                                method = "LR", lengths = 1:20, q = rep(25, 20),
                                suppressWarningNoDeconvolution = TRUE)
  attr(compare, "q") <- NULL
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
                         method = "LR", lengths = 1:20, q2 = rep(25, 20), sd = testsd,
                         suppressWarningNoDeconvolution = TRUE), compare)
})
 
test_that("argument startTime works and is tested", {
  testdata <- c(rnorm(108, 0), rnorm(5, 10), rnorm(5, 20), rnorm(5, 30), rnorm(100, 40))
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 0.1), sr = 1, len = 8L,
                              shift = 0.5)
  testdata <- .convolve(testdata, testfilter)
  testq <- getCritVal(n = length(testdata), filter = testfilter, family = "hjsmurfSPS")
  
  expect_identical(hilde(family = "hjsmurfSPS", data = testdata, filter = testfilter, q1 = testq, 
                         method = "2Param", lengths = 1:20, q2 = rep(25, 20),
                         suppressWarningNoDeconvolution = TRUE),
                   hilde(family = "hjsmurfSPS", data = testdata, filter = testfilter, q1 = testq, 
                         method = "2Param", lengths = 1:20, q2 = rep(25, 20), startTime = 0,
                         suppressWarningNoDeconvolution = TRUE))
  
  expect_error(hilde(family = "hjsmurfSPS", data = testdata, filter = testfilter, q1 = testq, 
                     method = "2Param", lengths = 1:20, q2 = rep(25, 20), startTime = "0",
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "hjsmurfSPS", data = testdata, filter = testfilter, q1 = testq, 
                     method = "2Param", lengths = 1:20, q2 = rep(25, 20), startTime = Inf,
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "hjsmurfSPS", data = testdata, filter = testfilter, q1 = testq, 
                     method = "2Param", lengths = 1:20, q2 = rep(25, 20), startTime = as.numeric(NA),
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "hjsmurfSPS", data = testdata, filter = testfilter, q1 = testq, 
                     method = "2Param", lengths = 1:20, q2 = rep(25, 20), startTime = c(0, 0.5),
                     suppressWarningNoDeconvolution = TRUE))

  compare <- improveSmallScales(fit = jsmurf(family = "hjsmurfSPS", data = testdata, filter = testfilter, q = testq,
                                             locationCorrection = "none", startTime = -1),
                                data = testdata, filter = testfilter, startTime = -1,
                                method = "2Param", lengths = c(3, 4), q = rep(25, 2),
                                suppressWarningNoDeconvolution = TRUE)
  attr(compare, "q") <- NULL
  expect_identical(hilde(family = "hjsmurfSPS", data = testdata, filter = testfilter, q1 = testq, 
                         method = "2Param", lengths = c(3, 4), q2 = rep(25, 2), startTime = -1,
                         suppressWarningNoDeconvolution = TRUE), compare)
  
  compare <- improveSmallScales(fit = jsmurf(family = "hjsmurfSPS", data = testdata, filter = testfilter, q = testq,
                                             locationCorrection = "none", startTime = 100 / testfilter$sr),
                                data = testdata, filter = testfilter, startTime = 100 / testfilter$sr,
                                method = "2Param", lengths = c(7, 9), q = rep(25, 2),
                                suppressWarningNoDeconvolution = TRUE)
  attr(compare, "q") <- NULL
  expect_identical(hilde(family = "hjsmurfSPS", data = testdata, filter = testfilter, q1 = testq, 
                         method = "2Param", lengths = c(7, 9), q2 = rep(25, 2), startTime = 100 / testfilter$sr,
                         suppressWarningNoDeconvolution = TRUE), compare)
})

test_that("... works and is tested", {
  testdata <- rnorm(100)
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 0.1), sr = 1, len = 8L,
                              shift = 0.5)
  testq <- getCritVal(n = length(testdata), filter = testfilter, family = "jsmurfPS")
  
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
                     method = "LR", lengths = c(1:19, NA), q2 = rep(25, 20),
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
                     method = "LR", lengths = c(0, 1:19), q2 = rep(25, 20),
                     suppressWarningNoDeconvolution = TRUE))
  
  expect_warning(ret <- hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
                              method = "LR", lengths = c(1:20, 10), q2 = rep(25, 20),
                              suppressWarningNoDeconvolution = TRUE, output = "everything"))
  expect_identical(ret, hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
                              method = "LR", lengths = c(1:20), q2 = rep(25, 20),
                              suppressWarningNoDeconvolution = TRUE, output = "everything"))
  
  testq2 <- getCritVal(n = length(testdata), family = "LR", filter = testfilter, alpha = 0.04,
                       r = 10, options = list(load = list(), simulation = "matrixIncreased"),
                       lengths = 21L, nq = length(testdata))
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
                         method = "LR", lengths = 21L, r = 10L, options = list(load = list()),
                         suppressWarningNoDeconvolution = TRUE, output = "everything"),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
                         method = "LR", lengths = 21L, q2 = testq2,
                         suppressWarningNoDeconvolution = TRUE, output = "everything"))
  
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
                     method = "LR", lengths = 1:20, q2 = rep(25, 20),
                     thresholdLongSegment = c(10, 20), suppressWarningNoDeconvolution = TRUE))
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         thresholdLongSegment = 10.5, suppressWarningNoDeconvolution = TRUE),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         thresholdLongSegment = 10L, suppressWarningNoDeconvolution = TRUE))

  testfilter2 <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 31)
  testdata2 <- lowpassFilter::randomGeneration(n = 31, filter = testfilter, signal = rep(0, 31), noise = 1, seed = "no")
  ret <- hilde(family = "jsmurfPS", data = testdata2, filter = testfilter2, q1 = testq, 
               method = "LR", lengths = 1:20, q2 = rep(25, 20),
               thresholdLongSegment = 11, suppressWarningNoDeconvolution = TRUE)
  expect_identical(attr(ret, "noDeconvolution"), 1L)
  
  
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
                     method = "LR", lengths = 1:20, q2 = rep(25, 20),
                     localValue = 1, suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
                     method = "LR", lengths = 1:20, q2 = rep(25, 20),
                     localValue = function(x) {Inf}, suppressWarningNoDeconvolution = TRUE))

  ret <- hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
               method = "LR", lengths = 1:20, q2 = rep(1e14, 20),
               localValue = mean, suppressWarningNoDeconvolution = TRUE)
  expect_identical(ret$value, mean(testdata[8:92]))
  
  
  testqH <- getCritVal(n = length(testdata), filter = testfilter, family = "hjsmurf", r = 2)
  expect_error(hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testqH, 
                     method = "2Param", lengths = 3, q2 = 200,
                     localVar = function() {1}, suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testqH, 
                     method = "2Param", lengths = 3, q2 = 200,
                     localVar = function(x) {-1}, suppressWarningNoDeconvolution = TRUE))
  
  expect_identical(hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testqH, 
                         method = "2Param", lengths = 3, q2 = 200,
                         localVar = function(x) {1}, suppressWarningNoDeconvolution = TRUE),
                   hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testqH, 
                         method = "2Param", lengths = 3, q2 = 200,
                         localVar = function(x, y = 1) {y}, suppressWarningNoDeconvolution = TRUE))
  
  
  expect_error(hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testqH, 
                     method = "2Param", lengths = 3, q2 = 200,
                     regularization = NULL, suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testqH, 
                     method = "2Param", lengths = 3, q2 = 200,
                     regularization = list(c(1, 0.6, 0.1), c(1, 0.5)),
                     suppressWarningNoDeconvolution = TRUE))
  
  
  expect_error(hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testqH, 
                     method = "2Param", lengths = 3, q2 = 200,
                     gridSize = c(1, as.numeric(NA), 0.01),
                     suppressWarningNoDeconvolution = TRUE))
  expect_warning(ret <- hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testqH, 
                              method = "2Param", lengths = 3, q2 = 200,
                              gridSize = c(0.5, 0.1, 0.01) / testfilter$sr,
                              suppressWarningNoDeconvolution = TRUE))
  
  
  expect_error(hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testqH, 
                     method = "2Param", lengths = 3, q2 = 200,
                     windowFactorRefinement = c(1, 1, 1),
                     suppressWarningNoDeconvolution = TRUE))
  
  
  expect_error(hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testqH, 
                     method = "2Param", lengths = 3, q2 = 200,
                     report = 1, suppressWarningNoDeconvolution = TRUE))

  
  expect_error(hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testqH, 
                     method = "2Param", lengths = 3, q2 = 200,
                     suppressWarningNoDeconvolution = c(TRUE, TRUE)))
  
 
  testlocalList <- createLocalList(filter = testfilter, method = "2Param", lengths = 3)
  expect_error(hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testqH, 
                     method = "2Param", lengths = 3, q2 = 200,
                     suppressWarningNoDeconvolution = TRUE,
                     localList = unclass(testlocalList)))
  expect_identical(hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testqH, 
                         method = "2Param", lengths = 3, q2 = 200,
                         suppressWarningNoDeconvolution = TRUE,
                         localList = testlocalList),
                   hilde(family = "hjsmurf", data = testdata, filter = testfilter, q1 = testqH, 
                         method = "2Param", lengths = 3, q2 = 200,
                         suppressWarningNoDeconvolution = TRUE))
 
  testfilter2 <- lowpassFilter::lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 50)
  expect_error(hilde(family = "hjsmurf", data = testdata, filter = testfilter2, q1 = testqH, 
                     method = "2Param", lengths = 3, q2 = 200,
                     suppressWarningNoDeconvolution = TRUE,
                     localList = testlocalList))
  
  
  testdata <- c(rnorm(100, 0), rnorm(5, 10), rnorm(100, 0))
  testdata <- .convolve(testdata, testfilter)
  testq <- getCritVal(n = length(testdata), filter = testfilter, family = "jsmurfPS")
  testlocalList <- createLocalList(filter = testfilter, method = "LR", lengths = c(1:19, 21))

  compare <- improveSmallScales(fit = jsmurf(family = "jsmurfPS", data = testdata, filter = testfilter, q = testq,
                                             locationCorrection = "none"),
                                data = testdata, filter = testfilter,
                                method = "LR", q = rep(25, 20),
                                lengths = c(1:19, 21), thresholdLongSegment = 15L,
                                localValue = function(x, y = x) {(mean(x) + median(y)) / 2},
                                regularization = list(c(1, 0.6, 0.1), c(1, 0.5), 1),
                                gridSize = c(1, 0.2, 0.02) / testfilter$sr,
                                windowFactorRefinement = 2,
                                suppressWarningNoDeconvolution = TRUE)
  attr(compare, "q") <- NULL
  expect_identical(suppressMessages(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, 
                                          method = "LR", q2 = rep(25, 20),
                                          lengths = c(1:19, 21), thresholdLongSegment = 15L,
                                          localValue = function(x, y = x) {(mean(x) + median(y)) / 2},
                                          regularization = list(c(1, 0.6, 0.1), c(1, 0.5), 1),
                                          gridSize = c(1, 0.2, 0.02) / testfilter$sr,
                                          windowFactorRefinement = 2, report = TRUE,
                                          localList = testlocalList,
                                          suppressWarningNoDeconvolution = FALSE)), compare)
  
  
  
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 0.1), sr = 1, len = 8L,
                              shift = 0.5)
  testq <- getCritVal(n = length(testdata), filter = testfilter, family = "jsmurfPS")
  teststat <- stepR::monteCarloSimulation(n = 215, r = 100, family = "jsmurfPS", filter = testfilter)
  
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, stat = rnorm(100),
                     method = "LR", lengths = 1:20, q2 = rep(25, 20),
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurfPS", data = testdata, stat = teststat,
                     filter = lowpassFilter(param = list(pole = 4L, cutoff = 0.2), len = 8),
                     method = "LR", lengths = 1:20, q2 = rep(25, 20),
                     suppressWarningNoDeconvolution = TRUE))
  
  
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                     r = 0, options = list(load = list()),
                     method = "LR", lengths = 1:20, q2 = rep(25, 20),
                     suppressWarningNoDeconvolution = TRUE))
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                         r = 10.5, options = list(load = list()),
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         suppressWarningNoDeconvolution = TRUE, output = "every"),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                         r = 10L, options = list(load = list()),
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         suppressWarningNoDeconvolution = TRUE, output = "every"))
  
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, nq = 150L,
                     method = "LR", lengths = 1:20, q2 = rep(25, 20),
                     suppressWarningNoDeconvolution = TRUE))
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                         nq = 300.5, r = 10L, options = list(load = list()),
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         suppressWarningNoDeconvolution = TRUE, output = "every"),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                         nq = 300L, r = 10L, options = list(load = list()),
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         suppressWarningNoDeconvolution = TRUE, output = "every"))
  
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                     method = "LR", lengths = 1:20, q2 = rep(25, 20),
                     options = list(save = list(workspace = "vecto")),
                     suppressWarningNoDeconvolution = TRUE))
  
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                     method = "LR", lengths = 1:20, q2 = rep(25, 20),
                     messages = -1, options = list(load = list()),
                     suppressWarningNoDeconvolution = TRUE))
  expect_identical(suppressMessages(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                                          method = "LR", lengths = 1:20, q2 = rep(25, 20),
                                          messages = 2.5, options = list(load = list()), r = 10,
                                          suppressWarningNoDeconvolution = TRUE, output = "every")),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         options = list(load = list()), r = 10,
                         suppressWarningNoDeconvolution = TRUE, output = "every"))
  

  
  
  testfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".RDS")
  testvariable <- "test"
  testStepR <- new.env()
  
  testfilter1 <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 8L)
  teststat1 <- stepR::monteCarloSimulation(n = 197L, r = 10L, family = "jsmurfPS",
                                           filter = testfilter1, output = "maximum")
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter1, r = 10L,
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         options = list(save = list(RDSfile = testfile, variable = testvariable,
                                                    workspace = c("vector", "vectorIncreased")),
                                        load = list(), envir = testStepR, dirs = "testStepR"),
                         suppressWarningNoDeconvolution = TRUE, output = "every"),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter1,
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         stat = teststat1, options = list(save = list()),
                         suppressWarningNoDeconvolution = TRUE, output = "every"))
  expect_identical(readRDS(testfile), teststat1)
  expect_identical(get("test", envir = testStepR), teststat1)
  remove(test, envir = testStepR)
  
  testfilter2 <- lowpassFilter(param = list(pole = 4L, cutoff = 0.2), len = 8L)
  teststat2 <- stepR::monteCarloSimulation(n = 197L, r = 10L, family = "jsmurfPS",
                                           filter = testfilter2, output = "maximum")
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter2, r = 10L,
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         options = list(envir = testStepR, dirs = "testStepR", 
                                        save = list(fileSystem = "vector", workspace = "vector")),
                         suppressWarningNoDeconvolution = TRUE, output = "every"),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter2,
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         options = list(save = list()), stat = teststat2,
                         suppressWarningNoDeconvolution = TRUE, output = "every"))
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter1, r = 20L,
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         options = list(load = list(RDSfile = testfile), save = list(fileSystem = "vector"),
                                        envir = testStepR, dirs = "testStepR"),
                         suppressWarningNoDeconvolution = TRUE, output = "every"),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter1,
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         options = list(save = list()), stat = teststat1,
                         suppressWarningNoDeconvolution = TRUE, output = "every"))
  unlink(testfile)
  
  testfilter3 <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11L)
  teststat3 <- stepR::monteCarloSimulation(n = 197, r = 10L, family = "jsmurfPS",
                                           filter = testfilter3, output = "maximum")
  expect_identical(getCritVal(family = "jsmurfPS", n = 190L, filter = testfilter3, r = 10L, nq = 197L,
                              options = list(save = list(workspace = "vector", fileSystem = "vectorIncreased"),
                                             envir = testStepR, dirs = "testStepR")),
                   getCritVal(family = "jsmurfPS", n = 190L, stat = teststat3, filter = testfilter3,
                              options = list(save = list())))
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter3, r = 5L, nq = 300L,
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         options = list(save = list(workspace = "vector", fileSystem = "vectorIncreased"),
                                        envir = testStepR, dirs = "testStepR"),
                         suppressWarningNoDeconvolution = TRUE, output = "every"),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter3,
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         options = list(save = list()), stat = teststat3,
                         suppressWarningNoDeconvolution = TRUE, output = "every"))
  
  teststat4 <- stepR::monteCarloSimulation(n = 250L, r = 10L, family = "jsmurfPS", lengths = 2^(4:7),
                                           filter = testfilter3, output = "maximum")
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter3, r = 10L, nq = 250L,
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         options = list(save = list(workspace = "vector", fileSystem = "vectorIncreased"),
                                        load = list(workspace = "vectorIncreased"),
                                        envir = testStepR, dirs = "testStepR"),
                         suppressWarningNoDeconvolution = TRUE, output = "every"),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter3,
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         options = list(save = list()), stat = teststat4,
                         suppressWarningNoDeconvolution = TRUE, output = "every"))
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter3, r = 10L, nq = 250L,
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         options = list(simulation = "vector",
                                        save = list(workspace = "vector", fileSystem = "vectorIncreased"),
                                        load = list(workspace = "vectorIncreased"),
                                        envir = testStepR, dirs = "testStepR"),
                         suppressWarningNoDeconvolution = TRUE, output = "every"),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter3,
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         options = list(save = list()), stat = teststat3,
                         suppressWarningNoDeconvolution = TRUE, output = "every"))
  
  teststat5 <- stepR::monteCarloSimulation(n = 250L, r = 20L, family = "jsmurfPS", lengths = 2^(4:7),
                                           filter = testfilter3, output = "maximum")
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter3, r = 20L, nq = 250L,
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         options = list(save = list(workspace = "vectorIncreased",
                                                    fileSystem = "vector"),
                                        load = list(workspace = "vectorIncreased",
                                                    fileSystem = "vectorIncreased"),
                                        envir = testStepR, dirs = "testStepR"),
                         suppressWarningNoDeconvolution = TRUE, output = "every"),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter3,
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         options = list(save = list()), stat = teststat5,
                         suppressWarningNoDeconvolution = TRUE, output = "every"))
  
  
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter3, r = 10L, nq = 250L,
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         options = list(save = list(workspace = "vectorIncreased",
                                                    fileSystem = "vector"),
                                        load = list(workspace = "vectorIncreased",
                                                    fileSystem = "vectorIncreased"),
                                        envir = testStepR, dirs = "testStepR"),
                         suppressWarningNoDeconvolution = TRUE, output = "every"),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter3,
                         method = "LR", lengths = 1:20, q2 = rep(25, 20),
                         options = list(save = list()), stat = teststat5,
                         suppressWarningNoDeconvolution = TRUE, output = "every"))

  expect_identical(length(get("critValStepRTab", envir = testStepR, inherits = FALSE)$stat), 4L)
  expect_identical(get("critValStepRTab", envir = testStepR, inherits = FALSE)$stat[[1]], teststat1)
  expect_identical(get("critValStepRTab", envir = testStepR, inherits = FALSE)$stat[[2]], teststat2)
  expect_identical(get("critValStepRTab", envir = testStepR, inherits = FALSE)$stat[[3]], teststat3)
  expect_identical(get("critValStepRTab", envir = testStepR, inherits = FALSE)$stat[[4]], teststat5)
  
  expect_identical(length(list.files(file.path(R.cache::getCacheRootPath(), "testStepR"))), 4L)
  expect_identical(R.cache::loadCache(attr(teststat2, "keyList"), dirs = "testStepR"), teststat2)
  expect_identical(R.cache::loadCache(attr(teststat1, "keyList"), dirs = "testStepR"), teststat1)
  expect_identical(R.cache::loadCache(attr(teststat3, "keyList"), dirs = "testStepR"), teststat3)
  expect_identical(R.cache::loadCache(attr(teststat4, "keyList"), dirs = "testStepR"), teststat4)
  
  remove(critValStepRTab, envir = testStepR)
  unlink(file.path(R.cache::getCacheRootPath(), "testStepR"), recursive = TRUE)
  

  testq <- getCritVal(n = length(testdata), filter = testfilter, family = "jsmurfPS")
  teststat <- stepR::monteCarloSimulation(n = length(testdata), r = 10L, family = "LR", filter = testfilter)
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                     method = "LR", lengths = 1:20, r = 0L, options = list(load = list()),
                     suppressWarningNoDeconvolution = TRUE))
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                         method = "LR", lengths = 1:20, r = 10, options = list(load = list()),
                         suppressWarningNoDeconvolution = TRUE, output = "everything"),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                         method = "LR", lengths = 1:20, r = 10L, options = list(load = list()),
                         suppressWarningNoDeconvolution = TRUE, output = "everything"))

  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                     method = "LR", lengths = 1:20, stat = teststat, penalty = c("sqrt", "log"),
                     suppressWarningNoDeconvolution = TRUE))
  compareq <- getCritVal(n = length(testdata), filter = testfilter, family = "LR", penalty = "sqrt",
                         stat = teststat)
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                         method = "LR", lengths = 1:20, stat = teststat, penalty = "sqrt",
                         suppressWarningNoDeconvolution = TRUE, output = "everything"),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                         method = "LR", lengths = 1:20, q2 = compareq,
                         suppressWarningNoDeconvolution = TRUE, output = "everything"))
  
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                     method = "LR", lengths = 1:20, stat = unclass(teststat),
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurfPS", data = testdata, q1 = testq,
                     filter = lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11),
                     method = "LR", lengths = 1:20, stat = teststat,
                     suppressWarningNoDeconvolution = TRUE))
  
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                     method = "LR", lengths = 1:20, stat = teststat, weights = rep(1 / 19, 19),
                     suppressWarningNoDeconvolution = TRUE))
  compareq <- getCritVal(n = length(testdata), filter = testfilter, family = "LR", weights = 20:1 / sum(20:1),
                         stat = teststat)
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                         method = "LR", lengths = 1:20, stat = teststat, weights = 20:1 / sum(20:1), 
                         suppressWarningNoDeconvolution = TRUE, output = "everything"),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                         method = "LR", lengths = 1:20, q2 = compareq,
                         suppressWarningNoDeconvolution = TRUE, output = "everything"))
  
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                     method = "LR", lengths = 1:20, stat = teststat,
                     options = list(load = list(workspace = "ma")),
                     suppressWarningNoDeconvolution = TRUE))
  
  expect_error(suppressWarnings(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                                      method = "LR", lengths = 1:20, options = list(load = list()), seed = "s",
                                      suppressWarningNoDeconvolution = TRUE)))
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, r = 10L,
                         method = "LR", lengths = 1:20, options = list(load = list()), seed = 10.5,
                         suppressWarningNoDeconvolution = TRUE, output = "everything"),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq, r = 10L,
                         method = "LR", lengths = 1:20, options = list(load = list()), seed = 10L,
                         suppressWarningNoDeconvolution = TRUE, output = "everything"))

  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                     method = "LR", lengths = 1:20, options = list(load = list()), r = 10L,
                     rand.gen = 1,
                     suppressWarningNoDeconvolution = TRUE))
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                     method = "LR", lengths = 1:20, options = list(load = list()), r = 10L,
                     rand.gen = function(data) data$n,
                     suppressWarningNoDeconvolution = TRUE))


  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                     method = "LR", lengths = 1:20, r = 10L,
                     messages = -1, options = list(load = list()),
                     suppressWarningNoDeconvolution = TRUE))
  expect_identical(suppressMessages(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                                          method = "LR", lengths = 1:20, r = 10L,
                                          messages = 1, options = list(load = list()),
                                          suppressWarningNoDeconvolution = TRUE, output = "everything")),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                         method = "LR", lengths = 1:20, r = 10L,
                         options = list(load = list()),
                         suppressWarningNoDeconvolution = TRUE, output = "everything"))
  
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                     method = "LR", lengths = 1:20, r = 10L, nq = 100L,
                     options = list(load = list()),
                     suppressWarningNoDeconvolution = TRUE))
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                         method = "LR", lengths = 1:20, r = 10L, nq = 300.746,
                         options = list(load = list()),
                         suppressWarningNoDeconvolution = TRUE, output = "everything"),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                         method = "LR", lengths = 1:20, r = 10L, nq = 300L,
                         options = list(load = list()),
                         suppressWarningNoDeconvolution = TRUE, output = "everything"))
  
  
  testStepR <- new.env()
  teststat <- stepR::monteCarloSimulation(length(testdata), r = 10L, family = "LR", filter = testfilter)
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                         method = "LR", lengths = 1:20, r = 10L,
                         options = list(simulation = "matrix", save = list(workspace = "matrix"),
                                        load = list(), envir = testStepR),
                         suppressWarningNoDeconvolution = TRUE, output = "everything"),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter, q1 = testq,
                         method = "LR", lengths = 1:20, stat = teststat,
                         options = list(save = list()),
                         suppressWarningNoDeconvolution = TRUE, output = "everything"))
  expect_identical(length(get("critValStepRTab", envir = testStepR, inherits = FALSE)$stat), 1L)
  expect_identical(get("critValStepRTab", envir = testStepR, inherits = FALSE)$stat[[1]], teststat)
  remove(critValStepRTab, envir = testStepR)
  
  # test multiple parameters at the same time
  teststat1 <- stepR::monteCarloSimulation(n = length(testdata), r = 10L, family = "jsmurfPS", filter = testfilter)
  testq1 <- getCritVal(n = length(testdata), filter = testfilter, family = "jsmurfPS", stat = teststat1)
  teststat2 <- stepR::monteCarloSimulation(length(testdata), r = 10L, family = "LR", filter = testfilter)
  testq2 <- getCritVal(n = length(testdata), filter = testfilter, family = "LR", stat = teststat2)
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                         method = "LR", lengths = 1:20, r = 10L,
                         options = list(load = list()),
                         suppressWarningNoDeconvolution = TRUE, output = "everything"),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                         method = "LR", lengths = 1:20, r = 10L,
                         q1 = testq1, q2 = testq2,
                         suppressWarningNoDeconvolution = TRUE, output = "everything"))
  
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                     method = "LR", lengths = 1:20, r = 10L, stat = teststat1,
                     options = list(load = list()),
                     suppressWarningNoDeconvolution = TRUE, output = "everything"))
  
  teststat1 <- stepR::monteCarloSimulation(n = 300, r = 10L, family = "jsmurfPS", filter = testfilter)
  teststat2 <- stepR::monteCarloSimulation(300, r = 10L, family = "LR", filter = testfilter)
  testq1 <- getCritVal(n = 300L, filter = testfilter, family = "jsmurfPS", stat = teststat1)
  testq2 <- getCritVal(n = 300L, filter = testfilter, family = "LR", stat = teststat2, penalty = "sqrt")
  expect_warning(ret <- hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                              method = "LR", lengths = 1:20, r = 10L,
                              options = list(simulation = "matrixIncreased", load = list()),
                              penalty = "sqrt", nq = 300L,
                              suppressWarningNoDeconvolution = TRUE, output = "everything"))
  expect_identical(ret,
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                         method = "LR", lengths = 1:20, r = 10L,
                         q1 = testq1, q2 = testq2,
                         suppressWarningNoDeconvolution = TRUE, output = "everything"))
  
  
  testq1 <- getCritVal(n = length(testdata), filter = testfilter, family = "jsmurfPS", r = 10L)
  testq2 <- getCritVal(n = length(testdata), filter = testfilter, family = "LR", r = 10L,
                       lengths = 1:10, thresholdLongSegment = 15L,
                       localValue = function(x, y = x) {(mean(x) + median(y)) / 2})
  testlocalList <- createLocalList(filter = testfilter, method = "LR", lengths = 1:10)
  expect_identical(suppressMessages(hilde(family = "jsmurfPS", data = testdata, filter = testfilter, r = 10L,
                                          method = "LR", lengths = 1:10, thresholdLongSegment = 15L,
                                          localValue = function(x, y = x) {(mean(x) + median(y)) / 2},
                                          regularization = list(c(1, 0.6, 0.1), c(1, 0.5), 1),
                                          gridSize = c(1, 0.2, 0.02) / testfilter$sr,
                                          windowFactorRefinement = 2, report = TRUE,
                                          localList = testlocalList,
                                          suppressWarningNoDeconvolution = FALSE)),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                         method = "LR", q1 = testq1, q2 = testq2,
                         lengths = 1:10, thresholdLongSegment = 15L,
                         localValue = function(x, y = x) {(mean(x) + median(y)) / 2},
                         regularization = list(c(1, 0.6, 0.1), c(1, 0.5), 1),
                         gridSize = c(1, 0.2, 0.02) / testfilter$sr,
                         windowFactorRefinement = 2,
                         localList = testlocalList,
                         suppressWarningNoDeconvolution = TRUE))
  
  testStepR <- new.env()
  teststat1 <- stepR::monteCarloSimulation(n = length(testdata), r = 10L, family = "jsmurfPS", filter = testfilter)
  teststat2 <- stepR::monteCarloSimulation(length(testdata), r = 10L, family = "LR", filter = testfilter)
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                         method = "LR", lengths = 1:20, r = 10L,
                         options = list(save = list(workspace = c("matrix", "matrixIncreased"),
                                                    fileSystem = c("matrix", "matrixIncreased")),
                                        load = list(), envir = testStepR, dirs = "testStepR"),
                         suppressWarningNoDeconvolution = TRUE, output = "everything"),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                         method = "LR", lengths = 1:20, r = 10L,
                         options = list(load = list(), save = list()),
                         suppressWarningNoDeconvolution = TRUE, output = "everything"))

  expect_identical(length(get("critValStepRTab", envir = testStepR, inherits = FALSE)$stat), 2L)
  expect_identical(get("critValStepRTab", envir = testStepR, inherits = FALSE)$stat[[1]], teststat1)
  expect_identical(get("critValStepRTab", envir = testStepR, inherits = FALSE)$stat[[2]], teststat2)
  
  expect_identical(length(list.files(file.path(R.cache::getCacheRootPath(), "testStepR"))), 2L)
  expect_identical(R.cache::loadCache(attr(teststat1, "keyList"), dirs = "testStepR"), teststat1)
  expect_identical(R.cache::loadCache(attr(teststat2, "keyList"), dirs = "testStepR"), teststat2)
  
  remove(critValStepRTab, envir = testStepR)
  unlink(file.path(R.cache::getCacheRootPath(), "testStepR"), recursive = TRUE)

  
  testStepR <- new.env()
  teststat1 <- stepR::monteCarloSimulation(n = length(testdata), r = 10L, family = "jsmurfPS", filter = testfilter)
  teststat2 <- stepR::monteCarloSimulation(length(testdata), r = 10L, family = "LR", filter = testfilter,
                                           localValue = mean)
  expect_identical(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                         method = "LR", lengths = 1:20, r = 10L,
                         options = list(save = list(workspace = c("matrix", "matrixIncreased"),
                                                    fileSystem = c("matrix", "matrixIncreased")),
                                        load = list(), envir = testStepR, dirs = "testStepR"),
                         localValue = mean,
                         suppressWarningNoDeconvolution = TRUE, output = "everything"),
                   hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                         method = "LR", lengths = 1:20, r = 10L,
                         options = list(load = list(), save = list()),
                         localValue = mean,
                         suppressWarningNoDeconvolution = TRUE, output = "everything"))
  
  expect_identical(length(get("critValStepRTab", envir = testStepR, inherits = FALSE)$stat), 1L)
  expect_identical(get("critValStepRTab", envir = testStepR, inherits = FALSE)$stat[[1]], teststat1)

  expect_identical(length(list.files(file.path(R.cache::getCacheRootPath(), "testStepR"))), 1L)
  expect_identical(R.cache::loadCache(attr(teststat1, "keyList"), dirs = "testStepR"), teststat1)
  
  remove(critValStepRTab, envir = testStepR)
  unlink(file.path(R.cache::getCacheRootPath(), "testStepR"), recursive = TRUE)
  

  ret <- hilde(data = testdata, filter = testfilter, q1 = testq, family = "jsmurfPS", 
               method = "LR", lengths = c(1:10, 21), r = 10L, output = "everything",
               options = list(load = list()), suppressWarningNoDeconvolution = TRUE)
  expect_identical(length(ret$q2), 11L)
  expect_identical(attr(ret$q2, "n"), 197L)
  
  teststat <- stepR::monteCarloSimulation(n = 197L, r = 10L, family = "LR", filter = testfilter)
  expect_error(hilde(data = testdata, filter = testfilter, q1 = testq, family = "jsmurfPS", 
                     method = "LR", lengths = c(1:10, 21), stat = teststat, output = "everything",
                     options = list(load = list()), suppressWarningNoDeconvolution = TRUE))

  teststat <- stepR::monteCarloSimulation(n = 197L, r = 10L, family = "LR", filter = testfilter, lengths = c(5, 21:23),
                                          output = "maximum", penalty = "sqrt")
  expect_identical(hilde(data = testdata, filter = testfilter, q1 = testq, family = "jsmurfPS", penalty = "sqrt",
                         method = "LR", lengths = c(5, 21:23), stat = teststat, output = "everything",
                         options = list(load = list()), suppressWarningNoDeconvolution = TRUE),
                   hilde(data = testdata, filter = testfilter, q1 = testq, family = "jsmurfPS", penalty = "sqrt",
                         method = "LR", lengths = c(5, 21:23), r = 10L, output = "everything",
                         options = list(load = list()), suppressWarningNoDeconvolution = TRUE))
  
  expect_identical(hilde(data = testdata, filter = testfilter, q1 = testq, family = "jsmurfPS", penalty = "sqrt",
                         method = "LR", lengths = c(5, 21:23), output = "everything", r = 10L,
                         options = list(simulation = "matrix",
                                        save = list(fileSystem = c("matrix", "vector",
                                                                   "matrixIncreased", "vectorIncreased")),
                                        dirs = "testStepR"),
                         suppressWarningNoDeconvolution = TRUE),
                   hilde(data = testdata, filter = testfilter, q1 = testq, family = "jsmurfPS", penalty = "sqrt",
                         method = "LR", lengths = c(5, 21:23), output = "everything", stat = teststat,
                         options = list(simulation = "matrix",
                                        save = list(fileSystem = c("matrix", "vector",
                                                                   "matrixIncreased", "vectorIncreased")),
                                        dirs = "testStepR"),
                         suppressWarningNoDeconvolution = TRUE))
  expect_false(file_test(op = "-d", file.path(R.cache::getCacheRootPath(), "testStepR")))
  

  testq1 <- getCritVal(n = length(testdata), filter = testfilter, family = "jsmurfPS", r = 10L)
  testq2 <- getCritVal(n = length(testdata), filter = testfilter, family = "LR", r = 10L)
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                     method = "LR", lengths = 1:20, q1 = testq1, q2 = testq2,
                     suppressWarningNoDeconvolution = TRUE, output = "everything",
                     q = 1))
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                     method = "LR", lengths = 1:20, q1 = testq1, q2 = testq2,
                     suppressWarningNoDeconvolution = TRUE, output = "everything",
                     alpha = 0.05))
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                     method = "LR", lengths = 1:20, q1 = testq1, q2 = testq2,
                     suppressWarningNoDeconvolution = TRUE, output = "everything",
                     intervalSystem = "all"))
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                     method = "LR", lengths = 1:20, q1 = testq1, q2 = testq2,
                     suppressWarningNoDeconvolution = TRUE, output = "everything",
                     n = 215))
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                     method = "LR", lengths = 1:20, q1 = testq1, q2 = testq2,
                     suppressWarningNoDeconvolution = TRUE, output = "everything",
                     correlations = 1))
  expect_error(hilde(family = "jsmurfPS", data = testdata, filter = testfilter,
                     method = "LR", lengths = 1:20, q1 = testq1, q2 = testq2,
                     suppressWarningNoDeconvolution = TRUE, output = "everything",
                     fit = 1))
})
