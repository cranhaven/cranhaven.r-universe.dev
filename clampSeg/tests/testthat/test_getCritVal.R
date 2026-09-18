
context("getCritVal")

test <- function(testalpha, testn, testnq, teststat, testfilter) {
  ret <- getCritVal(alpha = testalpha, n = testn, nq = testnq, stat = teststat, filter = testfilter) 
  
  expect_identical(ret, stepR::critVal(alpha = testalpha, n = testn, nq = testnq, family = "mDependentPS",
                                       intervalSystem = "dyaLen", lengths = 2^(0:(as.integer(log2(testn) + 1e-6))),
                                       penalty = "sqrt", stat = teststat, output = "value",
                                       filter = testfilter))
  
  expect_identical(stepR::critVal(q = ret, alpha = testalpha, n = testn, nq = testnq, family = "mDependentPS",
                                  intervalSystem = "dyaLen", lengths = 2^(0:(as.integer(log2(testn) + 1e-6))),
                                  penalty = "sqrt", stat = teststat, output = "value", filter = testfilter),
                   stepR::critVal(alpha = testalpha, n = testn, nq = testnq, family = "mDependentPS",
                                  intervalSystem = "dyaLen", lengths = 2^(0:(as.integer(log2(testn) + 1e-6))),
                                  penalty = "sqrt", stat = teststat, output = "value", filter = testfilter))
}

test_that("n is tested and works for default parameters and given stat", {
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 30, r = 100, family = "mDependentPS",
                                          filter = testfilter, output = "maximum")
  
  expect_error(getCritVal())
  expect_error(getCritVal(stat = teststat))
  expect_error(getCritVal(stat = teststat, filter = testfilter))
  expect_error(getCritVal(n = "s", stat = teststat, filter = testfilter))
  expect_error(getCritVal(n = c(1L, 2L), stat = teststat, filter = testfilter))
  expect_error(getCritVal(n = as.integer(NA), stat = teststat, filter = testfilter))
  expect_error(getCritVal(n = NULL, stat = teststat, filter = testfilter))
  expect_error(getCritVal(n = Inf, stat = teststat, filter = testfilter))
  expect_error(getCritVal(n = 0L, stat = teststat, filter = testfilter))
  expect_error(getCritVal(n = -1L, stat = teststat, filter = testfilter))
  
  expect_identical(getCritVal(n = 30L, stat = teststat, filter = testfilter),
                   getCritVal(n = 30L, stat = teststat, filter = testfilter, nq = 30L, alpha = 0.05))
  expect_identical(getCritVal(n = 30, stat = teststat, filter = testfilter),
                   getCritVal(n = 30L, stat = teststat, filter = testfilter))
  expect_identical(getCritVal(n = 30.5, stat = teststat, filter = testfilter),
                   getCritVal(n = 30L, stat = teststat, filter = testfilter))
  
  test(testalpha = 0.05, testn = 30L, testnq = 30L, teststat = teststat, testfilter = testfilter)
})

test_that("alpha is tested and works for given stat", {
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 30, r = 100, family = "mDependentPS",
                                          filter = testfilter, output = "maximum")
  
  expect_error(getCritVal(n = 30L, alpha = "s", stat = teststat, filter = testfilter))
  expect_error(getCritVal(n = 30L, alpha = NULL, stat = teststat, filter = testfilter))
  expect_error(getCritVal(n = 30L, alpha = NA, stat = teststat, filter = testfilter))
  expect_error(getCritVal(n = 30L, alpha = Inf, stat = teststat, filter = testfilter))
  expect_error(getCritVal(n = 30L, alpha = c(0.1, 0.05), stat = teststat, filter = testfilter))
  expect_error(getCritVal(n = 30L, alpha = 0, stat = teststat, filter = testfilter))
  expect_error(getCritVal(n = 30L, alpha = 1, stat = teststat, filter = testfilter))
  expect_error(getCritVal(n = 30L, alpha = -0.05, stat = teststat, filter = testfilter))
  expect_error(getCritVal(n = 30L, alpha = 1.1, stat = teststat, filter = testfilter))
  
  expect_identical(getCritVal(n = 30L, stat = teststat, filter = testfilter),
                   getCritVal(n = 30L, alpha = 0.05, stat = teststat, filter = testfilter))
  
  test(testalpha = 0.02, testn = 30L, testnq = 30L, teststat = teststat, testfilter = testfilter)
  test(testalpha = 0.335, testn = 30L, testnq = 30L, teststat = teststat, testfilter = testfilter)
})

test_that("filter is tested and works for given stat", {
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 30, r = 100, family = "mDependentPS",
                                          filter = testfilter, output = "maximum")
  
  expect_error(getCritVal(n = 30L, stat = teststat, filter = unclass(testfilter)))
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 8)
  teststat <- stepR::monteCarloSimulation(n = 45, r = 100, family = "mDependentPS",
                                          filter = testfilter, output = "maximum")
  test(testalpha = 0.1223, testn = 45L, testnq = 45L, teststat = teststat, testfilter = testfilter)
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.05), len = 14)
  teststat <- stepR::monteCarloSimulation(n = 23, r = 100, family = "mDependentPS",
                                          filter = testfilter, output = "maximum")
  test(testalpha = 0.01, testn = 23L, testnq = 23L, teststat = teststat, testfilter = testfilter)
  
  testfilter <- lowpassFilter(param = list(pole = 6L, cutoff = 0.25), len = 5)
  teststat <- stepR::monteCarloSimulation(n = 45, r = 100, family = "mDependentPS",
                                          filter = testfilter, output = "maximum")
  test(testalpha = 0.3, testn = 45L, testnq = 45L, teststat = teststat, testfilter = testfilter)
})

test_that("stat is tested and works", {
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  
  teststat <- matrix(runif(1e2 * 5), 5, 1e2)
  expect_error(getCritVal(n = 30L, stat = teststat, filter = testfilter))
  
  teststat <- rnorm(100)
  expect_error(getCritVal(n = 30L, stat = teststat, filter = testfilter))
  
  teststat <- stepR::monteCarloSimulation(n = 30, r = 100, family = "mDependentPS",
                                          filter = testfilter, output = "maximum")
  testmatrix <- stepR::monteCarloSimulation(n = 30, r = 100, family = "mDependentPS", filter = testfilter)
  expect_identical(getCritVal(n = 30L, stat = testmatrix, filter = testfilter),
                   getCritVal(n = 30L, stat = teststat, filter = testfilter))
  
  expect_error(getCritVal(n = 32L, stat = teststat, filter = testfilter))
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 10)
  expect_error(getCritVal(n = 30L, stat = teststat, filter = testfilter))
  
  testfilter <- lowpassFilter(param = list(pole = 6L, cutoff = 0.1), len = 11)
  expect_error(getCritVal(n = 30L, stat = teststat, filter = testfilter))
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.15), len = 11)
  expect_error(getCritVal(n = 30L, stat = teststat, filter = testfilter))
})

test_that("r is tested and works", {
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  
  expect_error(getCritVal(n = 30L, filter = testfilter, r = "s", options = list(load = list())))
  expect_error(getCritVal(n = 30L, filter = testfilter, r = 0L, options = list(load = list())))
  expect_error(getCritVal(n = 30L, filter = testfilter, r = c(100L, 200L), options = list(load = list())))
  
  expect_identical(getCritVal(n = 30L, filter = testfilter, r = 100.5, options = list(load = list())),
                   getCritVal(n = 30L, filter = testfilter, r = 100L, options = list(load = list())))
  
  teststat <- stepR::monteCarloSimulation(n = 30, family = "mDependentPS",
                                          filter = testfilter, output = "maximum")
  
  expect_identical(getCritVal(n = 30L, filter = testfilter, options = list(load = list())),
                   getCritVal(n = 30L, filter = testfilter, stat = teststat))
})

test_that("nq is tested and works for given stat", {
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 30, r = 100, family = "mDependentPS",
                                          filter = testfilter, output = "maximum")
  
  expect_error(getCritVal(n = 30L, nq = "s", stat = teststat,
                          filter = testfilter, options = list(load = list())))
  expect_error(getCritVal(n = 30L, nq = c(1L, 2L), stat = teststat,
                          filter = testfilter, options = list(load = list())))
  expect_error(getCritVal(n = 30L, nq = NA, stat = teststat,
                          filter = testfilter, options = list(load = list())))
  expect_error(getCritVal(n = 30L, nq = Inf, stat = teststat,
                          filter = testfilter, options = list(load = list())))
  expect_error(getCritVal(n = 30L, nq = NULL, stat = teststat,
                          filter = testfilter, options = list(load = list())))
  expect_error(getCritVal(n = 30L, nq = 8L, stat = teststat,
                          filter = testfilter, options = list(load = list())))
  
  expect_identical(getCritVal(n = 30L, filter = testfilter, r = 100, options = list(load = list())),
                   getCritVal(n = 30L, nq = 30L, filter = testfilter, r = 100, options = list(load = list())))
  expect_identical(getCritVal(n = 30L, nq = 100, filter = testfilter, r = 100, options = list(load = list())),
                   getCritVal(n = 30L, nq = 100L, filter = testfilter, r = 100, options = list(load = list())))
  expect_identical(getCritVal(n = 30L, nq = 30.5, filter = testfilter, r = 100, options = list(load = list())),
                   getCritVal(n = 30L, nq = 30L, filter = testfilter, r = 100, options = list(load = list())))
})

test_that("options is tested", {
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  
  expect_warning(getCritVal(n = 30L, filter = testfilter, simulation = "vector"))
  expect_error(getCritVal(n = 30L, filter = testfilter, options = "vector"))
  expect_error(getCritVal(n = 30L, filter = testfilter, options = list(a = "vector")))
  
  expect_error(getCritVal(n = 30L, filter = testfilter,
                          options = list(simulation = c("vector", "matrix"), save = list(), load = list())))
  expect_error(getCritVal(n = 30L, filter = testfilter,
                          options = list(simulation = "vecto", save = list(), load = list())))
  
  expect_error(getCritVal(n = 30L, filter = testfilter, options = list(load = "test")))
  expect_error(getCritVal(n = 30L, filter = testfilter, options = list(load = list(test = "test"))))
  
  testfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".RDS")
  expect_error(getCritVal(n = 30L, filter = testfilter, options = list(load = list(RDSfile = rep(testfile, 2)))))
  
  
  expect_error(getCritVal(n = 30L, filter = testfilter, options = list(save = "test")))
  expect_error(getCritVal(n = 30L, filter = testfilter, options = list(save = list(test = "test"))))
  
  testfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".RDS")
  expect_error(getCritVal(n = 30L, filter = testfilter, options = list(save = list(RDSfile = rep(testfile, 3)))))
  
  testvariable <- "test"
  testStepR <- new.env()
  
  expect_error(getCritVal(n = 30L, filter = testfilter,
                          options = list(save = list(variable = 10), envir = testStepR)))
  expect_error(getCritVal(n = 30L, filter = testfilter,
                          options = list(save = list(variable = rep(testvariable, 3)), envir = testStepR)))
  
  expect_error(getCritVal(n = 30L, filter = testfilter,
                          options = list(save = list(variable = testvariable), envir = "testStepR")))
  expect_error(getCritVal(n = 30L, filter = testfilter,
                          options = list(save = list(variable = testvariable), envir = c(testStepR, .GlobalEnv))))
  
  expect_error(getCritVal(n = 30L, filter = testfilter,
                          options = list(save = list(workspace = "vecto"), envir = testStepR)))
  expect_error(getCritVal(n = 30L, filter = testfilter,
                          options = list(save = list(workspace = c("vector", "vecto")), envir = testStepR)))
  
  expect_error(getCritVal(n = 30L, filter = testfilter,
                          options = list(load = list(workspace = "vecto"), envir = testStepR)))
  expect_error(getCritVal(n = 30L, filter = testfilter,
                          options = list(load = list(workspace = c("vector", "vecto")), envir = testStepR)))
  
  expect_error(getCritVal(n = 30L, filter = testfilter,
                          options = list(save = list(fileSystem = "vecto"), dirs = "testStepR")))
  expect_error(getCritVal(n = 30L, filter = testfilter,
                          options = list(save = list(fileSystem = c("vector", "vecto")), dirs = "testStepR")))
  
  expect_error(getCritVal(n = 30L, filter = testfilter,
                          options = list(load = list(fileSystem = "vecto"), dirs = "testStepR")))
  expect_error(getCritVal(n = 30L, filter = testfilter,
                          options = list(load = list(fileSystem = c("vector", "vecto")), dirs = "testStepR")))
  
  expect_error(getCritVal(n = 30L, filter = testfilter,
                          options = list(save = list(fileSystem = "vector"), dirs = c("testStepR", "test"))))
  expect_error(getCritVal(n = 30L, filter = testfilter,
                          options = list(save = list(fileSystem = "vector"), dirs = 10)))
})

test_that("stat is simulated correctly", {
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 30, r = 100, family = "mDependentPS",
                                          filter = testfilter, output = "maximum")
  
  expect_identical(getCritVal(n = 30L, stat = teststat, filter = testfilter),
                   getCritVal(n = 30L, filter = testfilter, r = 100, options = list(load = list())))
  expect_identical(getCritVal(n = 30L, stat = teststat, filter = testfilter),
                   getCritVal(n = 30L, filter = testfilter, r = 100, nq = 35L,
                              options = list(simulation = "vector", load = list())))
  expect_identical(getCritVal(n = 30L, stat = teststat, filter = testfilter),
                   getCritVal(n = 30L, filter = testfilter, r = 100,
                              options = list(simulation = "vectorIncreased", load = list())))
  expect_identical(getCritVal(n = 30L, stat = teststat, filter = testfilter),
                   getCritVal(n = 30L, filter = testfilter, r = 100,
                              options = list(simulation = "matrix", load = list())))
  expect_identical(getCritVal(n = 28L, stat = teststat, filter = testfilter),
                   getCritVal(n = 28L, filter = testfilter, r = 100, nq = 30L,
                              options = list(simulation = "vectorIncreased", load = list())))
})

test_that("stat is saved and loaded correctly", {
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 30, r = 100, family = "mDependentPS",
                                          filter = testfilter, output = "maximum")
  
  testfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".RDS")
  expect_identical(getCritVal(n = 30L, filter = testfilter, r = 100L,
                              options = list(save = list(RDSfile = testfile), load = list())),
                   getCritVal(n = 30L, stat = teststat, filter = testfilter, options = list(save = list())))
  expect_identical(readRDS(testfile), teststat)
  expect_identical(getCritVal(n = 30L, filter = testfilter, r = 50L,
                              options = list(save = list(), load = list(RDSfile = testfile))),
                   getCritVal(n = 30L, stat = teststat, filter = testfilter, options = list(save = list())))
  unlink(testfile)
  
  testvariable <- "test"
  testStepR <- new.env()
  expect_identical(getCritVal(n = 30L, filter = testfilter, r = 100L,
                              options = list(save = list(variable = testvariable), envir = testStepR,
                                             load = list())),
                   getCritVal(n = 30L, stat = teststat, filter = testfilter, options = list(save = list())))
  expect_identical(get("test", envir = testStepR), teststat)
  remove(test, envir = testStepR)
  
  expect_identical(getCritVal(n = 30L, filter = testfilter, r = 100L,
                              options = list(save = list(workspace = "vector"), envir = testStepR,
                                             load = list())),
                   getCritVal(n = 30L, stat = teststat, filter = testfilter, options = list(save = list())))
  expect_identical(get("critValStepRTab", envir = testStepR, inherits = FALSE)$stat[[1]], teststat)
  expect_identical(length(get("critValStepRTab", envir = testStepR, inherits = FALSE)$stat), 1L)
  expect_identical(getCritVal(n = 30L, filter = testfilter, r = 50L,
                              options = list(load = list(workspace = "vector"), envir = testStepR,
                                             save = list())),
                   getCritVal(n = 30L, stat = teststat, filter = testfilter, options = list(save = list())))
  expect_identical(getCritVal(n = 30L, filter = testfilter, r = 50L,
                              options = list(load = list(workspace = "vectorIncreased"), envir = testStepR,
                                             save = list())),
                   getCritVal(n = 30L, stat = teststat, filter = testfilter, options = list(save = list())))
  remove(critValStepRTab, envir = testStepR)
  
  expect_identical(getCritVal(n = 28L, filter = testfilter, r = 100L, nq = 30L,
                              options = list(save = list(workspace = "vectorIncreased"), envir = testStepR,
                                             load = list())),
                   getCritVal(n = 28L, stat = teststat, filter = testfilter, options = list(save = list())))
  expect_identical(get("critValStepRTab", envir = testStepR, inherits = FALSE)$stat[[1]], teststat)
  expect_identical(length(get("critValStepRTab", envir = testStepR, inherits = FALSE)$stat), 1L)
  expect_identical(getCritVal(n = 28L, filter = testfilter, r = 50L, nq = 30L,
                              options = list(load = list(workspace = "vectorIncreased"), envir = testStepR,
                                             save = list())),
                   getCritVal(n = 28L, stat = teststat, filter = testfilter, options = list(save = list())))
  remove(critValStepRTab, envir = testStepR)
  
  
  expect_identical(getCritVal(n = 30L, filter = testfilter, r = 100L,
                              options = list(save = list(workspace = "vector"), envir = testStepR,
                                             load = list())),
                   getCritVal(n = 30L, stat = teststat, filter = testfilter, options = list(save = list())))
  teststat200 <- stepR::monteCarloSimulation(n = 30, r = 200, family = "mDependentPS",
                                             filter = testfilter, output = "maximum")
  expect_identical(getCritVal(n = 30L, filter = testfilter, r = 200L,
                              options = list(save = list(workspace = "vector"), envir = testStepR,
                                             load = list("vector"))),
                   getCritVal(n = 30L, stat = teststat200, filter = testfilter, options = list(save = list())))
  expect_identical(getCritVal(n = 30L, filter = testfilter, r = 100L,
                              options = list(save = list(workspace = "vector"), envir = testStepR,
                                             load = list())),
                   getCritVal(n = 30L, stat = teststat, filter = testfilter, options = list(save = list())))
  expect_identical(get("critValStepRTab", envir = testStepR, inherits = FALSE)$stat[[1]], teststat200)
  expect_identical(length(get("critValStepRTab", envir = testStepR, inherits = FALSE)$stat), 1L)
  
  expect_identical(getCritVal(n = 30L, filter = testfilter, r = 50L,
                              options = list(load = list(workspace = "vector"), envir = testStepR,
                                             save = list())),
                   getCritVal(n = 30L, stat = teststat200, filter = testfilter, options = list(save = list())))
  remove(critValStepRTab, envir = testStepR)
  
  
  expect_identical(getCritVal(n = 30L, filter = testfilter, r = 100L,
                              options = list(save = list(fileSystem = "vector"), dirs = "testStepR",
                                             load = list())),
                   getCritVal(n = 30L, stat = teststat, filter = testfilter, options = list(save = list())))
  expect_identical(R.cache::loadCache(attr(teststat, "keyList"), dirs = "testStepR"), teststat)
  expect_identical(length(list.files(file.path(R.cache::getCacheRootPath(), "testStepR"))), 1L)
  expect_identical(getCritVal(n = 30L, filter = testfilter, r = 50L,
                              options = list(load = list(fileSystem = "vector"), dirs = "testStepR",
                                             save = list())),
                   getCritVal(n = 30L, stat = teststat, filter = testfilter, options = list(save = list())))
  expect_identical(getCritVal(n = 30L, filter = testfilter, r = 50L,
                              options = list(load = list(fileSystem = "vectorIncreased"), dirs = "testStepR",
                                             save = list())),
                   getCritVal(n = 30L, stat = teststat, filter = testfilter, options = list(save = list())))
  unlink(file.path(R.cache::getCacheRootPath(), "testStepR"), recursive = TRUE)
  
  expect_identical(getCritVal(n = 28L, filter = testfilter, r = 100L, nq = 30L,
                              options = list(save = list(fileSystem = "vectorIncreased"), dirs = "testStepR",
                                             load = list())),
                   getCritVal(n = 28L, stat = teststat, filter = testfilter, options = list(save = list())))
  expect_identical(R.cache::loadCache(attr(teststat, "keyList"), dirs = "testStepR"), teststat)
  expect_identical(length(list.files(file.path(R.cache::getCacheRootPath(), "testStepR"))), 1L)
  expect_identical(getCritVal(n = 28L, filter = testfilter, r = 50L, nq = 30L,
                              options = list(load = list(fileSystem = "vectorIncreased"), dirs = "testStepR",
                                             save = list())),
                   getCritVal(n = 28L, stat = teststat, filter = testfilter, options = list(save = list())))
  unlink(file.path(R.cache::getCacheRootPath(), "testStepR"), recursive = TRUE)
  
  
  expect_identical(getCritVal(n = 30L, filter = testfilter, r = 100L,
                              options = list(save = list(fileSystem = "vector"), dirs = "testStepR",
                                             load = list())),
                   getCritVal(n = 30L, stat = teststat, filter = testfilter, options = list(save = list())))
  teststat200 <- stepR::monteCarloSimulation(n = 30, r = 200, family = "mDependentPS",
                                             filter = testfilter, output = "maximum")
  expect_identical(getCritVal(n = 30L, filter = testfilter, r = 200L,
                              options = list(save = list(fileSystem = "vector"), dirs = "testStepR",
                                             load = list("vector"))),
                   getCritVal(n = 30L, stat = teststat200, filter = testfilter, options = list(save = list())))
  expect_identical(getCritVal(n = 30L, filter = testfilter, r = 100L,
                              options = list(save = list(fileSystem = "vector"), dirs = "testStepR",
                                             load = list())),
                   getCritVal(n = 30L, stat = teststat, filter = testfilter, options = list(save = list())))
  expect_identical(R.cache::loadCache(attr(teststat, "keyList"), dirs = "testStepR"), teststat200)
  expect_identical(length(list.files(file.path(R.cache::getCacheRootPath(), "testStepR"))), 1L)
  
  expect_identical(getCritVal(n = 30L, filter = testfilter, r = 50L,
                              options = list(load = list(fileSystem = "vector"), dirs = "testStepR",
                                             save = list())),
                   getCritVal(n = 30L, stat = teststat200, filter = testfilter, options = list(save = list())))
  unlink(file.path(R.cache::getCacheRootPath(), "testStepR"), recursive = TRUE)
  
  
  testfilter1 <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 8)
  teststat1 <- stepR::monteCarloSimulation(n = 30, r = 100, family = "mDependentPS",
                                           filter = testfilter1, output = "maximum")
  expect_identical(getCritVal(n = 30L, filter = testfilter1, r = 100L,
                              options = list(save = list(RDSfile = testfile, variable = testvariable,
                                                         workspace = c("vector", "vectorIncreased")),
                                             load = list(), envir = testStepR, dirs = "testStepR")),
                   getCritVal(n = 30L, stat = teststat1, filter = testfilter1, options = list(save = list())))
  expect_identical(readRDS(testfile), teststat1)
  expect_identical(get("test", envir = testStepR), teststat1)
  remove(test, envir = testStepR)
  
  testfilter2 <- lowpassFilter(param = list(pole = 4L, cutoff = 0.2), len = 8)
  teststat2 <- stepR::monteCarloSimulation(n = 30, r = 100, family = "mDependentPS",
                                           filter = testfilter2, output = "maximum")
  expect_identical(getCritVal(n = 30L, filter = testfilter2, r = 100L,
                              options = list(envir = testStepR, dirs = "testStepR", 
                                             save = list(fileSystem = "vector", workspace = "vector"))),
                   getCritVal(n = 30L, stat = teststat2, filter = testfilter2, options = list(save = list())))
  
  expect_identical(getCritVal(n = 30L, filter = testfilter1, r = 200L,
                              options = list(load = list(RDSfile = testfile), save = list(fileSystem = "vector"),
                                             envir = testStepR, dirs = "testStepR")),
                   getCritVal(n = 30L, stat = teststat1, filter = testfilter1, options = list(save = list())))
  unlink(testfile)
  
  testfilter3 <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat3 <- stepR::monteCarloSimulation(n = 30, r = 100, family = "mDependentPS",
                                           filter = testfilter3, output = "maximum")
  expect_identical(getCritVal(n = 28L, filter = testfilter3, r = 100L, nq = 30L,
                              options = list(save = list(workspace = "vector", fileSystem = "vectorIncreased"),
                                             envir = testStepR, dirs = "testStepR")),
                   getCritVal(n = 28L, stat = teststat3, filter = testfilter3, options = list(save = list())))
  
  expect_identical(getCritVal(n = 30L, filter = testfilter3, r = 50L, nq = 32L,
                              options = list(save = list(workspace = "vector", fileSystem = "vectorIncreased"),
                                             envir = testStepR, dirs = "testStepR")),
                   getCritVal(n = 30L, stat = teststat3, filter = testfilter3, options = list(save = list())))
  
  teststat4 <- stepR::monteCarloSimulation(n = 32, r = 100, family = "mDependentPS", lengths = 2^(0:4),
                                           filter = testfilter3, output = "maximum")
  expect_identical(getCritVal(n = 30L, filter = testfilter3, r = 100L, nq = 32L,
                              options = list(save = list(workspace = "vector", fileSystem = "vectorIncreased"),
                                             load = list(workspace = "vectorIncreased"),
                                             envir = testStepR, dirs = "testStepR")),
                   getCritVal(n = 30L, stat = teststat4, filter = testfilter3, options = list(save = list())))
  
  expect_identical(getCritVal(n = 30L, filter = testfilter3, r = 100L, nq = 32L,
                              options = list(simulation = "vector",
                                             save = list(workspace = "vector", fileSystem = "vectorIncreased"),
                                             load = list(workspace = "vectorIncreased"),
                                             envir = testStepR, dirs = "testStepR")),
                   getCritVal(n = 30L, stat = teststat3, filter = testfilter3, options = list(save = list())))
  
  teststat5 <- stepR::monteCarloSimulation(n = 32, r = 200, family = "mDependentPS", lengths = 2^(0:4),
                                           filter = testfilter3, output = "maximum")
  expect_identical(getCritVal(n = 30L, filter = testfilter3, r = 200L, nq = 32L,
                              options = list(save = list(workspace = "vectorIncreased",
                                                         fileSystem = "vector"),
                                             load = list(workspace = "vectorIncreased",
                                                         fileSystem = "vectorIncreased"),
                                             envir = testStepR, dirs = "testStepR")),
                   getCritVal(n = 30L, stat = teststat5, filter = testfilter3, options = list(save = list())))
  
  expect_identical(getCritVal(n = 30L, filter = testfilter3, r = 100L, nq = 32L,
                              options = list(save = list(workspace = "vectorIncreased",
                                                         fileSystem = "vector"),
                                             load = list(workspace = "vectorIncreased",
                                                         fileSystem = "vectorIncreased"),
                                             envir = testStepR, dirs = "testStepR")),
                   getCritVal(n = 30L, stat = teststat5, filter = testfilter3, options = list(save = list())))
  
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
})

test_that("messages is tested and works", {
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 30, r = 100, family = "mDependentPS",
                                          filter = testfilter, output = "maximum")
  
  expect_error(getCritVal(n = 30L, filter = testfilter, messages = "s",
                          r = 100, options = list(load = list())))
  expect_error(getCritVal(n = 30L, filter = testfilter, messages = 0L,
                          r = 100, options = list(load = list())))
  expect_error(getCritVal(n = 30L, filter = testfilter, messages = c(10L, 20L),
                          r = 100, options = list(load = list())))
  
  expect_identical(suppressMessages(getCritVal(n = 30L, filter = testfilter, messages = 10L,
                                               r = 100, options = list(load = list()))),
                   getCritVal(n = 30L, filter = testfilter, r = 100, options = list(load = list())))
  
  expect_identical(suppressMessages(getCritVal(n = 30L, filter = testfilter, messages = 10.5,
                                               r = 100, options = list(load = list()))),
                   getCritVal(n = 30L, filter = testfilter, r = 100, options = list(load = list())))
})

test_that("... can only be used by families LR and 2Param", {
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  
  expect_warning(ret <- getCritVal(n = 30L, filter = testfilter, localValue = mean, r = 10L))
  expect_identical(ret, getCritVal(n = 30L, filter = testfilter, r = 10L))
  expect_warning(ret <- getCritVal(n = 30L, family = "jsmurf", filter = testfilter, localVar = stats::sd, r = 10L))
  expect_identical(ret, getCritVal(n = 30L, family = "jsmurf", filter = testfilter, r = 10L))
  expect_warning(ret <- getCritVal(n = 30L, family = "hjsmurf", filter = testfilter, thresholdLongSegment = 10L, r = 10L))
  expect_identical(ret, getCritVal(n = 30L, family = "hjsmurf", filter = testfilter, r = 10L))
})

test_that("family jsmurf works", {
  testJsmurf <- function(testalpha, testn, testnq, teststat, testfilter) {
    ret <- getCritVal(alpha = testalpha, n = testn, nq = testnq, stat = teststat, filter = testfilter,
                      family = "jsmurf") 
    
    expect_identical(ret, stepR::critVal(alpha = testalpha, n = testn, nq = testnq, family = "jsmurf",
                                         intervalSystem = "dyaLen",
                                         lengths = 2^((1L + as.integer(log2(testfilter$len) + 1e-6)):
                                                        (as.integer(log2(testn) + 1e-6))),
                                         penalty = "sqrt", stat = teststat, output = "value",
                                         filter = testfilter))
    
    expect_identical(stepR::critVal(q = ret, alpha = testalpha, n = testn, nq = testnq, family = "jsmurf",
                                    intervalSystem = "dyaLen",
                                    lengths = 2^((1L + as.integer(log2(testfilter$len) + 1e-6)):
                                                   (as.integer(log2(testn) + 1e-6))),
                                    penalty = "sqrt", stat = teststat, output = "vector", filter = testfilter),
                     stepR::critVal(alpha = testalpha, n = testn, nq = testnq, family = "jsmurf",
                                    intervalSystem = "dyaLen",
                                    lengths = 2^((1L + as.integer(log2(testfilter$len) + 1e-6)):
                                                   (as.integer(log2(testn) + 1e-6))),
                                    penalty = "sqrt", stat = teststat, output = "vector", filter = testfilter))
  }
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 70, r = 100, family = "jsmurf",
                                          filter = testfilter, output = "maximum")
  
  expect_error(getCritVal(family = "jsmurf"))
  expect_error(getCritVal(family = "jsmurf", stat = teststat))
  expect_error(getCritVal(family = "jsmurf", stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "jsmurf", n = "s", stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "jsmurf", n = c(1L, 2L), stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "jsmurf", n = as.integer(NA), stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "jsmurf", n = NULL, stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "jsmurf", n = Inf, stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "jsmurf", n = 0L, stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "jsmurf", n = -1L, stat = teststat, filter = testfilter))
  
  expect_identical(getCritVal(family = "jsmurf", n = 70L, stat = teststat, filter = testfilter),
                   getCritVal(family = "jsmurf", n = 70L, stat = teststat, filter = testfilter,
                              nq = 70L, alpha = 0.05))
  expect_identical(getCritVal(family = "jsmurf", n = 70, stat = teststat, filter = testfilter),
                   getCritVal(family = "jsmurf", n = 70L, stat = teststat, filter = testfilter))
  expect_identical(getCritVal(family = "jsmurf", n = 70.5, stat = teststat, filter = testfilter),
                   getCritVal(family = "jsmurf", n = 70L, stat = teststat, filter = testfilter))
  
  testJsmurf(testalpha = 0.05, testn = 70L, testnq = 70L, teststat = teststat, testfilter = testfilter)
  
  testfilter <- lowpassFilter(param = list(pole = 6L, cutoff = 0.25), len = 5)
  teststat <- stepR::monteCarloSimulation(n = 60, r = 100, family = "jsmurf",
                                          filter = testfilter, output = "maximum")
  testJsmurf(testalpha = 0.3, testn = 45L, testnq = 60L, teststat = teststat, testfilter = testfilter)
  
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 30, r = 100, family = "jsmurf",
                                          filter = testfilter, output = "maximum")
  expect_identical(getCritVal(n = 30L, family = "jsmurf", stat = teststat, filter = testfilter),
                   getCritVal(n = 30L, family = "jsmurf", filter = testfilter, r = 100, options = list(load = list())))
  
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 30, r = 100, family = "jsmurf",
                                          filter = testfilter, output = "maximum")
  
  testfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".RDS")
  expect_identical(getCritVal(n = 30L, family = "jsmurf", filter = testfilter, r = 100L,
                              options = list(save = list(RDSfile = testfile), load = list())),
                   getCritVal(n = 30L, family = "jsmurf", stat = teststat, filter = testfilter,
                              options = list(save = list())))
  expect_identical(readRDS(testfile), teststat)
  expect_identical(getCritVal(n = 30L, family = "jsmurf", filter = testfilter, r = 50L,
                              options = list(save = list(), load = list(RDSfile = testfile))),
                   getCritVal(n = 30L, family = "jsmurf", stat = teststat, filter = testfilter,
                              options = list(save = list())))
  unlink(testfile)
})

test_that("family jsmurfPS works", {
  testJsmurfPS <- function(testalpha, testn, testnq, teststat, testfilter) {
    ret <- getCritVal(alpha = testalpha, n = testn, nq = testnq, stat = teststat, filter = testfilter,
                      family = "jsmurfPS") 
    
    expect_identical(ret, stepR::critVal(alpha = testalpha, n = testn, nq = testnq, family = "jsmurfPS",
                                         intervalSystem = "dyaLen",
                                         lengths = 2^((1L + as.integer(log2(testfilter$len) + 1e-6)):
                                                        (as.integer(log2(testn) + 1e-6))),
                                         penalty = "sqrt", stat = teststat, output = "value",
                                         filter = testfilter))
    
    expect_identical(stepR::critVal(q = ret, alpha = testalpha, n = testn, nq = testnq, family = "jsmurfPS",
                                    intervalSystem = "dyaLen",
                                    lengths = 2^((1L + as.integer(log2(testfilter$len) + 1e-6)):
                                                   (as.integer(log2(testn) + 1e-6))),
                                    penalty = "sqrt", stat = teststat, output = "vector", filter = testfilter),
                     stepR::critVal(alpha = testalpha, n = testn, nq = testnq, family = "jsmurfPS",
                                    intervalSystem = "dyaLen",
                                    lengths = 2^((1L + as.integer(log2(testfilter$len) + 1e-6)):
                                                   (as.integer(log2(testn) + 1e-6))),
                                    penalty = "sqrt", stat = teststat, output = "vector", filter = testfilter))
  }
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 70, r = 100, family = "jsmurfPS",
                                          filter = testfilter, output = "maximum")
  
  expect_error(getCritVal(family = "jsmurfPS"))
  expect_error(getCritVal(family = "jsmurfPS", stat = teststat))
  expect_error(getCritVal(family = "jsmurfPS", stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "jsmurfPS", n = "s", stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "jsmurfPS", n = c(1L, 2L), stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "jsmurfPS", n = as.integer(NA), stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "jsmurfPS", n = NULL, stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "jsmurfPS", n = Inf, stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "jsmurfPS", n = 0L, stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "jsmurfPS", n = -1L, stat = teststat, filter = testfilter))
  
  expect_identical(getCritVal(family = "jsmurfPS", n = 70L, stat = teststat, filter = testfilter),
                   getCritVal(family = "jsmurfPS", n = 70L, stat = teststat, filter = testfilter,
                              nq = 70L, alpha = 0.05))
  expect_identical(getCritVal(family = "jsmurfPS", n = 70, stat = teststat, filter = testfilter),
                   getCritVal(family = "jsmurfPS", n = 70L, stat = teststat, filter = testfilter))
  expect_identical(getCritVal(family = "jsmurfPS", n = 70.5, stat = teststat, filter = testfilter),
                   getCritVal(family = "jsmurfPS", n = 70L, stat = teststat, filter = testfilter))
  
  testJsmurfPS(testalpha = 0.05, testn = 70L, testnq = 70L, teststat = teststat, testfilter = testfilter)
  
  testfilter <- lowpassFilter(param = list(pole = 6L, cutoff = 0.25), len = 5)
  teststat <- stepR::monteCarloSimulation(n = 60, r = 100, family = "jsmurfPS",
                                          filter = testfilter, output = "maximum")
  testJsmurfPS(testalpha = 0.3, testn = 45L, testnq = 60L, teststat = teststat, testfilter = testfilter)
  
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 30, r = 100, family = "jsmurfPS",
                                          filter = testfilter, output = "maximum")
  expect_identical(getCritVal(n = 30L, family = "jsmurfPS", stat = teststat, filter = testfilter),
                   getCritVal(n = 30L, family = "jsmurfPS", filter = testfilter, r = 100, options = list(load = list())))
  
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 30, r = 100, family = "jsmurfPS",
                                          filter = testfilter, output = "maximum")
  
  testfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".RDS")
  expect_identical(getCritVal(n = 30L, family = "jsmurfPS", filter = testfilter, r = 100L,
                              options = list(save = list(RDSfile = testfile), load = list())),
                   getCritVal(n = 30L, family = "jsmurfPS", stat = teststat, filter = testfilter,
                              options = list(save = list())))
  expect_identical(readRDS(testfile), teststat)
  expect_identical(getCritVal(n = 30L, family = "jsmurfPS", filter = testfilter, r = 50L,
                              options = list(save = list(), load = list(RDSfile = testfile))),
                   getCritVal(n = 30L, family = "jsmurfPS", stat = teststat, filter = testfilter,
                              options = list(save = list())))
  unlink(testfile)
})

test_that("family jsmurfLR works", {
  testJsmurfLR <- function(testalpha, testn, testnq, teststat, testfilter) {
    ret <- getCritVal(alpha = testalpha, n = testn, nq = testnq, stat = teststat, filter = testfilter,
                      family = "jsmurfLR") 
    
    expect_identical(ret, stepR::critVal(alpha = testalpha, n = testn, nq = testnq, family = "jsmurfLR",
                                         intervalSystem = "dyaLen",
                                         lengths = 2^((1L + as.integer(log2(testfilter$len) + 1e-6)):
                                                        (as.integer(log2(testn) + 1e-6))),
                                         penalty = "sqrt", stat = teststat, output = "value",
                                         filter = testfilter))
    
    expect_identical(stepR::critVal(q = ret, alpha = testalpha, n = testn, nq = testnq, family = "jsmurfLR",
                                    intervalSystem = "dyaLen",
                                    lengths = 2^((1L + as.integer(log2(testfilter$len) + 1e-6)):
                                                   (as.integer(log2(testn) + 1e-6))),
                                    penalty = "sqrt", stat = teststat, output = "vector", filter = testfilter),
                     stepR::critVal(alpha = testalpha, n = testn, nq = testnq, family = "jsmurfLR",
                                    intervalSystem = "dyaLen",
                                    lengths = 2^((1L + as.integer(log2(testfilter$len) + 1e-6)):
                                                   (as.integer(log2(testn) + 1e-6))),
                                    penalty = "sqrt", stat = teststat, output = "vector", filter = testfilter))
  }
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 70, r = 100, family = "jsmurfLR",
                                          filter = testfilter, output = "maximum")
  
  expect_error(getCritVal(family = "jsmurfLR"))
  expect_error(getCritVal(family = "jsmurfLR", stat = teststat))
  expect_error(getCritVal(family = "jsmurfLR", stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "jsmurfLR", n = "s", stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "jsmurfLR", n = c(1L, 2L), stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "jsmurfLR", n = as.integer(NA), stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "jsmurfLR", n = NULL, stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "jsmurfLR", n = Inf, stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "jsmurfLR", n = 0L, stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "jsmurfLR", n = -1L, stat = teststat, filter = testfilter))
  
  expect_identical(getCritVal(family = "jsmurfLR", n = 70L, stat = teststat, filter = testfilter),
                   getCritVal(family = "jsmurfLR", n = 70L, stat = teststat, filter = testfilter,
                              nq = 70L, alpha = 0.05))
  expect_identical(getCritVal(family = "jsmurfLR", n = 70, stat = teststat, filter = testfilter),
                   getCritVal(family = "jsmurfLR", n = 70L, stat = teststat, filter = testfilter))
  expect_identical(getCritVal(family = "jsmurfLR", n = 70.5, stat = teststat, filter = testfilter),
                   getCritVal(family = "jsmurfLR", n = 70L, stat = teststat, filter = testfilter))
  
  testJsmurfLR(testalpha = 0.05, testn = 70L, testnq = 70L, teststat = teststat, testfilter = testfilter)
  
  testfilter <- lowpassFilter(param = list(pole = 6L, cutoff = 0.25), len = 5)
  teststat <- stepR::monteCarloSimulation(n = 60, r = 100, family = "jsmurfLR",
                                          filter = testfilter, output = "maximum")
  testJsmurfLR(testalpha = 0.3, testn = 45L, testnq = 60L, teststat = teststat, testfilter = testfilter)
  
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 30, r = 100, family = "jsmurfLR",
                                          filter = testfilter, output = "maximum")
  expect_identical(getCritVal(n = 30L, family = "jsmurfLR", stat = teststat, filter = testfilter),
                   getCritVal(n = 30L, family = "jsmurfLR", filter = testfilter, r = 100, options = list(load = list())))
  
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 30, r = 100, family = "jsmurfLR",
                                          filter = testfilter, output = "maximum")
  
  testfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".RDS")
  expect_identical(getCritVal(n = 30L, family = "jsmurfLR", filter = testfilter, r = 100L,
                              options = list(save = list(RDSfile = testfile), load = list())),
                   getCritVal(n = 30L, family = "jsmurfLR", stat = teststat, filter = testfilter,
                              options = list(save = list())))
  expect_identical(readRDS(testfile), teststat)
  expect_identical(getCritVal(n = 30L, family = "jsmurfLR", filter = testfilter, r = 50L,
                              options = list(save = list(), load = list(RDSfile = testfile))),
                   getCritVal(n = 30L, family = "jsmurfLR", stat = teststat, filter = testfilter,
                              options = list(save = list())))
  unlink(testfile)
})

test_that("family hjsmurf works", {
  testHjsmurf <- function(testalpha, testn, testnq, teststat, testfilter) {
    ret <- getCritVal(alpha = testalpha, n = testn, nq = testnq, stat = teststat, filter = testfilter,
                      family = "hjsmurf") 
    
    expect_identical(ret, stepR::critVal(alpha = testalpha, n = testn, nq = testnq, family = "hjsmurf",
                                         intervalSystem = "dyaLen",
                                         lengths = 2^((1L + as.integer(log2(testfilter$len) + 1e-6)):
                                                        (as.integer(log2(testn) + 1e-6))),
                                         penalty = "weights", stat = teststat, output = "vector",
                                         filter = testfilter))
    
    expect_identical(stepR::critVal(q = ret, alpha = testalpha, n = testn, nq = testnq, family = "hjsmurf",
                                    intervalSystem = "dyaLen",
                                    lengths = 2^((1L + as.integer(log2(testfilter$len) + 1e-6)):
                                                   (as.integer(log2(testn) + 1e-6))),
                                    penalty = "weights", stat = teststat, output = "vector", filter = testfilter),
                     stepR::critVal(alpha = testalpha, n = testn, nq = testnq, family = "hjsmurf",
                                    intervalSystem = "dyaLen",
                                    lengths = 2^((1L + as.integer(log2(testfilter$len) + 1e-6)):
                                                   (as.integer(log2(testn) + 1e-6))),
                                    penalty = "weights", stat = teststat, output = "vector", filter = testfilter))
  }
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 70, r = 100, family = "hjsmurf",
                                          filter = testfilter, output = "vector")
  
  expect_error(getCritVal(family = "hjsmurf"))
  expect_error(getCritVal(family = "hjsmurf", stat = teststat))
  expect_error(getCritVal(family = "hjsmurf", stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "hjsmurf", n = "s", stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "hjsmurf", n = c(1L, 2L), stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "hjsmurf", n = as.integer(NA), stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "hjsmurf", n = NULL, stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "hjsmurf", n = Inf, stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "hjsmurf", n = 0L, stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "hjsmurf", n = -1L, stat = teststat, filter = testfilter))
  
  expect_identical(getCritVal(family = "hjsmurf", n = 70L, stat = teststat, filter = testfilter),
                   getCritVal(family = "hjsmurf", n = 70L, stat = teststat, filter = testfilter,
                              nq = 70L, alpha = 0.05))
  expect_identical(getCritVal(family = "hjsmurf", n = 70, stat = teststat, filter = testfilter),
                   getCritVal(family = "hjsmurf", n = 70L, stat = teststat, filter = testfilter))
  expect_identical(getCritVal(family = "hjsmurf", n = 70.5, stat = teststat, filter = testfilter),
                   getCritVal(family = "hjsmurf", n = 70L, stat = teststat, filter = testfilter))
  
  testHjsmurf(testalpha = 0.05, testn = 70L, testnq = 70L, teststat = teststat, testfilter = testfilter)
  
  testfilter <- lowpassFilter(param = list(pole = 6L, cutoff = 0.25), len = 5)
  teststat <- stepR::monteCarloSimulation(n = 60, r = 100, family = "hjsmurf",
                                          filter = testfilter, output = "vector")
  testHjsmurf(testalpha = 0.3, testn = 45L, testnq = 60L, teststat = teststat, testfilter = testfilter)
  
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 30, r = 100, family = "hjsmurf",
                                          filter = testfilter, output = "vector")
  expect_identical(getCritVal(n = 30L, family = "hjsmurf", stat = teststat, filter = testfilter),
                   getCritVal(n = 30L, family = "hjsmurf", filter = testfilter, r = 100, options = list(load = list())))
  
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 30, r = 100, family = "hjsmurf",
                                          filter = testfilter, output = "vector")
  
  testfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".RDS")
  expect_identical(getCritVal(n = 30L, family = "hjsmurf", filter = testfilter, r = 100L,
                              options = list(save = list(RDSfile = testfile), load = list())),
                   getCritVal(n = 30L, family = "hjsmurf", stat = teststat, filter = testfilter,
                              options = list(save = list())))
  expect_identical(readRDS(testfile), teststat)
  expect_identical(getCritVal(n = 30L, family = "hjsmurf", filter = testfilter, r = 50L,
                              options = list(save = list(), load = list(RDSfile = testfile))),
                   getCritVal(n = 30L, family = "hjsmurf", stat = teststat, filter = testfilter,
                              options = list(save = list())))
  unlink(testfile)
})

test_that("family hjsmurfSPS works", {
  testHjsmurfSPS <- function(testalpha, testn, testnq, teststat, testfilter) {
    ret <- getCritVal(alpha = testalpha, n = testn, nq = testnq, stat = teststat, filter = testfilter,
                      family = "hjsmurfSPS") 
    
    expect_identical(ret, stepR::critVal(alpha = testalpha, n = testn, nq = testnq, family = "hjsmurfSPS",
                                         intervalSystem = "dyaLen",
                                         lengths = 2^((1L + as.integer(log2(testfilter$len) + 1e-6)):
                                                        (as.integer(log2(testn) + 1e-6))),
                                         penalty = "weights", stat = teststat, output = "vector",
                                         filter = testfilter))
    
    expect_identical(stepR::critVal(q = ret, alpha = testalpha, n = testn, nq = testnq, family = "hjsmurfSPS",
                                    intervalSystem = "dyaLen",
                                    lengths = 2^((1L + as.integer(log2(testfilter$len) + 1e-6)):
                                                   (as.integer(log2(testn) + 1e-6))),
                                    penalty = "weights", stat = teststat, output = "vector", filter = testfilter),
                     stepR::critVal(alpha = testalpha, n = testn, nq = testnq, family = "hjsmurfSPS",
                                    intervalSystem = "dyaLen",
                                    lengths = 2^((1L + as.integer(log2(testfilter$len) + 1e-6)):
                                                   (as.integer(log2(testn) + 1e-6))),
                                    penalty = "weights", stat = teststat, output = "vector", filter = testfilter))
  }
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 70, r = 100, family = "hjsmurfSPS",
                                          filter = testfilter, output = "vector")
  
  expect_error(getCritVal(family = "hjsmurfSPS"))
  expect_error(getCritVal(family = "hjsmurfSPS", stat = teststat))
  expect_error(getCritVal(family = "hjsmurfSPS", stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "hjsmurfSPS", n = "s", stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "hjsmurfSPS", n = c(1L, 2L), stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "hjsmurfSPS", n = as.integer(NA), stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "hjsmurfSPS", n = NULL, stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "hjsmurfSPS", n = Inf, stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "hjsmurfSPS", n = 0L, stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "hjsmurfSPS", n = -1L, stat = teststat, filter = testfilter))
  
  expect_identical(getCritVal(family = "hjsmurfSPS", n = 70L, stat = teststat, filter = testfilter),
                   getCritVal(family = "hjsmurfSPS", n = 70L, stat = teststat, filter = testfilter,
                              nq = 70L, alpha = 0.05))
  expect_identical(getCritVal(family = "hjsmurfSPS", n = 70, stat = teststat, filter = testfilter),
                   getCritVal(family = "hjsmurfSPS", n = 70L, stat = teststat, filter = testfilter))
  expect_identical(getCritVal(family = "hjsmurfSPS", n = 70.5, stat = teststat, filter = testfilter),
                   getCritVal(family = "hjsmurfSPS", n = 70L, stat = teststat, filter = testfilter))
  
  testHjsmurfSPS(testalpha = 0.05, testn = 70L, testnq = 70L, teststat = teststat, testfilter = testfilter)
  
  testfilter <- lowpassFilter(param = list(pole = 6L, cutoff = 0.25), len = 5)
  teststat <- stepR::monteCarloSimulation(n = 60, r = 100, family = "hjsmurfSPS",
                                          filter = testfilter, output = "vector")
  testHjsmurfSPS(testalpha = 0.3, testn = 45L, testnq = 60L, teststat = teststat, testfilter = testfilter)
  
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 30, r = 100, family = "hjsmurfSPS",
                                          filter = testfilter, output = "vector")
  expect_identical(getCritVal(n = 30L, family = "hjsmurfSPS", stat = teststat, filter = testfilter),
                   getCritVal(n = 30L, family = "hjsmurfSPS", filter = testfilter, r = 100, options = list(load = list())))
  
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 30, r = 100, family = "hjsmurfSPS",
                                          filter = testfilter, output = "vector")
  
  testfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".RDS")
  expect_identical(getCritVal(n = 30L, family = "hjsmurfSPS", filter = testfilter, r = 100L,
                              options = list(save = list(RDSfile = testfile), load = list())),
                   getCritVal(n = 30L, family = "hjsmurfSPS", stat = teststat, filter = testfilter,
                              options = list(save = list())))
  expect_identical(readRDS(testfile), teststat)
  expect_identical(getCritVal(n = 30L, family = "hjsmurfSPS", filter = testfilter, r = 50L,
                              options = list(save = list(), load = list(RDSfile = testfile))),
                   getCritVal(n = 30L, family = "hjsmurfSPS", stat = teststat, filter = testfilter,
                              options = list(save = list())))
  unlink(testfile)
})

test_that("family hjsmurfLR works", {
  testHjsmurfLR <- function(testalpha, testn, testnq, teststat, testfilter) {
    ret <- getCritVal(alpha = testalpha, n = testn, nq = testnq, stat = teststat, filter = testfilter,
                      family = "hjsmurfLR") 
    
    expect_identical(ret, stepR::critVal(alpha = testalpha, n = testn, nq = testnq, family = "hjsmurfLR",
                                         intervalSystem = "dyaLen",
                                         lengths = 2^((1L + as.integer(log2(testfilter$len) + 1e-6)):
                                                        (as.integer(log2(testn) + 1e-6))),
                                         penalty = "weights", stat = teststat, output = "vector",
                                         filter = testfilter))
    
    expect_identical(stepR::critVal(q = ret, alpha = testalpha, n = testn, nq = testnq, family = "hjsmurfLR",
                                    intervalSystem = "dyaLen",
                                    lengths = 2^((1L + as.integer(log2(testfilter$len) + 1e-6)):
                                                   (as.integer(log2(testn) + 1e-6))),
                                    penalty = "weights", stat = teststat, output = "vector", filter = testfilter),
                     stepR::critVal(alpha = testalpha, n = testn, nq = testnq, family = "hjsmurfLR",
                                    intervalSystem = "dyaLen",
                                    lengths = 2^((1L + as.integer(log2(testfilter$len) + 1e-6)):
                                                   (as.integer(log2(testn) + 1e-6))),
                                    penalty = "weights", stat = teststat, output = "vector", filter = testfilter))
  }
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 70, r = 100, family = "hjsmurfLR",
                                          filter = testfilter, output = "vector")
  
  expect_error(getCritVal(family = "hjsmurfLR"))
  expect_error(getCritVal(family = "hjsmurfLR", stat = teststat))
  expect_error(getCritVal(family = "hjsmurfLR", stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "hjsmurfLR", n = "s", stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "hjsmurfLR", n = c(1L, 2L), stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "hjsmurfLR", n = as.integer(NA), stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "hjsmurfLR", n = NULL, stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "hjsmurfLR", n = Inf, stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "hjsmurfLR", n = 0L, stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "hjsmurfLR", n = -1L, stat = teststat, filter = testfilter))
  
  expect_identical(getCritVal(family = "hjsmurfLR", n = 70L, stat = teststat, filter = testfilter),
                   getCritVal(family = "hjsmurfLR", n = 70L, stat = teststat, filter = testfilter,
                              nq = 70L, alpha = 0.05))
  expect_identical(getCritVal(family = "hjsmurfLR", n = 70, stat = teststat, filter = testfilter),
                   getCritVal(family = "hjsmurfLR", n = 70L, stat = teststat, filter = testfilter))
  expect_identical(getCritVal(family = "hjsmurfLR", n = 70.5, stat = teststat, filter = testfilter),
                   getCritVal(family = "hjsmurfLR", n = 70L, stat = teststat, filter = testfilter))
  
  testHjsmurfLR(testalpha = 0.05, testn = 70L, testnq = 70L, teststat = teststat, testfilter = testfilter)
  
  testfilter <- lowpassFilter(param = list(pole = 6L, cutoff = 0.25), len = 5)
  teststat <- stepR::monteCarloSimulation(n = 60, r = 100, family = "hjsmurfLR",
                                          filter = testfilter, output = "vector")
  testHjsmurfLR(testalpha = 0.3, testn = 45L, testnq = 60L, teststat = teststat, testfilter = testfilter)
  
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 30, r = 100, family = "hjsmurfLR",
                                          filter = testfilter, output = "vector")
  expect_identical(getCritVal(n = 30L, family = "hjsmurfLR", stat = teststat, filter = testfilter),
                   getCritVal(n = 30L, family = "hjsmurfLR", filter = testfilter, r = 100, options = list(load = list())))
  
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 30, r = 100, family = "hjsmurfLR",
                                          filter = testfilter, output = "vector")
  
  testfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".RDS")
  expect_identical(getCritVal(n = 30L, family = "hjsmurfLR", filter = testfilter, r = 100L,
                              options = list(save = list(RDSfile = testfile), load = list())),
                   getCritVal(n = 30L, family = "hjsmurfLR", stat = teststat, filter = testfilter,
                              options = list(save = list())))
  expect_identical(readRDS(testfile), teststat)
  expect_identical(getCritVal(n = 30L, family = "hjsmurfLR", filter = testfilter, r = 50L,
                              options = list(save = list(), load = list(RDSfile = testfile))),
                   getCritVal(n = 30L, family = "hjsmurfLR", stat = teststat, filter = testfilter,
                              options = list(save = list())))
  unlink(testfile)
})

test_that("family LR works", {
  testLR <- function(testalpha, testn, testnq, teststat, testfilter) {
    ret <- getCritVal(alpha = testalpha, n = testn, nq = testnq, stat = teststat, filter = testfilter,
                      family = "LR") 
    
    expect_identical(ret, stepR::critVal(alpha = testalpha, n = testn, nq = testnq, family = "LR",
                                         intervalSystem = "all", lengths = 1:20, penalty = "weights",
                                         stat = teststat, output = "vector", filter = testfilter))
    
    expect_identical(stepR::critVal(q = ret, alpha = testalpha, n = testn, nq = testnq, family = "LR",
                                    intervalSystem = "all", lengths = 1:20,
                                    penalty = "weights", stat = teststat, output = "vector", filter = testfilter),
                     stepR::critVal(alpha = testalpha, n = testn, nq = testnq, family = "LR",
                                    intervalSystem = "all", lengths = 1:20,
                                    penalty = "weights", stat = teststat, output = "vector", filter = testfilter))
  }
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 70, r = 100, family = "LR", filter = testfilter, output = "vector")
  
  expect_error(getCritVal(family = "LR"))
  expect_error(getCritVal(family = "LR", stat = teststat))
  expect_error(getCritVal(family = "LR", stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "LR", n = "s", stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "LR", n = c(1L, 2L), stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "LR", n = as.integer(NA), stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "LR", n = NULL, stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "LR", n = Inf, stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "LR", n = 0L, stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "LR", n = -1L, stat = teststat, filter = testfilter))
  
  expect_identical(getCritVal(family = "LR", n = 70L, stat = teststat, filter = testfilter),
                   getCritVal(family = "LR", n = 70L, stat = teststat, filter = testfilter,
                              nq = 70L, alpha = 0.05))
  expect_identical(getCritVal(family = "LR", n = 70, stat = teststat, filter = testfilter),
                   getCritVal(family = "LR", n = 70L, stat = teststat, filter = testfilter))
  expect_identical(getCritVal(family = "LR", n = 70.5, stat = teststat, filter = testfilter),
                   getCritVal(family = "LR", n = 70L, stat = teststat, filter = testfilter))
  
  testLR(testalpha = 0.05, testn = 70L, testnq = 70L, teststat = teststat, testfilter = testfilter)
  
  testfilter <- lowpassFilter(param = list(pole = 6L, cutoff = 0.25), len = 5)
  teststat <- stepR::monteCarloSimulation(n = 60, r = 100, family = "LR",
                                          filter = testfilter, output = "vector")
  testLR(testalpha = 0.3, testn = 45L, testnq = 60L, teststat = teststat, testfilter = testfilter)
  
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 50, r = 100, family = "LR",
                                          filter = testfilter, output = "vector")
  expect_identical(getCritVal(n = 50L, family = "LR", stat = teststat, filter = testfilter),
                   getCritVal(n = 50L, family = "LR", filter = testfilter, r = 100, options = list(load = list())))
  
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 50, r = 100, family = "LR",
                                          filter = testfilter, output = "vector")
  
  testfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".RDS")
  expect_identical(getCritVal(n = 50L, family = "LR", filter = testfilter, r = 100L,
                              options = list(save = list(RDSfile = testfile), load = list())),
                   getCritVal(n = 50L, family = "LR", stat = teststat, filter = testfilter,
                              options = list(save = list())))
  expect_identical(readRDS(testfile), teststat)
  expect_identical(getCritVal(n = 50L, family = "LR", filter = testfilter, r = 50L,
                              options = list(save = list(), load = list(RDSfile = testfile))),
                   getCritVal(n = 50L, family = "LR", stat = teststat, filter = testfilter,
                              options = list(save = list())))
  unlink(testfile)
  
  testStepR <- new.env()
  teststat <- stepR::monteCarloSimulation(100L, r = 100L, family = "LR", localVal = mean,
                                          filter = testfilter)
  expect_identical(getCritVal(100L, alpha = 0.1, r = 100L, family = "LR",
                              filter = testfilter, localValue = mean,
                              options = list(simulation = "matrix", save = list(workspace = "matrix"),
                                             load = list(), envir = testStepR)),
                   getCritVal(100L, alpha = 0.1, stat = teststat, family = "LR",
                              filter = testfilter, options = list()))
  expect_false(exists("critValStepRTab", envir = testStepR, inherits = FALSE))
  
  
  testlocalList <- createLocalList(filter = testfilter, method = "LR")
  expect_error(getCritVal(family = "LR", n = 100L, stat = teststat, filter = testfilter,
                          localList = 1))
  expect_error(getCritVal(family = "LR", n = 100L, stat = teststat, filter = testfilter,
                          localList = list()))
  expect_error(getCritVal(family = "LR", n = 100L, stat = teststat, filter = testfilter,
                          localList = unclass(testlocalList)))
  
  expect_identical(getCritVal(family = "LR", n = 100L, stat = teststat, filter = testfilter,
                              localList = testlocalList),
                   getCritVal(family = "LR", n = 100L, stat = teststat, filter = testfilter))
  
  testlengths <- c(2, 11)
  testlocalList <- createLocalList(filter = testfilter, method = "LR", lengths = testlengths)
  expect_error(getCritVal(family = "LR", n = 100L, stat = teststat, filter = testfilter,
                          lengths = c(1, 10), localList = testlocalList))
  expect_identical(getCritVal(family = "LR", n = 100L, stat = teststat, filter = testfilter, lengths = testlengths,
                              localList = testlocalList),
                   getCritVal(family = "LR", n = 100L, stat = teststat, filter = testfilter, lengths = testlengths))
  
  
  testfilter2 <- lowpassFilter::lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 5)
  expect_error(getCritVal(family = "LR", n = 100L, stat = teststat, filter = testfilter2,
                          lengths = testlengths, localList = testlocalList))
  
  testfilter2 <- lowpassFilter::lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 1)
  expect_identical(getCritVal(family = "LR", n = 100L, stat = teststat, filter = testfilter,
                              lengths = testlengths, localList = testlocalList),
                   getCritVal(family = "LR", n = 100L, stat = teststat, filter = testfilter2,
                              lengths = testlengths, localList = testlocalList))
  
  testlocalList <- createLocalList(filter = testfilter, method = "2Param", lengths = testlengths)
  expect_error(getCritVal(family = "LR", n = 100L, stat = teststat, filter = testfilter,
                          lengths = testlengths, localList = testlocalList))
  
  # localList is compatibel with other arguments and saving of MC
  testlocalList <- createLocalList(filter = testfilter, method = "LR", lengths = testlengths)
  expect_identical(getCritVal(family = "LR", n = 100L, stat = teststat, filter = testfilter,
                              lengths = testlengths, alpha = 0.35, localValue = mean,
                              localList = testlocalList),
                   getCritVal(family = "LR", n = 100L, stat = teststat, filter = testfilter,
                              lengths = testlengths, alpha = 0.35, localValue = mean))
  
  
  testlocalList <- createLocalList(filter = testfilter, method = "LR")
  teststat <- stepR::monteCarloSimulation(100L, r = 100L, family = "LR",
                                          filter = testfilter)
  testfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".RDS")
  expect_identical(getCritVal(n = 100L, family = "LR", filter = testfilter, r = 100L,
                              options = list(save = list(RDSfile = testfile), load = list()),
                              nq = 100L,
                              localList = testlocalList),
                   getCritVal(n = 100L, family = "LR", stat = teststat, filter = testfilter,
                              options = list(save = list())))
  expect_identical(readRDS(testfile), teststat)
  expect_identical(getCritVal(n = 100L, family = "LR", filter = testfilter, r = 50L,
                              options = list(save = list(), load = list(RDSfile = testfile))),
                   getCritVal(n = 100L, family = "LR", stat = teststat, filter = testfilter,
                              options = list(save = list())))
  unlink(testfile)
  
  expect_error(getCritVal(n = 100L, family = "LR", stat = teststat, filter = testfilter, method = "LR"))
  expect_error(getCritVal(n = 100L, family = "LR", stat = teststat, filter = testfilter, 
                          localVar = sd))
  
  
  ret <- getCritVal(n = 100L, family = "LR", filter = testfilter, r = 10L, lengths = c(1:10, 21))
  expect_identical(length(ret), 11L)
  expect_identical(attr(ret, "n"), 100L)
  
  teststat <- stepR::monteCarloSimulation(n = 100L, r = 10L, family = "LR", filter = testfilter)
  expect_error(getCritVal(n = 100L, family = "LR", filter = testfilter, stat = teststat, lengths = c(10, 25)))
  
  teststat <- stepR::monteCarloSimulation(n = 100L, r = 10L, family = "LR", filter = testfilter, lengths = c(10, 75),
                                          output = "maximum", penalty = "sqrt")
  expect_identical(getCritVal(n = 100L, family = "LR", filter = testfilter, r = 10L,
                              lengths = c(10, 75), penalty = "sqrt"),
                   stepR::critVal(n = 100L, family = "LR", filter = testfilter, alpha = 0.05,
                                  lengths = c(10, 75), penalty = "sqrt", stat = teststat))
  
  expect_identical(getCritVal(n = 100L, family = "LR", filter = testfilter, r = 10L,
                           lengths = c(10, 75), penalty = "sqrt",
                           options = list(simulation = "matrix",
                                          save = list(fileSystem = c("matrix", "vector",
                                                                     "matrixIncreased", "vectorIncreased")),
                                          dirs = "testStepR")),
                   getCritVal(n = 100L, family = "LR", filter = testfilter,
                           lengths = c(10, 75), penalty = "sqrt", stat = teststat))
  expect_false(file_test(op = "-d", file.path(R.cache::getCacheRootPath(), "testStepR")))
})

test_that("family 2Param works", {
  skip_on_cran()
  test2Param <- function(testalpha, testn, testnq, teststat, testfilter) {
    ret <- getCritVal(alpha = testalpha, n = testn, nq = testnq, stat = teststat, filter = testfilter,
                      family = "2Param", lengths = c(3, 13:15, 64:65)) 
    
    expect_identical(ret, stepR::critVal(alpha = testalpha, n = testn, nq = testnq, family = "2Param",
                                         intervalSystem = "all", lengths = c(3, 13:15, 64:65), penalty = "weights",
                                         stat = teststat, output = "vector", filter = testfilter))
    
    expect_identical(stepR::critVal(q = ret, alpha = testalpha, n = testn, nq = testnq, family = "2Param",
                                    intervalSystem = "all", lengths = c(3, 13:15, 64:65),
                                    penalty = "weights", stat = teststat, output = "vector", filter = testfilter),
                     stepR::critVal(alpha = testalpha, n = testn, nq = testnq, family = "2Param",
                                    intervalSystem = "all", lengths = c(3, 13:15, 64:65),
                                    penalty = "weights", stat = teststat, output = "vector", filter = testfilter))
  }
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 90, r = 2, family = "2Param", filter = testfilter, output = "vector")
  
  expect_error(getCritVal(family = "2Param"))
  expect_error(getCritVal(family = "2Param", stat = teststat))
  expect_error(getCritVal(family = "2Param", stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "2Param", n = "s", stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "2Param", n = c(1L, 2L), stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "2Param", n = as.integer(NA), stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "2Param", n = NULL, stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "2Param", n = Inf, stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "2Param", n = 0L, stat = teststat, filter = testfilter))
  expect_error(getCritVal(family = "2Param", n = -1L, stat = teststat, filter = testfilter))
  
  expect_identical(getCritVal(family = "2Param", n = 90L, stat = teststat, filter = testfilter,
                              lengths = c(3, 13:15, 64:65)),
                   getCritVal(family = "2Param", n = 90L, stat = teststat, filter = testfilter,
                              nq = 90L, alpha = 0.05, lengths = c(3, 13:15, 64:65)))
  
  test2Param(testalpha = 0.05, testn = 90L, testnq = 90L, teststat = teststat, testfilter = testfilter)
  
  
  testfilter <- lowpassFilter(param = list(pole = 4L, cutoff = 0.1), len = 11)
  teststat <- stepR::monteCarloSimulation(n = 100L, r = 2L, family = "2Param",
                                          filter = testfilter)
  
  testfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".RDS")
  expect_identical(getCritVal(n = 100L, family = "2Param", filter = testfilter, r = 2L,
                              options = list(save = list(RDSfile = testfile), load = list())),
                   getCritVal(n = 100L, family = "2Param", stat = teststat, filter = testfilter,
                              options = list(save = list())))
  expect_identical(readRDS(testfile), teststat)
  expect_identical(getCritVal(n = 100L, family = "2Param", filter = testfilter, r = 1L,
                              options = list(save = list(), load = list(RDSfile = testfile))),
                   getCritVal(n = 100L, family = "2Param", stat = teststat, filter = testfilter,
                              options = list(save = list())))
  unlink(testfile)
  
  testStepR <- new.env()
  teststat <- stepR::monteCarloSimulation(100L, r = 2L, family = "2Param", localVar = sd,
                                          filter = testfilter)
  expect_identical(getCritVal(100L, alpha = 0.1, r = 2L, family = "2Param",
                              filter = testfilter, localVar = sd,
                              options = list(simulation = "matrix", save = list(workspace = "matrix"),
                                             load = list(), envir = testStepR)),
                   getCritVal(100L, alpha = 0.1, stat = teststat, family = "2Param",
                              filter = testfilter, options = list()))
  expect_false(exists("critValStepRTab", envir = testStepR, inherits = FALSE))
  
  
  teststat <- stepR::monteCarloSimulation(n = 100L, r = 2L, family = "2Param",
                                          filter = testfilter, output = "vector")
  testlocalList <- createLocalList(filter = testfilter, method = "2Param")
  expect_error(getCritVal(family = "2Param", n = 100L, stat = teststat, filter = testfilter,
                          localList = 1))
  expect_error(getCritVal(family = "2Param", n = 100L, stat = teststat, filter = testfilter,
                          localList = list()))
  expect_error(getCritVal(family = "2Param", n = 100L, stat = teststat, filter = testfilter,
                          localList = unclass(testlocalList)))
  
  expect_identical(getCritVal(family = "2Param", n = 100L, stat = teststat, filter = testfilter,
                              localList = testlocalList),
                   getCritVal(family = "2Param", n = 100L, stat = teststat, filter = testfilter))
  
  testlengths <- c(2, 11)
  testlocalList <- createLocalList(filter = testfilter, method = "2Param", lengths = testlengths)
  expect_error(getCritVal(family = "2Param", n = 100L, stat = teststat, filter = testfilter,
                          lengths = c(1, 10), localList = testlocalList))
  expect_identical(getCritVal(family = "2Param", n = 100L, stat = teststat, filter = testfilter, lengths = testlengths,
                              localList = testlocalList),
                   getCritVal(family = "2Param", n = 100L, stat = teststat, filter = testfilter, lengths = testlengths))
  
  
  testfilter2 <- lowpassFilter::lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 5)
  expect_error(getCritVal(family = "2Param", n = 100L, stat = teststat, filter = testfilter2,
                          lengths = testlengths, localList = testlocalList))
  
  testfilter2 <- lowpassFilter::lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 1)
  expect_identical(getCritVal(family = "2Param", n = 100L, stat = teststat, filter = testfilter,
                              lengths = testlengths, localList = testlocalList),
                   getCritVal(family = "2Param", n = 100L, stat = teststat, filter = testfilter2,
                              lengths = testlengths, localList = testlocalList))
  
  testlocalList <- createLocalList(filter = testfilter, method = "LR", lengths = testlengths)
  expect_error(getCritVal(family = "2Param", n = 100L, stat = teststat, filter = testfilter,
                          lengths = testlengths, localList = testlocalList))
  
  # localList is compatibel with other arguments and saving of MC
  testlocalList <- createLocalList(filter = testfilter, method = "2Param", lengths = testlengths)
  expect_identical(getCritVal(family = "2Param", n = 100L, stat = teststat, filter = testfilter,
                              lengths = testlengths, alpha = 0.35, localValue = mean, localVar = sd,
                              localList = testlocalList),
                   getCritVal(family = "2Param", n = 100L, stat = teststat, filter = testfilter,
                              lengths = testlengths, alpha = 0.35, localValue = mean, localVar = sd))
  
  
  testlocalList <- createLocalList(filter = testfilter, method = "2Param")
  teststat <- stepR::monteCarloSimulation(100L, r = 2L, family = "2Param",
                                          filter = testfilter)
  testfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".RDS")
  expect_identical(getCritVal(n = 100L, family = "2Param", filter = testfilter, r = 2L,
                              options = list(save = list(RDSfile = testfile), load = list()),
                              nq = 100L,
                              localList = testlocalList),
                   getCritVal(n = 100L, family = "2Param", stat = teststat, filter = testfilter,
                              options = list(save = list())))
  expect_identical(readRDS(testfile), teststat)
  expect_identical(getCritVal(n = 100L, family = "2Param", filter = testfilter, r = 1L,
                              options = list(save = list(), load = list(RDSfile = testfile))),
                   getCritVal(n = 100L, family = "2Param", stat = teststat, filter = testfilter,
                              options = list(save = list())))
  unlink(testfile)
  
  expect_error(getCritVal(n = 100L, family = "2Param", stat = teststat, filter = testfilter, method = "2Param"))
})
