context("test-mcmc")

TestDir <- function() {
    test.dir <- tempfile()
    if(dir.create(test.dir)) {
        return(test.dir)
    }
    else {
        stop("Failed to create test directory")
    }
}

test_that("MCMC samples saved", {
    iter <- 3
    test.dir <- TestDir()

    Mcmc <- InitMcmc(iter, test.dir)
    x <- c(0, 0)
    y <- 0
    samps <- Mcmc({
        x <- c(1, 2)
        y <- 1
    })

    expect_equivalent(samps$x[,], matrix(rep(1:2, each=iter), nrow=iter))
    expect_equivalent(samps$y[,], matrix(rep(1, each=iter), nrow=iter))
})

test_that("Out of memory MCMC is file backed", {
    iter <- 1
    test.dir <- TestDir()

    Mcmc <- InitMcmc(iter, test.dir)
    samps <- Mcmc({
        x <- 1
    })

    expect_true(bigmemory::is.filebacked(samps$x))
})

test_that("In memory MCMC is not file backed", {
    iter <- 1

    Mcmc <- InitMcmc(iter)
    samps <- Mcmc({
        x <- 1
    })

    expect_false(bigmemory::is.big.matrix(samps$x))
})

test_that("Thinning works", {
    iter <- 3
    thin <- 2
    test.dir <- TestDir()

    Mcmc <- InitMcmc(iter, test.dir, thin)
    x <- 0
    samps <- Mcmc({
        x <- x + 1
    })

    expect_equivalent(samps$x[,], matrix(c(2, 4, 6)), ncol=1)
})

test_that("Non-numeric assignments aren't saved", {
    iter <- 3
    test.dir <- TestDir()

    Mcmc <- InitMcmc(iter, test.dir)
    x <- 0
    y <- c("char1", "char2")
    z <- "char3"
    samps <- Mcmc({
        x <- 1
        y <- c("testchar1", "testchar2")
        z <- "testchar3"
    })

    expect_null(samps$y)
    expect_null(samps$z)
    expect_equivalent(samps$x[,], rep(1, iter))
})

test_that("MCMC stops if thinning value isn't valid", {
    iter <- 1
    test.dir <- TestDir()

    thinning.error.msg <- "'thin' must be an integer > 0"
    expect_error(InitMcmc(iter, test.dir, thin=-1), thinning.error.msg)
    expect_error(InitMcmc(iter, test.dir, thin=1.5), thinning.error.msg)
})

test_that("MCMC stops if number of iterations isn't valid", {
    mcmc.iterations.error.msg <- "'n.save' must be an integer > 0"

    expect_error(InitMcmc(-1), mcmc.iterations.error.msg)
    expect_error(InitMcmc(0), mcmc.iterations.error.msg)
    expect_error(InitMcmc(1.1), mcmc.iterations.error.msg)
})

test_that("Warning given if the exclusions isn't a character vector", {
    exclude.warning.msg <- "'exclude' will be ignored, expected character vector"
    expect_warning(InitMcmc(1, exclude=c(1, 2, 3)), exclude.warning.msg)
})

test_that("Excluded assignments aren't saved", {
    iter <- 1
    test.dir <- TestDir()

    Mcmc <- InitMcmc(iter, test.dir, exclude="x")
    samps <- Mcmc({
        x <- 1
        y <- 1
    })
    expect_null(samps$x)
    expect_equivalent(samps$y[,], 1)
})

test_that("Error thrown if saving on-disk but 'bigmemory' not installed", {
    m <- mockery::mock(FALSE)
    mockery::stub(InitMcmc, 'requireNamespace', m)

    iter <- 1
    test.dir <- TestDir()

    # Error if saving on-disk and bigmemory not installed
    bigmem.err.msg <- "Package 'bigmemory' required for saving on-disk"
    expect_error(InitMcmc(iter, test.dir), bigmem.err.msg)
    mockery::expect_call(m, 1, requireNamespace("bigmemory", quietly=TRUE))

    # Saving in-memory still works
    Mcmc <- InitMcmc(iter)
    samps <- Mcmc({
        x <- 1
    })
    expect_equivalent(samps$x, rep(1, iter))
})

test_that("Matrix assignments are saved successfully", {
    iter <- 2
    test.dir <- TestDir()
    expected.save.result <- matrix(rep(1:4, times=iter), nrow=2, byrow=TRUE)

    Mcmc1 <- InitMcmc(iter, test.dir)
    samps1 <- Mcmc1({
        x <- matrix(expected.save.result[1,], nrow=2)
    })
    expect_equivalent(samps1$x[,], expected.save.result)

    Mcmc2 <- InitMcmc(iter)
    samps2 <- Mcmc2({
        x <- matrix(expected.save.result[1,], nrow=2)
    })
    expect_equivalent(samps2$x, expected.save.result)
})

test_that("Array assignments are saved successfully", {
    iter <- 2
    test.dir <- TestDir()
    expected.save.result <- matrix(rep(1:6, each=2), nrow=2)

    Mcmc1 <- InitMcmc(iter, test.dir)
    samps1 <- Mcmc1({
        x <- array(1:6, dim=c(1, 2, 3))
    })
    expect_equivalent(samps1$x[,], expected.save.result)

    Mcmc2 <- InitMcmc(iter)
    samps2 <- Mcmc2({
        x <- array(1:6, dim=c(1, 2, 3))
    })
    expect_equivalent(samps2$x, expected.save.result)
})

test_that("Completed file-backed MCMC can be loaded back into R", {
    iter <- 2
    test.dir <- TestDir()

    Mcmc <- InitMcmc(iter, test.dir)
    samps <- Mcmc({
        x <- 1
        y <- 2
    })
    samps.loaded <- LoadMcmc(test.dir)

    expect_equal(samps, samps.loaded)
    expect_equivalent(samps$x[,], samps.loaded$x[,])
    expect_equivalent(samps$y[,], samps.loaded$y[,])
})

test_that("InitMcmc samples overwritten if overwrite=TRUE", {
    iter <- 2
    test.dir <- TestDir()

    Mcmc <- InitMcmc(iter, test.dir, overwrite=TRUE)
    samps.1 <- Mcmc({
        x <- 1
    })

    samps.2 <- Mcmc({
        x <- 2
    })
    expect_equivalent(samps.2$x[,], rep(2, iter))
})

test_that("InitMcmc samples not overwritten if overwrite=FALSE", {
    err.msg <- paste0("Backing file already exists in backing.path. ",
                      "Use overwrite=TRUE to replace.")
    iter <- 2
    test.dir <- TestDir()

    Mcmc <- InitMcmc(iter, test.dir, overwrite=FALSE)
    samps.1 <- Mcmc({
        x <- 1
    })

    expect_error(samps.2 <- Mcmc({ x <- 2 }), err.msg)
    expect_equivalent(samps.1$x[,], rep(1, iter))
    rm(samps.1)
    loaded.samps.1 <- LoadMcmc(test.dir)
    expect_equivalent(loaded.samps.1$x[,], rep(1, iter))
})

test_that("Default behavior for InitMcmc is overwrite=FALSE", {
    err.msg <- paste0("Backing file already exists in backing.path. ",
                      "Use overwrite=TRUE to replace.")
    iter <- 2
    test.dir <- TestDir()

    Mcmc <- InitMcmc(iter, test.dir)
    samps.1 <- Mcmc({
        x <- 1
    })

    expect_error(samps.2 <- Mcmc({ x <- 2 }), err.msg)
})

test_that("Function returned by InitMcmc can take overwrite argument", {
    iter <- 2
    test.dir <- TestDir()

    Mcmc <- InitMcmc(iter, test.dir)
    samps.1 <- Mcmc({
        x <- 1
    })

    samps.2 <- Mcmc({
        x <- 2
    }, overwrite=TRUE)
    expect_equivalent(samps.2$x[,], rep(2, iter))
})

test_that("Error message if no .desc files in backing.path for LoadMcmc", {
    test.dir <- TestDir()
    wrong.path.err.msg <-
        paste0("No '.desc' files found in ", test.dir)
    expect_error(LoadMcmc(test.dir), wrong.path.err.msg, fixed=TRUE)
})

test_that("In-progress file-backed MCMC can be viewed", {
    iter <- 10
    test.dir <- TestDir()

    # Make some fake samples
    x <- matrix(NA, nrow=iter, ncol=1)
    y <- matrix(NA, nrow=iter, ncol=2)
    x[1:8,] <- 1:8
    y[1:6,] <- 1:6
    y[6, 2] <- NA
    bigmemory::as.big.matrix(x, backingpath=test.dir, backingfile="x",
                             descriptorfile="x.desc")
    bigmemory::as.big.matrix(y, backingpath=test.dir, backingfile="y",
                             descriptorfile="y.desc")

    samps <- Peek(test.dir)
    expect_equivalent(samps$x[,], 1:5)
    expect_equivalent(samps$y[,], matrix(rep(1:5, times=2), nrow=5))
})

test_that("Error message if no .desc files in backing.path for Peek", {
    test.dir <- TestDir()
    wrong.path.err.msg <-
        paste0("No '.desc' files found in ", test.dir)
    expect_error(Peek(test.dir), wrong.path.err.msg, fixed=TRUE)
})

test_that("ToMemory converts MCMC samples to in-memory", {
    iter <- 3
    test.dir <- TestDir()

    Mcmc <- InitMcmc(iter, test.dir)
    x <- c(0, 0)
    y <- 0
    samps <- Mcmc({
        x <- c(1, 2)
        y <- 1
    })
    in.memory.samps <- ToMemory(samps)

    # Check that the samples are the same
    expect_equivalent(samps$x[,], in.memory.samps$x)
    expect_equivalent(samps$y[,], in.memory.samps$y)

    # Check that the samples are in-memory
    expect_true(is.matrix(in.memory.samps$x))
    expect_false(bigmemory::is.big.matrix(in.memory.samps$x))
    expect_true(is.matrix(in.memory.samps$y))
    expect_false(bigmemory::is.big.matrix(in.memory.samps$y))

    # Check that the dimensions are the same
    expect_true(all(dim(samps$x) == dim(in.memory.samps$x)))
    expect_true(all(dim(samps$y) == dim(in.memory.samps$y)))
})

test_that("File-backed MCMC is flushed on exit", {
    iter <- 3
    test.dir <- TestDir()

    m <- mockery::mock(TRUE, TRUE)
    Mcmc <- InitMcmc(iter, test.dir)
    with_mock(flush=m,
              samps <- Mcmc({ x <- c(1, 2)
                              y <- 1 }),
              .env="bigmemory")

    mockery::expect_called(m, 2)
    mockery::expect_args(m, 1, samps$x)
    mockery::expect_args(m, 2, samps$y)
})

test_that("Warning given if file-backed MCMC fails to flush on exit", {
    iter <- 3
    test.dir <- TestDir()
    flush.failed.warning.msg <-
        "Failed to flush big.matrix, some samples may not be saved on-disk"

    m <- mockery::mock(FALSE)
    Mcmc <- InitMcmc(iter, test.dir)
    expect_warning(with_mock(flush=m,
                             samps <- Mcmc({ x <- c(1, 2)}),
                             .env="bigmemory"),
                   flush.failed.warning.msg)
    mockery::expect_called(m, 1)
})

test_that("Peek doesn't fail if the MCMC is complete", {
    n.iter <- 5
    SampleSomething <- function() 1

    backing.path <- TestDir()

    Mcmc <- InitMcmc(n.iter, backing.path=backing.path)
    Mcmc({
        x <- SampleSomething()
    })

    samples.so.far <- Peek(backing.path)
    expect_equivalent(samples.so.far$x[,], rep(1, n.iter))
})

test_that("Parameter dimensions can change if overwrite=TRUE", {
    skip_on_os("windows") # file.remove doesn't always work on Windows
    n.iter <- 5
    backing.path <- TestDir()

    f <- function() 1
    Mcmc <- InitMcmc(n.iter, backing.path=backing.path, overwrite=TRUE)
    samps <- Mcmc({
        x <- f()
    })

    f <- function() c(2, 2)
    samps <- Mcmc({
        x <- f()
    })
    expect_equivalent(samps$x[,], matrix(rep(c(2, 2), each=n.iter),
                                         nrow=n.iter, ncol=2))
})

test_that("Error message given if backing file can't be removed", {
    skip_on_os("windows") # file.remove doesn't always work on Windows
    n.iter <- 5
    backing.path <- TestDir()
    file.rm.err.msg <- paste0("Failed to remove ", file.path(backing.path, "x"),
                              ".  Try removing the file manually.")

    f <- function() 1
    Mcmc <- InitMcmc(n.iter, backing.path=backing.path, overwrite=TRUE)
    samps <- Mcmc({
        x <- f()
    })

    m <- mockery::mock(FALSE)
    # mockery::stub(Mcmc, 'FileRemove', m, depth=10)
    f <- function() c(2, 2)

    expect_error(with_mock(file.remove=m,
                           samps <- Mcmc({ x <- f() })),
                 file.rm.err.msg,
                 fixed=TRUE
    )
    mockery::expect_called(m, 1)
})

test_that("Error message given if .desc file isn't found and overwrite=TRUE", {
    n.iter <- 5
    backing.path <- TestDir()
    desc.rm.err.msg <- paste0("Tried to overwrite ", "x",
           " but couldn't find ", file.path(backing.path, "x.desc"),
           ". Try removing the old results manually.")

    f <- function() 1
    Mcmc <- InitMcmc(n.iter, backing.path=backing.path, overwrite=TRUE)
    samps <- Mcmc({
        x <- f()
    })

    file.remove(file.path(backing.path, "x.desc"))
    expect_error(samps <- Mcmc({ x <- f() }), desc.rm.err.msg, fixed=TRUE)
})

test_that("MCMC can be resumed", {
    n.iter <- 5
    SampleX <- function(x) x + 1
    SampleY <- function(y) y + 10
    backing.path <- TestDir()

    x <- 0
    y <- 0
    interrupt.mcmc <- TRUE
    Mcmc <- InitMcmc(n.iter, backing.path=backing.path)

    # Stop the MCMC in the middle of the third iteration
    testthat::expect_error(
    samps <- Mcmc({
        x <- SampleX(x)
        if(x==3 && interrupt.mcmc) break
        y <- SampleY(y)
    }))

    interrupt.mcmc <- FALSE
    samps <- Resume(backing.path)

    expect_equivalent(samps$x[,], 1:5)
    expect_equivalent(samps$y[,], seq(10, 50, by=10))
})

test_that("Resume doesn't fail if MCMC is already complete", {
    n.iter <- 5
    SampleX <- function(x) x + 1
    SampleY <- function(y) y + 10
    backing.path <- TestDir()

    x <- 0
    y <- 0
    Mcmc <- InitMcmc(n.iter, backing.path=backing.path)

    samps <- Mcmc({
        x <- SampleX(x)
        y <- SampleY(y)
    })
    samps <- Resume(backing.path)

    expect_equivalent(samps$x[,], 1:5)
    expect_equivalent(samps$y[,], seq(10, 50, by=10))
})

test_that("Resume works if thinning is set", {
    n.iter <- 5
    thin <- 2
    SampleX <- function(x) x + 1
    SampleY <- function(y) y + 10
    backing.path <- TestDir()

    x <- 0
    y <- 0
    interrupt.mcmc <- TRUE
    Mcmc <- InitMcmc(n.iter, backing.path=backing.path, thin=thin)

    # Stop the MCMC in the middle of the run
    testthat::expect_error(
    samps <- Mcmc({
        if(x==3 && interrupt.mcmc) break
        x <- SampleX(x)
        y <- SampleY(y)
    }))

    interrupt.mcmc <- FALSE
    samps <- Resume(backing.path)

    expect_equivalent(samps$x[,], seq(2, 10, by=2))
    expect_equivalent(samps$y[,], seq(20, 100, by=20))
})
