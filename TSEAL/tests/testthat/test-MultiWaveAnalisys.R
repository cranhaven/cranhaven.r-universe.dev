test_that("test MultiWaveAnalisysVarCOR", {
    NVar <- 30
    NCor <- 135

    testExperiments <- readRDS("../testExperiments.rds")
    MedicalClasification <- c(1, 1, 2, 2)

    MWA <- MultiWaveAnalysis(testExperiments, "haar")
    m <- read.csv("../Results/MWA.csv", header = FALSE)
    expect_equal(
        rbind(MWA$Features$Var, MWA$Features$Cor),
        as.matrix(m),
        tolerance = 0.01,
        ignore_attr = TRUE
    )
})

test_that("test MultiWaveAnalisysCor", {
    NVar <- 0
    NCor <- 135
    m <- read.csv("../Results/MWA.csv", header = FALSE)
    m <- m[31:(30 + NCor), ] # remove the correlations

    testExperiments <- readRDS("../testExperiments.rds")
    MedicalClasification <- c(1, 1, 2, 2)

    MWA <- MultiWaveAnalysis(testExperiments, "haar")

    expect_equal(MWA$Features$Cor,
                 as.matrix(m),
                 tolerance = 0.01,
                 ignore_attr = TRUE)
})

test_that("test MultiWaveAnalisysVar", {
    NVar <- 30
    NCor <- 0
    m <- read.csv("../Results/MWA.csv", header = FALSE)
    m <- m[1:NVar, ] # remove the correlations

    testExperiments <- readRDS("../testExperiments.rds")
    MedicalClasification <- c(1, 1, 2, 2)

    MWA <- MultiWaveAnalysis(testExperiments, "haar")

    expect_equal(MWA$Features$Var,
                 as.matrix(m),
                 tolerance = 0.01,
                 ignore_attr = TRUE)
})

test_that("test MultiWaveAnalisys Input errors", {
    testExperiments <- readRDS("../testExperiments.rds")

    expect_error(MultiWaveAnalysis(f = "haar"))
    expect_error(MultiWaveAnalysis(testExperiments))
    expect_error(MultiWaveAnalysis(testExperiments, f = "haar", features = c()))

    test2D <- testExperiments[, , 1]

    expect_error(MultiWaveAnalysis(test2D, f = "haar"))
    expect_error(MultiWaveAnalysis(testExperiments, f = "haar", lev = -1))
    expect_error(MultiWaveAnalysis(testExperiments, f = "haar", nCores = -1))


    expect_error(MultiWaveAnalysis(testExperiments, f = "haar", lev = 1.5))
    expect_error(MultiWaveAnalysis(testExperiments, f = "haar", nCores = 1.5))

    expect_error(MultiWaveAnalysis(
        testExperiments,
        f = "haar",
        features = c("var", "P")
    )) # Not supported feature
    expect_warning(MultiWaveAnalysis(testExperiments, f = "haar", lev = 100))
})

test_that("MultiWaveAnalisys one case", {
    testExperiments <- readRDS("../testExperiments.rds")
    MedicalClasification <- c(1, 1, 2, 2)

    oneData <- testExperiments[, , 1, drop = FALSE]
    MWA <-
        MultiWaveAnalysis(oneData, f = "haar", features = c("IQR"))

    nDimensions <- lapply(MWA$Features, function(x)
        length(dim(x)))

    expect_true(all(nDimensions == 2))
})

test_that("choose Level Input", {
    expect_error(chooseLevel(filter = "haar", N = 8))
    expect_error(chooseLevel(choice = "max", N = 8))
    expect_error(chooseLevel(choice = "max", filter = "haar"))
    expect_error(chooseLevel(
        choice = "p",
        filter = "haar",
        N = 8
    ))
    expect_error(chooseLevel(
        choice = "max",
        filter = "haar",
        N = 7.9
    ))
})
