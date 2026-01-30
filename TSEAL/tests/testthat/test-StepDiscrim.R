test_that("StepDiscrim", {
    testExperiments <- readRDS("../testExperiments.rds")
    MedicalClasification <- c(1, 1, 2, 2)

    MWA <- MultiWaveAnalysis(testExperiments, "haar", nCores = 2)
    MWA2 <-
        StepDiscrim(MWA, MedicalClasification, 1, c("Var", "Cor"), nCores = 2)

    m <- read.csv("../Results/StepVar&Cor.csv", header = FALSE)
    expect_equal(sort(values(MWA2)), sort(as.matrix(m)), ignore_attr = TRUE)
})

test_that("StepDiscrimVar", {
    testExperiments <- readRDS("../testExperiments.rds")
    MedicalClasification <- c(1, 1, 2, 2)

    MWA <- MultiWaveAnalysis(testExperiments, "haar", nCores = 2)
    MWA2 <-
        StepDiscrim(MWA, MedicalClasification, 2, c("Var"), nCores = 2)


    m <- read.csv("../Results/StepVar.csv", header = FALSE)
    expect_equal(sort(values(MWA2)), sort(as.matrix(m)), ignore_attr = TRUE)
})

test_that("StepDiscrimCor", {
    testExperiments <- readRDS("../testExperiments.rds")
    MedicalClasification <- c(1, 1, 2, 2)

    MWA <- MultiWaveAnalysis(testExperiments, "haar", nCores = 2)
    MWA2 <-
        StepDiscrim(MWA, MedicalClasification, 1, c("Cor"), nCores = 2)

    m <- read.csv("../Results/StepCor.csv", header = FALSE)
    expect_equal(sort(values(MWA2)), sort(as.matrix(m)), ignore_attr = TRUE)
})

test_that("StepDiscrim", {
    testExperiments <- readRDS("../testExperiments.rds")
    MedicalClasification <- c(1, 1, 2, 2)

    MWA <- MultiWaveAnalysis(testExperiments, "haar", nCores = 2)
    MWADiscrim <- StepDiscrim(MWA, MedicalClasification, maxvars = 3)
    MWADiscrimV <- StepDiscrimV(MWA, MedicalClasification, VStep = 1)

    expect_equal(MWADiscrim, MWADiscrimV)
})

test_that("SameDiscrim", {
    testExperiments <- readRDS("../testExperiments.rds")
    MedicalClasification <- c(1, 1, 2, 2)

    MWA <- MultiWaveAnalysis(testExperiments, "haar", nCores = 2)
    MWADiscrim <- StepDiscrim(MWA, MedicalClasification, maxvars = 3)
    MWASameDiscrim <- SameDiscrim(MWA, MWADiscrim)

    expect_equal(MWADiscrim, MWASameDiscrim)
})
