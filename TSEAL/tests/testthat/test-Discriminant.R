test_that("trainModel", {
    testExperiments <- readRDS("../testExperiments.rds")
    MedicalClasification <- c(1, 1, 2, 2)

    resultArray <-
        trainModel(testExperiments,
                   MedicalClasification,
                   "haar",
                   "linear",
                   maxvars = 2)

    MWA <- MultiWaveAnalysis(testExperiments, f = "haar")
    MWADiscrim <- StepDiscrim(MWA, MedicalClasification, 2)

    resultMWA <-
        trainModel(MWADiscrim, MedicalClasification, "linear")

    expect_s3_class(resultArray, "WaveModel")
    expect_s3_class(resultMWA, "WaveModel")
    expect_equal(resultArray, resultMWA)
})

test_that("trainModel Input", {
    testExperiments <- readRDS("../testExperiments.rds")
    MedicalClasification <- c(1, 1, 2, 2)

    MWA <- MultiWaveAnalysis(testExperiments, f = "haar")
    MWADiscrim <- StepDiscrim(MWA, MedicalClasification, 2)

    expect_error(trainModel(MWA, MedicalClasification))
    expect_error(trainModel(MWA, method = "linear"))
    expect_error(trainModel(grps = MedicalClasification, method = "linear"))
    expect_error(trainModel(MWA, MedicalClasification, method = "NotSupported"))

    expect_error(trainModel(testExperiments, MedicalClasification, "haar",
                            "linear"))

    expect_error(
        trainModel(
            data = testExperiments,
            grps = MedicalClasification,
            f = "haar",
            maxvars = 2
        )
    )

    expect_error(
        trainModel(
            data = testExperiments,
            grps = MedicalClasification,
            method = "linear",
            maxvars = 2
        )
    )

    expect_error(trainModel(
        data = testExperiments,
        f = "haar",
        method = "linear",
        maxvars = 2
    ))

    expect_error(trainModel(
        grps = MedicalClasification,
        f = "haar",
        method = "linear",
        maxvars = 2
    ))

    expect_error(
        trainModel(
            data = testExperiments,
            grps = MedicalClasification,
            f = "haar",
            method = "linear",
            maxvars = 0
        )
    )

    expect_error(
        trainModel(
            data = testExperiments,
            grps = MedicalClasification,
            f = "haar",
            method = "linear",
            VStep = 0
        )
    )
})

test_that("testModel", {
    load(system.file("extdata/ECGExample.rda",package = "TSEAL"))
    grps <- c(rep(1, 5), rep(2, 5))
    MWA <-
        generateStepDiscrim(ECGExample, grps, "haar", maxvars = 3)
    aux <- extractSubset(MWA, c(1, 2, 9, 10))
    MWATest <- aux[[1]]
    MWATrain <- aux[[2]]
    ldaDiscriminant <- trainModel(MWATrain, grps[3:8], "linear")

    CM <- testModel(ldaDiscriminant, MWATest, grps[c(1, 2, 9, 10)])

    expect_equal(as.matrix(CM$table),
                 matrix(c(2, 0, 0, 2), nrow = 2, ncol = 2),
                 ignore_attr = TRUE)
})

test_that("testModel Input", {
    load(system.file("extdata/ECGExample.rda",package = "TSEAL"))
    grps <- c(rep(1, 5), rep(2, 5))
    MWA <-
        generateStepDiscrim(ECGExample, grps, "haar", maxvars = 3)
    aux <- extractSubset(MWA, c(1, 2, 9, 10))
    MWATest <- aux[[1]]
    MWATrain <- aux[[2]]
    ldaDiscriminant <- trainModel(MWATrain, grps[3:8], "linear")

    expect_error(testModel(model = ldaDiscriminant, test = MWATest))
    expect_error(testModel(model = ldaDiscriminant, grps = grps[c(1, 2, 9, 10)]))
    expect_error(testModel(test = MWATest, grps = grps[c(1, 2, 9, 10)]))
    expect_error(testModel(
        model = ldaDiscriminant,
        test = MWATest,
        grps = grps[c(1, 2, 8, 9, 10)]
    ))
})

test_that("LOOCV", {
    testExperiments <- readRDS("../testExperiments.rds")
    MedicalClasification <- c(1, 1, 2, 2)

    MWA <- MultiWaveAnalysis(testExperiments, f = "haar")
    MWADiscrim <- StepDiscrim(MWA, MedicalClasification, 1)

    resultMWA <- LOOCV(MWADiscrim, MedicalClasification, "linear")

    resultArray <- LOOCV(
        testExperiments,
        MedicalClasification,
        f = "haar",
        method = "linear",
        maxvars = 1
    )

    expect_equal(resultArray, resultMWA)
})

test_that("LOOCV Input", {
    testExperiments <- readRDS("../testExperiments.rds")
    MedicalClasification <- c(1, 1, 2, 2)

    MWA <- MultiWaveAnalysis(testExperiments, f = "haar")
    MWADiscrim <- StepDiscrim(MWA, MedicalClasification, 1)

    expect_error(LOOCV(MWA, MedicalClasification))
    expect_error(LOOCV(MWA, method = "linear"))
    expect_error(LOOCV(grps = MedicalClasification, method = "linear"))
    expect_error(LOOCV(MWA, MedicalClasification, method = "NotSupported"))

    expect_error(LOOCV(testExperiments, MedicalClasification, "haar",
                       "linear"))

    expect_error(LOOCV(
        data = testExperiments,
        grps = MedicalClasification,
        f = "haar",
        maxvars = 2
    ))

    expect_error(
        LOOCV(
            data = testExperiments,
            grps = MedicalClasification,
            method = "linear",
            maxvars = 2
        )
    )

    expect_error(LOOCV(
        data = testExperiments,
        f = "haar",
        method = "linear",
        maxvars = 2
    ))

    expect_error(LOOCV(
        grps = MedicalClasification,
        f = "haar",
        method = "linear",
        maxvars = 2
    ))

    expect_error(
        LOOCV(
            data = testExperiments,
            grps = MedicalClasification,
            f = "haar",
            method = "linear",
            maxvars = 0
        )
    )

    expect_error(
        LOOCV(
            data = testExperiments,
            grps = MedicalClasification,
            f = "haar",
            method = "linear",
            VStep = 0
        )
    )
})

test_that("KFCV", {
    testExperiments <- readRDS("../testExperiments.rds")
    MedicalClasification <- c(1, 1, 2, 2)

    MWA <- MultiWaveAnalysis(testExperiments, f = "haar")
    MWADiscrim <- StepDiscrim(MWA, MedicalClasification, 1)

    resultMWA <- KFCV(MWADiscrim, MedicalClasification, "linear", 4)

    resultArray <- KFCV(
        testExperiments,
        MedicalClasification,
        k = 4,
        f = "haar",
        method = "linear",
        maxvars = 1
    )

    expect_equal(resultArray, resultMWA)
})

test_that("KFCV Input", {
    testExperiments <- readRDS("../testExperiments.rds")
    MedicalClasification <- c(1, 1, 2, 2)

    MWA <- MultiWaveAnalysis(testExperiments, f = "haar")
    MWADiscrim <- StepDiscrim(MWA, MedicalClasification, 1)

    expect_error(KFCV(MWA, MedicalClasification))
    expect_error(KFCV(MWA, method = "linear"))
    expect_error(KFCV(grps = MedicalClasification, method = "linear"))
    expect_error(KFCV(
        MWA,
        MedicalClasification,
        k = 4,
        method = "NotSupported"
    ))

    expect_error(KFCV(testExperiments, MedicalClasification, "haar",
                      "linear"))

    expect_error(KFCV(
        data = testExperiments,
        grps = MedicalClasification,
        f = "haar",
        maxvars = 2
    ))

    expect_error(
        KFCV(
            data = testExperiments,
            grps = MedicalClasification,
            method = "linear",
            maxvars = 2
        )
    )

    expect_error(KFCV(
        data = testExperiments,
        f = "haar",
        method = "linear",
        maxvars = 2
    ))

    expect_error(KFCV(
        grps = MedicalClasification,
        f = "haar",
        method = "linear",
        maxvars = 2
    ))

    expect_error(
        KFCV(
            data = testExperiments,
            grps = MedicalClasification,
            f = "haar",
            method = "linear",
            maxvars = 0
        )
    )

    expect_error(
        KFCV(
            data = testExperiments,
            grps = MedicalClasification,
            f = "haar",
            method = "linear",
            VStep = 0
        )
    )

    expect_error(
        KFCV(
            testExperiments,
            MedicalClasification,
            k = 0,
            f = "haar",
            method = "linear",
            maxvars = 1
        )
    )
})
