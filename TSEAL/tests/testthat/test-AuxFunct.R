test_that("GenerateStepDiscrim", {
    testExperiments <- readRDS("../testExperiments.rds")
    grps <- c(1, 1, 2, 2)

    MWADiscrim <- generateStepDiscrim(
        testExperiments,
        grps,
        f = "haar",
        maxvars = 1,
        features = c("Var", "Cor"),
        nCores = 2
    )


    m <- read.csv("../Results/StepVar&Cor.csv", header = FALSE)
    expect_equal(sort(values(MWADiscrim)), sort(as.matrix(m)), ignore_attr = TRUE)
})

test_that("GenerateStepDiscrim Iputs", {
    load(system.file("extdata/ECGExample.rda",package = "TSEAL"))
    testExperiments <- ECGExample
    grps <- c(1, 1, 2, 2)

    expect_error(generateStepDiscrim(
        grps = grps,
        f = "haar",
        maxvars = 10
    ))
    expect_error(generateStepDiscrim(testExperiments, f = "haar", maxvars = 10))
    expect_error(generateStepDiscrim(testExperiments, "haar"))
    expect_error(generateStepDiscrim(
        testExperiments,
        "haar",
        maxvars = 10,
        VStep = 1
    ))
    expect_error(generateStepDiscrim(testExperiments, "haar", maxvars = 0))
    expect_error(generateStepDiscrim(testExperiments, "haar", VStep = 0))
})

# test_that("testFilters", {
load(system.file("extdata/ECGExample.rda",package = "TSEAL"))
testExperiments <- ECGExample
grps <- c(rep(1, 5), rep(2, 5))

data <- testFilters(
    testExperiments,
    grps,
    maxvars = 2,
    f = c("haar"),
    features = c("var", "cor")
)

r1 <- LOOCV(
    testExperiments,
    grps,
    f = "haar",
    features = c("var", "cor"),
    method = "linear",
    maxvars = 2,
    returnClassification = TRUE
)

r2 <- LOOCV(
    testExperiments,
    grps,
    f = "haar",
    features = c("var", "cor"),
    method = "quadratic",
    maxvars = 2,
    returnClassification = TRUE
)

r3 <- LOOCV(
    testExperiments,
    grps,
    f = "haar",
    features = c("var"),
    method = "linear",
    maxvars = 2,
    returnClassification = TRUE
)

expect_equal(r1$classification, data[[5]]$classification)
expect_equal(r2$classification, data[[6]]$classification)
expect_equal(r2$classification, data[[1]]$classification)

# })


test_that("testFilters Iputs", {
    testExperiments <- readRDS("../testExperiments.rds")
    grps <- c(1, 1, 2, 2)

    expect_error(testFilters(grps = grps, maxvars = 10))
    expect_error(testFilters(testExperiments, maxvars = 10))
    expect_error(testFilters(testExperiments, grps))

    expect_error(testFilters(
        testExperiments,
        grps,
        maxvars = 10,
        features = c()
    ))
    expect_error(testFilters(
        testExperiments,
        grps,
        maxvars = 10,
        filters = c()
    ))
})
