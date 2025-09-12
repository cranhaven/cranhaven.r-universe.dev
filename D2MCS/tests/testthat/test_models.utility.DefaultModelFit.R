testthat::test_that("DefaultModelFit: initialize function works", {

  testthat::expect_is(DefaultModelFit$new(), "DefaultModelFit")

})

testthat::test_that("DefaultModelFit: createFormula function works", {

  instances <- data.frame(c(1, 2), c(2, 2))

  colnames(instances) <- c("C1", "Class")

  testthat::expect_is(DefaultModelFit$new()$createFormula(instances, "Class",
                                                          simplify = FALSE),
                        "formula")

  testthat::expect_is(DefaultModelFit$new()$createFormula(instances, "Class",
                                                          simplify = TRUE),
                        "formula")
})

testthat::test_that("DefaultModelFit: createRecipe function works", {

  instances <- data.frame(c(1, 2), c(2, 2))

  colnames(instances) <- c("C1", "Class")

  testthat::expect_is(DefaultModelFit$new()$createRecipe(instances, "Class"),
                        "recipe")
})
