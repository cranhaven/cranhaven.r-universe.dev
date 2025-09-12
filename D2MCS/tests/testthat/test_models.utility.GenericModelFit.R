testthat::test_that("GenericModelFit: initialize function works", {

  instances <- data.frame(c(1, 2), c(2, 2))

  colnames(instances) <- c("C1", "Class")

  testthat::expect_is(GenericModelFit$new(), "GenericModelFit")

})

testthat::test_that("GenericModelFit: createFormula function works", {

  instances <- data.frame(c(1, 2), c(2, 2))

  colnames(instances) <- c("C1", "Class")

  testthat::expect_error(GenericModelFit$new()$createFormula(instances = instances,
                                                             class.name = "Class"),
                         "[GenericModelFit][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("GenericModelFit: createRecipe function works", {

  instances <- data.frame(c(1, 2), c(2, 2))

  colnames(instances) <- c("C1", "Class")

  testthat::expect_error(GenericModelFit$new()$createRecipe(instances = instances,
                                                            class.name = "Class"),
                         "[GenericModelFit][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})
