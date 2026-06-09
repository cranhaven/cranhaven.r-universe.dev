if(require("psychTools")) {
  agreeableness = psychTools::bfi[1:100, c("A1", "A2", "A3", "A4", "A5")]
  agreeableness[, "A1"] = 7 - agreeableness[, "A1"] # Reverse-coded item.
  object = suppressWarnings(conogive(agreeableness))

  test_that("congive works on data theoretical input.", {
    object$cuts = qnorm(seq(0, 1, length.out = 6 + 1))
    obj = conogive(object, weights = "optimal")
    expect_equal(obj$lambda, object$lambda)
  })

  test_that("predict for optimal is not the same as predict for equal", {
    expect_true(predict(object, agreeableness[1, ]) !=
                  predict(object, agreeableness[1, ], weights = "equal"))
  })

  test_that("predict for vector same as for array", {
    expect_equal(predict(object, as.numeric(agreeableness[1, ])),
                 predict(object, agreeableness[1, ]))
  })

  test_that("theoretical_ordinal_r equal smaller than theoretical_ordinal_r optimal", {
    expect_gt(ordinal_r(object), ordinal_r(object, weights = "equal"))
    expect_gt(theoretical_ordinal_r(object), theoretical_ordinal_r(object, weights = "equal"))
  })

  test_that("theoretical_ordinal_r optimal default", {
    expect_equal(ordinal_r(object, weights = "optimal"),
                 ordinal_r(object))
    expect_equal(theoretical_ordinal_r(object, weights = "optimal"),
                 theoretical_ordinal_r(object))
  })
}
