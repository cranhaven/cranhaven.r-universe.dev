test_that("Shiny specific function call chunks return an empty Repro object", {
  validate_call <- str2lang("validate(is.data.frame(iris))")
  class(validate_call) <- c("validate", class(validate_call))

  validate_repro <- repro_call_chunk(validate_call)
  expect_identical(validate_repro, Repro())
})
