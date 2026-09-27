test_that("When passed a generic string, it gets added to the code slot of a Repro object", {
  repro_obj <- repro_chunk("foo")

  expect_s7_class(repro_obj, Repro)
  expect_identical(repro_obj@code, list("foo"))
  expect_identical(repro_obj@packages, character(0L))
  expect_identical(repro_obj@prerequisites, list())
  expect_identical(repro_obj@script, "\"foo\"")
})
