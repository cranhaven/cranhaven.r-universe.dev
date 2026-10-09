data("pt01Frag")

test_that("estimateSOZ", {
  estimateSOZ(pt01Frag) |> expect_no_error()
  estimateSOZ(pt01Frag, "max") |> expect_no_error()
  estimateSOZ(pt01Frag, "min") |> expect_no_error()
  estimateSOZ(pt01Frag, proportion = 0.2) |> expect_no_error()
})
