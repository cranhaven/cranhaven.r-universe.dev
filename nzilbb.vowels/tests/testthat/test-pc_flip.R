test_that("Bad `pc_no` rejected", {
  expect_error(
    pc_flip(1, "bad_pc_no"),
    "numeric"
  )
})

test_that(
  "`pc_flip` results unchanged for onze_intercepts.",
  {
    pca_obj <- prcomp(onze_intercepts |> dplyr::select(-speaker), scale=TRUE)
    expect_snapshot(
      pc_flip(pca_obj, pc_no = 3, flip_var = "F1_GOOSE")
    )
  }
)
