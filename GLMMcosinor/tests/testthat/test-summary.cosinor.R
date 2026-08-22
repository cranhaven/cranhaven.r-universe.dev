test_that("summary print works", {
  # withr::local_seed(42)
  # simple_model <- cglmm(
  #   vit_d ~ amp_acro(time, group = "X", period = 12),
  #   data = vitamind
  # )
  # saveRDS(simple_model, test_path("fixtures", "simple_model.rds"))
  # multi_model <- cglmm(
  #   vit_d ~ amp_acro(time, group = "X", period = 12),
  #   dispformula = ~ amp_acro(time, group = "X", period = 12),
  #   ziformula = ~ amp_acro(time, group = "X", period = 12),
  #   data = vitamind
  # )
  # saveRDS(multi_model, test_path("fixtures", "multi_model.rds"))

  simple_model <- readRDS(test_path("fixtures", "simple_model.rds"))
  multi_model <- readRDS(test_path("fixtures", "multi_model.rds"))

  print_obj <- summary(simple_model)
  expect_snapshot(print(print_obj, digits = 2))
  expect_s3_class(print_obj, "cglmmSummary")

  print_obj <- summary(multi_model)
  expect_snapshot(print(print_obj, digits = 1))
})
