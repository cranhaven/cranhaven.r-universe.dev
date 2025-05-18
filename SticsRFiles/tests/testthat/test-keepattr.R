stics_version <- get_stics_versions_compat()$latest_version

workspace <- get_examples_path(file_type = "sti", stics_version = stics_version)
situations <- SticsRFiles::get_usms_list(file = file.path(
  workspace,
  "usms.xml"
))
sim <- SticsRFiles::get_sim(workspace = workspace, usm = situations)

test_that("cropr_simulation attribute is kept", {
  expect_s3_class(sim, "cropr_simulation")
})
