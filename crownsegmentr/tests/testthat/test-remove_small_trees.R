#  test-remove_small_trees

test_that("The function works on a point cloud data table", {
  mini_point_cloud <- data.frame(
    X = c(1, 1, 2, 3, 3, 4, 4, 5),
    Y = c(1, 5, 3, 1, 5, 3, 5, 5),
    Z = c(8, 8, 8, 8, 8, 8, 8, 8),
    crown_id = c(1, NA, 3, 3, 3, 3, 4, 4)
  )

  processed_point_cloud <- remove_small_trees(mini_point_cloud)

  expect_true(is.na(processed_point_cloud[1, "crown_id"]))
  expect_equal(processed_point_cloud[5, "crown_id"][[1]], 1)

  # vary min_height
  processed_point_cloud <- remove_small_trees(mini_point_cloud,
    min_height = 10
  )

  expect_equal(
    sum(is.na(processed_point_cloud$crown_id)),
    nrow(mini_point_cloud)
  )

  # Vary min_radius
  processed_point_cloud <- remove_small_trees(mini_point_cloud,
    min_radius = 5,
    min_height = 3
  )

  expect_equal(
    sum(is.na(processed_point_cloud$crown_id)),
    nrow(mini_point_cloud)
  )

  # vary crown_id_column_name
  names(mini_point_cloud) <- c("X", "Y", "Z", "foo")
  processed_point_cloud <- remove_small_trees(mini_point_cloud,
    crown_id_column_name = "foo"
  )
  expect_true(is.na(processed_point_cloud[1, "foo"]))
})




test_that("Function works with real-world LAS file", {
  # read and segment a point cloud for use in the function

  test_point_cloud_file_path <- system.file(
    "extdata", "MixedConifer.laz",
    package = "lidR"
  )
  test_point_cloud_header <- lidR::readLASheader(test_point_cloud_file_path)
  test_point_cloud <- lidR::clip_rectangle(
    lidR::readLAS(test_point_cloud_file_path),
    xleft = test_point_cloud_header@PHB$`Min X`,
    ybottom = test_point_cloud_header@PHB$`Min Y`,
    xright = test_point_cloud_header@PHB$`Min X` + 30,
    ytop = test_point_cloud_header@PHB$`Min Y` + 30
  )
  # segment point cloud as raw material
  segmented_las <- segment_tree_crowns(test_point_cloud,
    crown_diameter_to_tree_height = 0.2,
    crown_length_to_tree_height = 0.6,
    crown_diameter_constant = 2,
    crown_length_constant = 1
  )

  processed_las <- remove_small_trees(segmented_las)

  expect_true(length(unique(processed_las@data$crown_id)) <
    length(unique(segmented_las@data$crown_id)))
})

#
# test_that("Function works with LasCatalog",{
#   # TODO Check whether enabling parallelization like this is CRAN conform
#   # On some machines, using future::multisession without specifying number of
#   # workers produces errors
#   future::plan(strategy = future::multisession(workers = future::availableCores()))
#
#   # Load real point cloud data
#   test_catalog <- lidR::readLAScatalog(
#     system.file("extdata", "MixedConifer.laz", package = "lidR"),
#     chunk_size = 57,
#     chunk_buffer = 20
#   )
#
#   segmented_catalog <- segment_tree_crowns(
#     test_catalog,
#     crown_diameter_to_tree_height = 0.1,
#     crown_length_to_tree_height = 0.3,
#     crown_diameter_constant = 4,
#     crown_length_constant = 3,
#     crown_id_column_name = "tree_id"
#   )
# })
