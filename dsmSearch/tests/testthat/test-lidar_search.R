testthat::test_that("runs correctly", {

  # search for lidar data information using bbox
  search_result <- dsmSearch::lidar_search(bbox = c(-83.742282,
                                                    42.273389,
                                                    -83.733442,
                                                    42.278724),
                                           preview = FALSE)

  testthat::expect_type(search_result, "list")
})
