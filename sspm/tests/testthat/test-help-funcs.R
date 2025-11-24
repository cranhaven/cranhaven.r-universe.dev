# Testing helper functions

test_that("Methods choices are retrieved", {

  choices <- spm_methods()
  expect_character(choices)
  expect_names(choices, identical.to = c("tesselate_voronoi", "triangulate_delaunay"))

  smooth_choices <- spm_smooth_methods()
  expect_character(smooth_choices)
  expect_names(smooth_choices, identical.to = c("ICAR", "LINPRED"))

})

test_that("Aggregation choices are retrieved", {

  choices <- spm_aggregation_choices()
  expect_equal(choices, c('space', 'time', 'spacetime'))

})

test_that("Aggregation levels are retrieved", {

  choices <- spm_aggregation_levels_choices()
  expect_equal(choices, c("patch", "boundary"))

})

test_that("Aggregation types are retrieved", {

  choices <- spm_aggregation_types_choices()
  expect_equal(choices, c("data", "smoothed"))

})

test_that("Methods are dispatched correctly", {

  expect_equal(dispatch_method("tesselate_voronoi"), tesselate_voronoi)
  expect_equal(dispatch_method("triangulate_delaunay"), triangulate_delaunay)
  expect_error(dispatch_method("method_not_supported"),
               "Method 'method_not_supported' is not part of the supported methods.")

  expect_equal(dispatch_smooth("ICAR"), ICAR)
  expect_equal(dispatch_smooth("LINPRED"), LINPRED)
  expect_message(dispatch_smooth("method_not_supported"),
                 "Smoothing method 'method_not_supported' is not part of the supported methods.")

})

test_that("Warnings/messages can be suppressed", {

  expect_failure(expect_warning(suppressAll(warning("This is a warning"))))
  expect_failure(expect_message(suppressAll(message("This is a message"))))

})

test_that("Calls are modified correctly", {

  base_col <- str2lang("s()")
  modified_call <- modify_call(base_col, list(k = 1, bs = "mrf"))
  modified_call_str <- deparse(modified_call)

  expect_class(modified_call, "call")
  expect_match(modified_call_str, "s(k = 1, bs = \"mrf\")", fixed = TRUE)

})

test_that("Methods are correctly returned", {
  expect_class({spm_methods()}, "character")
  expect_length({spm_methods()}, 2)
})

test_that("Functons for methods are correctly dispatched", {
  expect_class({sspm:::dispatch_method("tesselate_voronoi")}, "function")
})

test_that("Multilag works", {
  expect_equal(multilag(c(1:5), 2),
               data.frame(lag1 = c(NA, 1, 2, 3, 4),
                          lag2 = c(NA, NA, 1, 2, 3)))
})

test_that("Datasets are correctly joined", {

  joined <- join_datasets(biomass_dataset, boundary_discrete)

  expect_true(is_mapped(joined))
  expect_true("patch_id" %in% names(spm_data(joined)))
  expect_equal(sum(is.na(spm_data(joined)$patch_id)), 0)
  expect_equal(spm_boundaries(joined), boundary_discrete)

})
