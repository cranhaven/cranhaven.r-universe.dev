test_that("generate_treatment_list() correctly generates a list of treatment labels", {
  # Test case 1: Valid input
  expect_equal(generate_treatment_list(1:12, "vertical"),
               list(`1` = 1, `2` = 2, `3` = 3, `4` = 4, `5` = 5, `6` = 6, `7` = 7, `8` = 8, `9` = 9, `10` = 10, `11` = 11, `12` = 12))
  expect_equal(generate_treatment_list(1:8, "horizontal"),
               list(A = 1, B = 2, C = 3, D = 4, E = 5, F = 6, G = 7, H = 8))
  
  # Test 2: Invalid input
  expect_error(generate_treatment_list(1:8, "vertical"),
               "Number of treatment labels must match the number of rows or columns as chosen by the direction parameter.")
  expect_error(generate_treatment_list(1:12, "horizontal"),
               "Number of treatment labels must match the number of rows or columns as chosen by the direction parameter.")
})