#test_that("Valid table ID returns a list", {
#  expect_type(get_table_dimensions("NM_1_1"), "list")
#}
#)

test_that("Valid query returning over 25,000 rows sends a warning", {
  expect_error(get_table_dimensions("NM_10005_1"))
}
)



test_that("Valid table ID returns a list", {
  expect_type(get_table_dimensions("NM_102_1"), "list")
}
)


test_that("Valid table ID returns a list", {
  expect_type(get_table_dimensions("NM_1042_1"), "list")
}
)