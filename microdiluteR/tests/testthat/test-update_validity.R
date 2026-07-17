# Generate test data
test_data <- data.frame(
  Position = c("A1", "A2", "B1", "B2"),
  Value = 1:4,
  Validity = "valid",
  Group_A = rep(LETTERS[1:2], each = 2),
  Group_B = rep(letters[1:2], times = 2),
  stringsAsFactors = FALSE
)

# Test case 1
test_that("update_validity() correctly labels specified well positions as invalid", {
  
  # Generate input and expected result
  well_positions <- c("A1", "B1")
  expected_output <- data.frame(
    Position = c("A1", "A2", "B1", "B2"),
    Value = 1:4,
    Validity = c("invalid", "valid", "invalid", "valid"),
    Group_A = rep(LETTERS[1:2], each = 2),
    Group_B = rep(letters[1:2], times = 2),
    stringsAsFactors = FALSE
  )
  
  # Test
  expect_equal(update_validity(test_data, well_positions = well_positions), expected_output)
})

# Test case 2
test_that("update_validity() correctly ignores well positions not in data", {
  
  # Generate input and expected result
  well_positions <- c("C1", "D1")
  expected_output <- data.frame(
    Position = c("A1", "A2", "B1", "B2"),
    Value = 1:4,
    Validity = "valid",
    Group_A = rep(LETTERS[1:2], each = 2),
    Group_B = rep(letters[1:2], times = 2),
    stringsAsFactors = FALSE
  )
  
  # Test
  expect_equal(update_validity(test_data, well_positions = well_positions), expected_output)
})

# Test case 3
test_that("update_validity correctly matches group levels and sets corresponding well positions as invalid", {
  
  # Generate input and expected result
  well_positions <- c("A2", "B2")
  group_levels <- list(Group_A = c("A", "B"), Group_B = "b")
  expected_output <- data.frame(
    Position = c("A1", "A2", "B1", "B2"),
    Value = c(1, 2, 3, 4),
    Validity = c("valid", "invalid", "valid", "invalid"),
    Group_A = rep(LETTERS[1:2], each = 2),
    Group_B = rep(letters[1:2], times = 2),
    stringsAsFactors = FALSE
  )
  
  # Test
  expect_equal(update_validity(test_data,
                               well_positions = well_positions,
                               group_levels = group_levels),
               expected_output)
})
