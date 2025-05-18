
context("Convert snake case variables to Stics variables")

test_that("With numerical (2 digits) or text index", {
  expect_equal(col_names_to_var("name_10"), "name(10)")
  expect_equal(col_names_to_var("name.10."), "name(10)")
  expect_equal(col_names_to_var("name_n"), "name(n)")
  expect_equal(col_names_to_var("name.n."), "name(n)")
  expect_false(col_names_to_var("name_100") == "name(100)")

})

test_that("With multiple numbers", {
  expect_equal(col_names_to_var("name_10_20"), "name_10_20")
})

test_that("With multiple underscores, index", {
  expect_equal(col_names_to_var("msrec_fou_coupe"), "msrec_fou_coupe")
  expect_equal(col_names_to_var("msrec.fou.coupe."), "msrec.fou.coupe.")
  expect_equal(col_names_to_var("SoilAvW_by_layers_5"),
               "SoilAvW_by_layers(5)")
  expect_equal(col_names_to_var("SoilAvW_by_layers.5."),
               "SoilAvW_by_layers(5)")

})

context("Convert all snake case variables to Stics variables")


dir_csv <- get_examples_path("csv")
outputs_file <- file.path(dir_csv, "outputs.csv")
df_outputs <- read.csv2(file = outputs_file,
                        header = TRUE,
                        stringsAsFactors = FALSE)
snake_names <- gsub(pattern = "\\(", x = df_outputs$Name, replacement = "_")
snake_names <- gsub(pattern = "\\)", x = snake_names, replacement = "")

converted <- col_names_to_var(snake_names)

test_that("Converted match original names", {
  expect_true(all(converted == df_outputs$Name))
})
