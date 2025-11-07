test_that("dropdownTabMenu creates the correct dropdown menu", {
  # Run the function
  test_obj <- dropdownTabMenu(type = "messages", title = "My Messages", header = "Header")

  # Check the class of the output
  expect_s3_class(test_obj, "shiny.tag")

  # Check the name of the tag
  expect_equal(test_obj$name, "li")

  # Check that the dropdown type is "messages"
  dropdown_class <- paste0("dropdown ", "messages", "-menu")
  expect_equal(test_obj$attribs$class, dropdown_class)

  # Check the header of the dropdown menu
  expect_equal(test_obj$children[[2]]$children[[1]]$children[[1]], "Header")
})
