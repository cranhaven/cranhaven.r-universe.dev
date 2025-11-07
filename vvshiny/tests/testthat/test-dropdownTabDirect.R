test_that("dropdownTabDirect creates the correct dropdown menu with tab selection", {
  # Run the function
  test_obj <- dropdownTabDirect(type = "messages", tab_name = "Tab1", title = "Interesting tab")

  # Check the class of the output
  expect_s3_class(test_obj, "shiny.tag")

  # Check the name of the tag
  expect_equal(test_obj$name, "li")

  # Check that the dropdown type is "messages"
  dropdown_class <- paste0("dropdown ", "messages", "-menu")
  expect_equal(test_obj$attribs$class, dropdown_class)

  # Check the 'data-tab-name' attribute
  expect_equal(test_obj$children[[1]]$attribs$`data-tab-name`, "Tab1")

  # Check the 'onclick' attribute
  expect_equal(test_obj$children[[1]]$attribs$onclick, "shinyjs.tabSelect('Tab1')")
})
