test_that("taskItemTab creates the correct list item with or without tab selection", {
  # Run the function with tabSelect = TRUE
  test_obj1 <- taskItemTab(text = "My Task", tab_name = "Tab1", tabSelect = TRUE)

  # Check the class of the output
  expect_s3_class(test_obj1, "shiny.tag")

  # Check the name of the tag
  expect_equal(test_obj1$name, "li")

  # Check the 'data-tab-name' attribute
  expect_equal(test_obj1$children[[1]]$attribs$`data-tab-name`, "Tab1")

  # Check the 'onclick' attribute
  expect_equal(test_obj1$children[[1]]$attribs$onclick, "shinyjs.tabSelect('Tab1')")

  # Run the function with tabSelect = FALSE
  test_obj2 <- taskItemTab(text = "My Task", href = "#link", tabSelect = FALSE)

  # Check the class of the output
  expect_s3_class(test_obj2, "shiny.tag")

  # Check the name of the tag
  expect_equal(test_obj2$name, "li")

  # Check the 'href' attribute
  expect_equal(test_obj2$children[[1]]$attribs$href, "#link")
})
