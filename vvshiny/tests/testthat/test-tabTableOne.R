
test_that("tabTableOne returns the correct object", {
  # create a dummy datatable
  dummy_data <- data.frame(
    A = 1:5,
    B = letters[1:5]
  )

  dummy_dt <- DT::datatable(dummy_data)

  # Run the function
  test_obj <- tabTableOne("dummy_id", dummy_dt)

  # Check the class of the output
  expect_s3_class(test_obj, "shiny.tag")

  # Check the name of the tag
  expect_equal(test_obj$name, "div")

  # Check the list of class attributes
  expect_true("tab-pane" %in% test_obj$attribs$class)
})

