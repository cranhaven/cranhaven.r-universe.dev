test_that("tabTableTwo returns the correct object with two tables", {
  # Dummy datatable
  dummy_data1 <- data.frame(
    A = 1:5,
    B = letters[1:5]
  )

  dummy_data2 <- data.frame(
    X = 6:10,
    Y = letters[6:10]
  )

  dummy_dt1 <- DT::datatable(dummy_data1)
  dummy_dt2 <- DT::datatable(dummy_data2)

  # Run the function
  test_obj <- tabTableTwo("dummy_id", dummy_dt1, dummy_dt2)

  # Check the class of the output
  expect_s3_class(test_obj, "shiny.tag")

  # Check the name of the tag
  expect_equal(test_obj$name, "div")

  # Check the list of class attributes
  expect_true("tab-pane" %in% test_obj$attribs$class)

  # Check for correct number of children in the fluidRow (i.e., two columns)
  expect_equal(length(test_obj$children[[1]]$children), 2)
})
