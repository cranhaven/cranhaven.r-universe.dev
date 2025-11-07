
test_that("prep_table function works correctly", {

  ## Generate some mock data for testing
  df <- data.frame(VIS_Groep = c("Group1", "Group1", "Group2", "Group2"),
                   VIS_Groep_naam = c("Name1", "Name1", "Name2", "Name2"),
                   y = c(TRUE, TRUE, FALSE, FALSE), z = c(TRUE, FALSE, TRUE, FALSE))
  df_summarized <- df %>%
    dplyr::group_by(VIS_Groep, VIS_Groep_naam) %>%
      dplyr::summarise(
      y = mean(y),
      z = mean(z)
    ) %>%
    dplyr::ungroup()

  id = "id"

  ## Test with y_right NULL
  output <- prep_table("y", df, df_summarized, id)

  # Check output type
  expect_s3_class(output, "datatables")
  expect_s3_class(output, "htmlwidget")

  # Check lay-out
  expect_equal(output$x$options$buttons, list("excel", "pdf", "print"))
  expect_equal(output$x$caption, "<caption>Name1</caption>")

  ## Test with y_right not NULL
  df_complicated <- dplyr::mutate(df, y_right = c(TRUE, FALSE, FALSE, FALSE))
  df_complicated_summarized <- df_complicated %>%
    dplyr::group_by(VIS_Groep, VIS_Groep_naam) %>%
    dplyr::summarise(
      y = mean(y),
      z = mean(z),
      y_right = mean(y_right)
    ) %>%
    dplyr::ungroup()

  output_complicated <-
    prep_table("y",
               df_complicated,
               df_complicated_summarized,
               id,
               "y_right",
               filter = "top")

  # Check output type
  expect_s3_class(output_complicated, "datatables")
  expect_s3_class(output_complicated, "htmlwidget")

  ## Check added filter
  expect_true("filterHTML" %in% names(output_complicated$x))

  # Check y right
  expect_equal(output_complicated$x$data$y_right, c(0.5, 0.0))

})
