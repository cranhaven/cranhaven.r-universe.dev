test_that("eco_visio counters returns a tibble", {
  skip_if_offline()
  skip_on_cran()

  results <- eco_visio_counters(4586)

  expect_s3_class(results, "tbl_df")
  expect_gt(nrow(results), 0)
})

test_that("eco_visio data returns a tibble", {
  skip_if_offline()
  skip_on_cran()

  counters <- eco_visio_counters(4586)
  skip_if(nrow(counters) == 0, "No counters available")

  first_counter <- counters$id[1]
  flow_ids <- counters$flow_ids[[1]]
  skip_if(length(flow_ids) == 0, "No flow IDs available")

  first_flow <- if (is.list(flow_ids)) flow_ids[[1]] else flow_ids[1]

  results <- eco_visio_data(
    id_organisme = 4586,
    id_pdc = first_counter,
    interval = 4,
    flow_ids = first_flow
  )

  expect_s3_class(results, "tbl_df")
})
