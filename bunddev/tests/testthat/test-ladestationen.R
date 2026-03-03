test_that("ladestationen query returns a tibble", {
  skip_if_offline()
  skip_on_cran()

  token <- Sys.getenv("LADESTATIONEN_TOKEN")

  geometry <- jsonlite::toJSON(
    list(
      xmin = 13.3,
      ymin = 52.4,
      xmax = 13.5,
      ymax = 52.6,
      spatialReference = list(wkid = 4326)
    ),
    auto_unbox = TRUE
  )

  params <- list(
    geometry = geometry,
    geometryType = "esriGeometryEnvelope",
    where = "1=1",
    outFields = "*",
    outSR = 4326,
    f = "json",
    returnGeometry = "false",
    resultRecordCount = 1
  )
  if (token != "") {
    params$token <- token
  }

  results <- tryCatch(
    ladestationen_query(params = params),
    error = function(e) {
      if (grepl("Token Required", conditionMessage(e))) {
        skip("Token required by ArcGIS service")
      }
      stop(e)
    }
  )

  expect_s3_class(results, "tbl_df")
})
