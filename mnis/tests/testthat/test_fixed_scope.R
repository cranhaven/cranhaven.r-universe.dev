

library(mnis)
context("fixed_scope")
test_that("fixed scope functions returns expected format", {
  skip_on_cran()

  xps <- mnis_party_state("2012-01-12")

  expect_type(xps, "list")
  expect_true(tibble::is_tibble(xps))
  expect_true("Bishops" %in% xps$name)

  xmd <- mnis_member_date(500)
  expect_type(xmd, "list")
  expect_true(tibble::is_tibble(xmd))

  xger <- mnis_general_election_results(location_type = "Country",
                                        location_name = "England",
                                        start_date = "2010-01-01",
                                        end_date = "2016-01-01")

  xger <- mnis_general_election_results(
    location_type = "Country",
    location_name = "England",
    start_date = "2010-01-01",
    end_date = "2016-01-01"
  )


  xlt <- mnis_lords_type(date = "2015-01-01")
  expect_length(xlt, 6)
  expect_type(xlt, "list")
  expect_true(tibble::is_tibble(xlt))
  expect_true(nrow(xlt) == 15)
})
