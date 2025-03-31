test_that("graphCohort", {
  hosptalised <- tibble::tribble(~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
                                                    2L,           5,       "2019-01-01",     "2019-01-10",
                                                    2L,           2,       "2016-01-01",     "2016-01-10",
                                                    2L,           6,       "2020-01-01",     "2020-01-10",
                                                    2L,           7,       "2021-01-01",     "2021-01-10",
                                                    2L,           3,       "2017-01-01",     "2017-01-10",
                                                    2L,           4,       "2018-01-01",     "2018-01-02",
                                                    2L,           1,       "2015-01-01",     "2015-01-10")

  icu_patients <- tibble::tribble(~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
                                                     5L,           6,       "2020-01-04",     "2020-01-08",
                                                     5L,           5,       "2019-01-03",     "2019-01-05",
                                                     5L,           3,       "2017-01-02",     "2017-01-04",
                                                     5L,           7,       "2021-01-01",     "2021-01-10",
                                                     5L,           2,       "2016-01-02",     "2016-01-04",
                                                     5L,           4,       "2018-01-02",     "2018-01-04")

  drugs_treatment <- tibble::tribble(~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
                                                        6L,          3L,       "2017-01-03",     "2017-01-05",
                                                        4L,          6L,       "2020-01-01",     "2020-01-02",
                                                        4L,          6L,       "2020-01-05",     "2020-01-06",
                                                        2L,          5L,       "2019-01-02",     "2019-01-04",
                                                        7L,          1L,       "2015-01-02",     "2015-01-04",
                                                       12L,          4L,       "2018-01-07",     "2018-01-09",
                                                        5L,          7L,       "2021-01-01",     "2021-01-02",
                                                        1L,          2L,       "2016-01-02",     "2016-01-04")

  subject_id <- 2
  cohorts <- list("hosptalised" = hosptalised,
                  "icu_patients" = icu_patients,
                  "drugs_treatment" = drugs_treatment)
  graph <- graphCohort(subject_id, cohorts = cohorts)
  expect_s3_class(graphCohort(subject_id, cohorts = cohorts), "ggplot")
  })


test_that("compressCohort", {

  hosptalised <- tibble::tribble(~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
                                 2L,           5,       "2019-01-01",     "2019-01-10",
                                 2L,           2,       "2016-01-01",     "2016-01-10",
                                 2L,           6,       "2020-01-01",     "2020-01-10",
                                 2L,           7,       "2021-01-01",     "2021-01-10",
                                 2L,           3,       "2017-01-01",     "2017-01-10",
                                 2L,           4,       "2018-01-01",     "2018-01-02",
                                 2L,           1,       "2015-01-01",     "2015-01-10")

  icu_patients <- tibble::tribble(~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
                                  5L,           6,       "2020-01-04",     "2020-01-08",
                                  5L,           5,       "2019-01-03",     "2019-01-05",
                                  5L,           3,       "2017-01-02",     "2017-01-04",
                                  5L,           7,       "2021-01-01",     "2021-01-10",
                                  5L,           2,       "2016-01-02",     "2016-01-04",
                                  5L,           4,       "2018-01-02",     "2018-01-04")

  drugs_treatment <- tibble::tribble(~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
                                     6L,          3L,       "2017-01-03",     "2017-01-05",
                                     4L,          6L,       "2020-01-01",     "2020-01-02",
                                     4L,          6L,       "2020-01-05",     "2020-01-06",
                                     2L,          5L,       "2019-01-02",     "2019-01-04",
                                     7L,          1L,       "2015-01-02",     "2015-01-04",
                                     12L,         4L,       "2018-01-07",     "2018-01-09",
                                     5L,          7L,       "2021-01-01",     "2021-01-02",
                                     1L,          2L,       "2016-01-02",     "2016-01-04")

  cohorts <- list("hosptalised" = hosptalised,
                  "icu_patients" = icu_patients,
                  "drugs_treatment" = drugs_treatment)
  id <- 2
  result <- compressCohorts(data = cohorts, id = id)
  expect_equal(nrow(result), 3)

  id <- 1
  result <- compressCohorts(data = cohorts, id = id)
  expect_equal(nrow(result), 2)
  })
