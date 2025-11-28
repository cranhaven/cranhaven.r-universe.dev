iris <- dplyr::group_by(iris, Species) %>%
  dplyr::mutate(id = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(id)

which_selected <- iris[["Species"]] != "setosa"
two_obs <- iris[["Sepal.Length"]][which_selected]
two_groups <- iris[["Species"]][which_selected] %>% droplevels()

obs <- iris[["Sepal.Length"]]
many_groups <- iris[["Species"]]

test_that("output class is correct", {
  skip_on_cran()

  expect_type(paired_test_continuous(two_groups, two_obs), "list")
  expect_type(paired_test_continuous(many_groups, obs), "list")
})

test_that("output structure is correct", {
  skip_on_cran()

  expect_true(all(
    c("P", "stat", "df", "testname", "statname", "namefun") %in%
      names(paired_test_continuous(two_groups, two_obs))
  ))
  expect_true(all(
    c("P", "stat", "df", "testname", "statname", "namefun") %in%
      names(paired_test_continuous(many_groups, obs))
  ))
})


test_that("wrong input are managed", {
  skip_on_cran()

  expect_usethis_error(
    paired_test_continuous(factor(c(1L, 2L)), 1L),
    "length"
  )
  expect_warning(expect_warning(
    paired_test_continuous(c(1L, 2L), c(1L, 2L)),
    "factor"), "one group")
})


test_that("data by groups are managed", {
  skip_on_cran()

  ord <- order(two_groups)

  expect_type(
    paired_test_continuous(two_groups[ord], two_obs[ord]),
    "list"
  )

  expect_warning(
    paired_test_continuous(two_groups[ord][-1L], two_obs[ord][-1L]),
    "incomplete"
  )

  expect_gte(
    suppressWarnings(
      paired_test_continuous(two_groups[ord][-1L], two_obs[ord][-1L])
    )[["P"]],
    9L
  )
})



test_that("one group is managed", {
  skip_on_cran()

  expect_warning(
    paired_test_continuous(factor(c("a", "a", "a")), c(1L, 2L, 3L)),
    "Only one group with data, no paired test is done"
  )
})


test_that("many groups works properly", {
  data_db <- iris %>% dplyr::select(Sepal.Length, Species, id)

  expect_equal(
    paired_test_continuous(many_groups, obs)[["P"]][[1L]],
    summary(
      stats::aov(Sepal.Length ~ Species + Error(id / Species),
                 data = data_db)
    )[["Error: Within"]][[1L]][1L, "Pr(>F)"]
  )
})
