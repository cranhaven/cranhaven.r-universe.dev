test_that("multiplication works", {
  testthat::skip_on_cran()
  library(lavaan)
  library(mxsem)
  set.seed(123)
  HS.model <- 'visual  =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9'

  fit <- cfa(HS.model,
             data = HolzingerSwineford1939,
             group = "school")

  mg_model <- mxsem(model = HS.model,
                    data  = HolzingerSwineford1939) |>
    # we want separate models for all combinations of grades and schools:
    mxsem_group_by(grouping_variables = c("school")) |>
    mxRun()

  testthat::expect_true(abs(
    mg_model$fitfunction$result[[1]] -
      -2*logLik(fit)
  ) < .1)

  HS.model <- 'visual  =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9
               # mxsem differs from lavaan in that it will not
               # automatically set intercepts freed for all but one group
               # once other parameters are set to equality
               visual   ~ 0*1
               textual  ~ 0*1
               speed    ~ 0*1
  '

  fit <- cfa(HS.model,
             data = HolzingerSwineford1939,
             group = "school",
             group.equal = c("intercepts", "loadings"))

  mg_model <- mxsem(model = HS.model,
                    data  = HolzingerSwineford1939) |>
    # we want separate models for all combinations of grades and schools:
    mxsem_group_by(grouping_variables = c("school"),
                   parameters = mxsem::unicode_undirected()) |>
    mxTryHard()

  testthat::expect_true(abs(
    mg_model$fitfunction$result[[1]] -
      -2*logLik(fit)
  ) < .1)


  HS.model <- 'visual  =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9
  '

  fit <- cfa(HS.model,
             data = HolzingerSwineford1939,
             group = "school",
             group.equal = c("loadings"))

  mg_model <- mxsem(model = HS.model,
                    data  = HolzingerSwineford1939) |>
    # we want separate models for all combinations of grades and schools:
    mxsem_group_by(grouping_variables = c("school"),
                   parameters = c(mxsem::unicode_undirected(),
                                  "one")) |>
    mxTryHard()

  testthat::expect_true(abs(
    mg_model$fitfunction$result[[1]] -
      -2*logLik(fit)
  ) < .1)
})
