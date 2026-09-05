test_that("predict() returns one data frame per subgroup model", {
  new_x <- simIMR$platforms$genomic[1:20, ]
  new_p <- simIMR$platforms$proteomic[1:20, ]
  pr <- predict(fit_bin, newdata = list(new_x, new_p),
                platform_names = c("1", "2"),
                covariates = simIMR$covariates)
  expect_type(pr, "list")
  expect_equal(length(pr), length(fit_bin$model_bitstrings))
  for (d in pr) expect_true(all(c("id", "predict") %in% names(d)))
})

test_that("predict_imr() dispatches through the predict() S3 method", {
  new_x <- simIMR$platforms$genomic[1:20, ]
  new_p <- simIMR$platforms$proteomic[1:20, ]
  expect_equal(
    predict_imr(fit_bin, newdata = list(new_x, new_p),
                platform_names = c("1", "2"),
                covariates = simIMR$covariates),
    predict(fit_bin, newdata = list(new_x, new_p),
            platform_names = c("1", "2"),
            covariates = simIMR$covariates)
  )
})

test_that("platform_names defaults to the training order", {
  new_x <- simIMR$platforms$genomic[1:30, ]
  new_p <- simIMR$platforms$proteomic[1:30, ]
  a <- predict(fit_bin, newdata = list(new_x, new_p),
               covariates = simIMR$covariates)                       # default
  b <- predict(fit_bin, newdata = list(new_x, new_p),
               platform_names = c("1", "2"), covariates = simIMR$covariates)
  expect_equal(a, b)
})

test_that("binary predictions are probabilities in [0, 1]", {
  new_x <- simIMR$platforms$genomic[1:60, ]
  new_p <- simIMR$platforms$proteomic[1:60, ]
  pr <- predict(fit_bin, newdata = list(new_x, new_p),
                covariates = simIMR$covariates)
  vals <- unlist(lapply(pr, function(d) as.numeric(d$predict)))
  vals <- vals[is.finite(vals)]
  expect_gt(length(vals), 0)
  expect_true(all(vals >= 0 & vals <= 1))
})

test_that("subjects are routed to the correct availability subgroup", {
  # ids 1:30 are present in genomic (1..240) and proteomic (1..180) but not
  # metabolomic (121..300), so they belong to subgroup '011'.
  new_x <- simIMR$platforms$genomic[1:30, ]
  new_p <- simIMR$platforms$proteomic[1:30, ]
  pr <- predict(fit_bin, newdata = list(new_x, new_p),
                covariates = simIMR$covariates)
  expect_equal(nrow(pr[["model:011"]]), 30L)
  expect_equal(nrow(pr[["model:100"]]), 0L)
})

test_that("continuous predictions correlate with the truth", {
  fit <- fit_demo("continuous", total = 800, burn = 300, seed = 9)
  new_x <- simIMR$platforms$genomic[simIMR$platforms$genomic$id <= 120, ]
  new_p <- simIMR$platforms$proteomic[simIMR$platforms$proteomic$id <= 120, ]
  pr <- predict(fit, newdata = list(new_x, new_p),
                covariates = simIMR$covariates)
  d <- merge(pr[["model:011"]], simIMR$outcome.continuous, by = "id")
  expect_gt(stats::cor(d$predict, d$y), 0.5)
})

test_that("prediction is invariant to newdata and covariate row order", {
  new_x <- simIMR$platforms$genomic[1:60, ]
  new_p <- simIMR$platforms$proteomic[1:60, ]
  base <- predict(fit_bin, newdata = list(new_x, new_p),
                  covariates = simIMR$covariates)

  set.seed(101)
  shuffled <- predict(
    fit_bin,
    newdata = list(new_x[sample(nrow(new_x)), ],
                   new_p[sample(nrow(new_p)), ]),
    covariates = simIMR$covariates[sample(nrow(simIMR$covariates)), ]
  )

  sort_predictions <- function(x) {
    lapply(x, function(d) {
      d <- d[order(as.character(d$id)), , drop = FALSE]
      rownames(d) <- NULL
      d
    })
  }
  expect_equal(sort_predictions(shuffled), sort_predictions(base))
})

test_that("predict validates new data, covariates and model averaging controls", {
  new_x <- simIMR$platforms$genomic[1:10, ]
  expect_error(
    predict(fit_bin, newdata = list(new_x)),
    "`covariates` is required"
  )

  no_id <- new_x
  names(no_id)[1] <- "sample_id"
  expect_error(
    predict(fit_bin, newdata = list(no_id), covariates = simIMR$covariates),
    "id.*first column"
  )

  expect_error(
    predict(fit_bin, newdata = list(new_x), platform_names = "4",
            covariates = simIMR$covariates),
    "`platform_names`"
  )

  missing_feature <- new_x[, -2]
  expect_error(
    predict(fit_bin, newdata = list(missing_feature),
            covariates = simIMR$covariates),
    "feature column"
  )

  renamed_covariates <- simIMR$covariates
  names(renamed_covariates)[2] <- "age2"
  expect_error(
    predict(fit_bin, newdata = list(new_x), covariates = renamed_covariates),
    "covariate names"
  )

  expect_error(
    predict(fit_bin, newdata = list(new_x), covariates = simIMR$covariates,
            max_models = 0),
    "`max_models`"
  )
})

test_that("predict returns empty model outputs when no subjects can be routed", {
  new_x <- simIMR$platforms$genomic[1:5, ]
  new_x$id <- new_x$id + 10000
  res <- NULL
  expect_warning(
    res <- predict(fit_bin, newdata = list(new_x), covariates = simIMR$covariates),
    "No subjects"
  )
  expect_named(res, paste0("model:", fit_bin$model_bitstrings))
  expect_true(all(vapply(res, nrow, integer(1)) == 0L))
})

test_that("predict preserves character ids while keeping predictions numeric", {
  platforms <- lapply(simIMR$platforms, function(x) {
    x$id <- paste0("id", x$id)
    x
  })
  outcome <- simIMR$outcome.binary
  outcome$id <- paste0("id", outcome$id)
  covariates <- simIMR$covariates
  covariates$id <- paste0("id", covariates$id)

  fit <- imr(
    platforms, outcome, cov = covariates, type_outcome = "binary",
    nu = c(-4, -3, -4), sample_mcmc = c(80, 40), ssize = 30, seed = 12
  )
  pr <- predict(
    fit,
    newdata = list(platforms$genomic[1:8, ], platforms$proteomic[1:8, ]),
    covariates = covariates
  )

  expect_type(pr[["model:011"]]$id, "character")
  expect_type(pr[["model:011"]]$predict, "double")
})

test_that("predict works for fits without clinical covariates", {
  fit <- imr(
    simIMR$platforms, simIMR$outcome.continuous,
    type_outcome = "continuous", nu = c(-4, -3, -4),
    sample_mcmc = c(80, 40), ssize = 30, seed = 13
  )

  pr <- predict(
    fit,
    newdata = list(simIMR$platforms$genomic[1:12, ],
                   simIMR$platforms$proteomic[1:12, ])
  )
  expect_type(pr[["model:011"]]$predict, "double")
  expect_equal(nrow(pr[["model:011"]]), 12L)

  expect_warning(
    predict(
      fit,
      newdata = list(simIMR$platforms$genomic[1:3, ],
                     simIMR$platforms$proteomic[1:3, ]),
      covariates = simIMR$covariates
    ),
    "ignoring"
  )
})

test_that("predict uses the fitted non-local prior scale during model averaging", {
  set.seed(21)
  n <- 36
  platform <- data.frame(id = seq_len(n), x1 = rnorm(n), x2 = rnorm(n))
  outcome <- data.frame(id = seq_len(n), y = 3 * platform$x1 + rnorm(n, sd = 0.2))

  fit <- imr(
    list(platform = platform), outcome, type_outcome = "continuous",
    nu = 2, hh = 0.02, sample_mcmc = c(120, 60), ssize = 5, seed = 21
  )
  base <- predict(fit, newdata = list(platform[1:12, ]))[["model:1"]]$predict

  changed <- fit
  changed$list_hyperpara[2] <- 20
  shifted <- predict(changed, newdata = list(platform[1:12, ]))[["model:1"]]$predict

  expect_false(isTRUE(all.equal(base, shifted, tolerance = 1e-12)))
})
