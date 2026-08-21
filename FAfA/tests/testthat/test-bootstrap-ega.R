test_that("Bootstrap EGA settings are validated", {
  settings <- FAfA:::validate_bootega_settings(500, 2, 2026)

  expect_identical(settings, list(iter = 500L, ncores = 2L, seed = 2026))
  expect_identical(typeof(settings$seed), "double")
  expect_error(FAfA:::validate_bootega_settings(19, 2, 2026), "at least 20")
  expect_error(FAfA:::validate_bootega_settings(500, 0, 2026), "positive integer")
  expect_error(FAfA:::validate_bootega_settings(500, 2, NA), "positive integer")
})

test_that("Bootstrap EGA retries a non-positive-definite lavaan correlation with Pearson", {
  calls <- character()
  fake_bootega <- function(data, corr, ...) {
    calls <<- c(calls, corr)
    if (identical(corr, "cor_auto")) {
      stop(
        paste0(
          "lavaan::lavCor() failed to estimate the correlation matrix: ",
          "sample covariance matrix is not positive-definite"
        )
      )
    }
    list(correlation = corr)
  }

  result <- FAfA:::run_bootega_with_correlation_fallback(
    data = data.frame(x = c(1, 2), y = c(2, 1)),
    corr = "cor_auto",
    bootega_fun = fake_bootega
  )

  expect_identical(calls, c("cor_auto", "pearson"))
  expect_true(result$used_fallback)
  expect_identical(result$correlation, "pearson")
  expect_identical(result$result$correlation, "pearson")
})

test_that("Bootstrap EGA does not hide unrelated estimation errors", {
  fake_bootega <- function(data, corr, ...) stop("unrelated network error")

  expect_error(
    FAfA:::run_bootega_with_correlation_fallback(
      data = data.frame(x = c(1, 2), y = c(2, 1)),
      corr = "cor_auto",
      bootega_fun = fake_bootega
    ),
    "unrelated network error"
  )
})

test_that("Bootstrap EGA converts integer variables without changing their values", {
  source_data <- data.frame(
    item_1 = c(1L, 2L, NA_integer_, 4L),
    item_2 = c(5L, 4L, 3L, 2L),
    check.names = FALSE
  )

  result <- FAfA:::prepare_bootega_data(source_data)

  expect_true(all(vapply(result, typeof, character(1)) == "double"))
  expect_equal(result, data.frame(
    item_1 = c(1, 2, NA_real_, 4),
    item_2 = c(5, 4, 3, 2),
    check.names = FALSE
  ))
  expect_true(all(vapply(source_data, typeof, character(1)) == "integer"))
  expect_error(
    FAfA:::prepare_bootega_data(data.frame(item = letters[1:4])),
    "numeric variables"
  )
})

test_that("Bootstrap EGA results are converted to stable report tables", {
  item_plot <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  bootega_object <- list(
    summary.table = data.frame(
      n.Boots = 500,
      median.dim = 2,
      SE.dim = 0.12,
      check.names = FALSE
    ),
    frequency = data.frame(
      `# of Factors` = c(1, 2),
      Frequency = c(25, 475),
      check.names = FALSE
    )
  )
  stability_object <- list(
    dimension.stability = list(
      structural.consistency = c(`1` = 0.90, `2` = 0.88),
      average.item.stability = c(`1` = 0.94, `2` = 0.91)
    ),
    item.stability = list(
      membership = list(structure = c(item_1 = 1, item_2 = 1, item_3 = 2)),
      item.stability = list(
        empirical.dimensions = c(item_1 = 0.95, item_2 = 0.90, item_3 = 0.92)
      ),
      plot = item_plot
    )
  )

  result <- FAfA:::prepare_bootega_results(bootega_object, stability_object)

  expect_named(result, c(
    "summary_table", "frequency_table", "dimension_table", "item_table", "item_plot"
  ))
  expect_equal(result$dimension_table$Structural_Consistency, c(0.90, 0.88))
  expect_equal(result$dimension_table$Average_Item_Stability, c(0.94, 0.91))
  expect_equal(result$item_table$Item, c("item_1", "item_2", "item_3"))
  expect_equal(result$item_table$Empirical_Dimension, c(1, 1, 2))
  expect_equal(result$item_table$Item_Stability, c(0.95, 0.90, 0.92))
  expect_s3_class(result$item_plot, "ggplot")
})

test_that("EGA interface exposes the complete Bootstrap EGA workflow", {
  html <- paste(as.character(FAfA:::ega_ui("ega")), collapse = "")

  expect_match(html, "Bootstrap Exploratory Graph Analysis (bootEGA)", fixed = TRUE)
  expect_match(html, "ega-run_bootega_button", fixed = TRUE)
  expect_match(html, "ega-bootega_dimension_stability_table", fixed = TRUE)
  expect_match(html, "ega-bootega_item_stability_plot", fixed = TRUE)
  expect_match(html, "ega-download_bootega_apa7", fixed = TRUE)
})

test_that("installed EGAnet provides the Bootstrap EGA stability API", {
  skip_if_not_installed("EGAnet", minimum_version = "2.4.1")

  expect_true(all(c("iter", "type", "ncores", "seed") %in%
                    names(formals(EGAnet::bootEGA))))
  expect_true(is.function(EGAnet::dimensionStability))
  expect_true(is.function(EGAnet::itemStability))
})

test_that("Bootstrap stability is not drawn inside the analysis observer", {
  server_code <- paste(deparse(body(FAfA:::ega_server)), collapse = "\n")

  expect_match(server_code, "dimensionStability", fixed = TRUE)
  expect_match(server_code, "IS.plot = FALSE", fixed = TRUE)
})
