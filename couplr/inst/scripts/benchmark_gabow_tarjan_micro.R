#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(microbenchmark)
})

args <- commandArgs(trailingOnly = TRUE)
times <- if (length(args) >= 1) as.integer(args[[1]]) else 20L
sizes <- if (length(args) >= 2) as.integer(strsplit(args[[2]], ",", fixed = TRUE)[[1]]) else c(8L, 12L, 16L, 24L, 32L, 48L)
max_cost <- if (length(args) >= 3) as.integer(args[[3]]) else 100000L

pkgload::load_all(".", quiet = TRUE)

bench_one <- function(n, times, max_cost) {
  set.seed(1000L + n)
  cost <- matrix(sample.int(max_cost, n * n, replace = TRUE), n, n)

  invisible(assignment(cost, method = "gabow_tarjan"))

  mb <- microbenchmark(
    gabow_tarjan = assignment(cost, method = "gabow_tarjan"),
    times = times,
    unit = "ms",
    control = list(warmup = 2L)
  )

  summary_mb <- summary(mb)
  data.frame(
    n = n,
    times = times,
    max_cost = max_cost,
    min_ms = summary_mb$min,
    median_ms = summary_mb$median,
    mean_ms = summary_mb$mean,
    max_ms = summary_mb$max,
    stringsAsFactors = FALSE
  )
}

results <- do.call(rbind, lapply(sizes, bench_one, times = times, max_cost = max_cost))
results$log_n <- log(results$n)
results$log_median <- log(results$median_ms)

fit <- lm(log_median ~ log_n, data = results)
results$fit_exponent <- unname(coef(fit)[["log_n"]])

print(results, row.names = FALSE, digits = 4)
cat(sprintf("Estimated median-time exponent over these sizes: %.3f\n", coef(fit)[["log_n"]]))
