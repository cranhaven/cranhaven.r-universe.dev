
# Test setup: small graph for all tests ----------------------------------------
set.seed(42)
true_net <- matrix(c(0, 1, 1, 0,
                     1, 0, 0, 1,
                     1, 0, 0, 1,
                     0, 1, 1, 0), nrow = 4, byrow = TRUE)

person1 <- matrix(c(0, 1, 0, 0,
                    1, 0, 1, 1,
                    0, 1, 0, 0,
                    0, 1, 0, 0), nrow = 4, byrow = TRUE)

person2 <- matrix(c(0, 1, 1, 1,
                    1, 0, 0, 0,
                    1, 0, 0, 1,
                    1, 0, 1, 0), nrow = 4, byrow = TRUE)

graph <- new_barry_graph(list(true_net, person1, person2))

# count_imaginary_census returns imaginary_census class ------------------------
census <- count_imaginary_census(graph)
expect_true(inherits(census, "imaginary_census"))
expect_true(inherits(census, "data.frame"))
expect_true(all(c("id", "name", "value") %in% names(census)))

# summary.imaginary_census returns named numeric vector ------------------------
agg <- summary(census)
expect_true(is.numeric(agg))
expect_true(!is.null(names(agg)))
expect_true(all(agg == sort(agg, decreasing = TRUE)))  # sorted descending
expect_equal(sum(agg), sum(census$value))               # totals must match

# test_imaginary_census returns imaginarycss_test class ------------------------
res <- test_imaginary_census(graph, n_sim = 20)
expect_true(inherits(res, "imaginarycss_test"))

# Structure checks
expect_true(is.list(res))
expect_true(all(c("results", "observed", "null_matrix", "n_sim", "alpha") %in%
                names(res)))

# results data frame has expected columns
expect_true(all(c("motif", "observed", "null_mean", "null_sd",
                  "z_score", "p_value", "significant") %in%
                names(res$results)))

# n_sim matches
expect_equal(res$n_sim, 20)
expect_equal(ncol(res$null_matrix), 20)

# alpha default
expect_equal(res$alpha, 0.05)

# observed sums match census summary
expect_equal(
  sort(names(res$observed)),
  sort(names(summary(census)))
)

# print method runs without error -----------------------------------------------
expect_silent(capture.output(print(res)))

# summary method runs without error ---------------------------------------------
expect_silent(capture.output(summary(res)))

# plot method runs without error ------------------------------------------------
expect_silent({
  f <- tempfile(fileext = ".pdf")
  pdf(f)
  on.exit(dev.off())
  plot(res)
  dev.off()
  on.exit()
})

# significance column is logical -------------------------------------------------
expect_true(is.logical(res$results$significant))

# z_scores are always finite (no NaN/Inf from zero-variance motifs) -------------
expect_true(all(is.finite(res$results$z_score)))
