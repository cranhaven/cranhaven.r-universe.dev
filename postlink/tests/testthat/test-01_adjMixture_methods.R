local_edition(3)

test_that("print.adjMixture works for fully specified objects", {
 # Setup data
 df <- data.frame(id = 1:100, x = runif(100), hndlnk = rep(c(TRUE, FALSE), 50))

 adj <- adjMixture(linked.data = df,
                   m.formula = ~ x,
                   m.rate = 0.1,
                   safe.matches = df$hndlnk)

 # Capture output
 out <- capture_output(print(adj))

 # Check for key strings
 expect_match(out, "Adjustment Object: Mixture Model")
 expect_match(out, "Observations:\\s+100")
 expect_match(out, "Mismatch Model:\\s+~x")
 expect_match(out,"Global Mismatch Rate:\\s+0.1.*\\(Constrained\\)")
 expect_match(out, "Safe Matches:\\s+50.*50.0%")
})

test_that("print.adjMixture works for minimal objects", {
 df <- data.frame(id = 1:50)
 adj <- adjMixture(linked.data = df, m.formula = ~1)

 out <- capture_output(print(adj))

 expect_match(out, "Global Mismatch Rate:\\s+Unconstrained")
 expect_match(out, "Safe Matches:\\s+None specified")
})

test_that("print.adjMixture handles corrupted/empty data gracefully", {
 # Manually create a broken object (simulate corruption)
 adj_broken <- structure(
  list(
   data_ref = new.env(), # Empty environment
   m.formula = ~1,
   logitbound = NULL,
   safe.matches = NULL
  ),
  class = c("adjMixture", "adjustment")
 )

 out <- capture_output(print(adj_broken))
})
