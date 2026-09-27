local_edition(3)

# Helper: Create Dummy Data
setup_data <- function() {
 data.frame(
  id = 1:50,
  val = rnorm(50)
 )
}

test_that("print.adjMixBayes: Output for specified data", {
 df <- setup_data()
 adj <- adjMixBayes(linked.data = df)

 # Capture the print output
 out <- capture_output(print(adj))

 # Check for header
 expect_match(out, "Adjustment Object: Bayesian Mixture")

 # Check data summary
 expect_match(out, "Observations:\\s+50")
})

test_that("print.adjMixBayes: Output for NULL data", {
 adj <- adjMixBayes(linked.data = NULL)

 out <- capture_output(print(adj))

 # Check for header
 expect_match(out, "Adjustment Object: Bayesian Mixture")

 # Check status message
 expect_match(out, "Status:\\s+None specified")
})

test_that("print.adjMixBayes: Robustness against corrupted objects", {
 # Simulate an object where the environment exists but is empty
 adj_empty_env <- structure(
  list(data_ref = new.env()),
  class = c("adjMixBayes", "adjustment")
 )

 out <- capture_output(print(adj_empty_env))

 # Should not crash, should report None specified
 expect_match(out, "Status:\\s+None specified")
})
