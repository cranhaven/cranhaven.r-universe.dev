local_edition(3)

# Helper: Create Dummy Data
setup_data <- function() {
 data.frame(
  id = 1:50,
  val = rnorm(50)
 )
}

test_that("adjMixBayes: Basic valid construction works", {
 df <- setup_data()

 # Initialize with dataframe
 adj <- adjMixBayes(linked.data = df)

 # Check Class
 expect_s3_class(adj, "adjMixBayes")
 expect_s3_class(adj, "adjustment")

 # Check Structure
 expect_true(is.environment(adj$data_ref))
 expect_identical(adj$data_ref$data, df)
})

test_that("adjMixBayes: Reference semantics (lightweight container) are preserved", {
 df <- setup_data()
 adj <- adjMixBayes(linked.data = df)

 # Ensure data is stored in an environment, not directly in the list
 expect_type(adj$data_ref, "environment")

 # Modify the environment data manually to prove it's a reference
 # (In a real scenario, this helps avoid copying the dataframe)
 adj$data_ref$data$new_col <- 1
 expect_true("new_col" %in% names(adj$data_ref$data))
})

test_that("adjMixBayes: Input validation enforces types", {
 # Error on character string
 expect_error(adjMixBayes(linked.data = "invalid_string"),
              "must be a data.frame, list, or environment")

 # Error on numeric vector
 expect_error(adjMixBayes(linked.data = c(1, 2, 3)),
              "must be a data.frame, list, or environment")
})

test_that("adjMixBayes: Handling of coercible types (List)", {
 df <- setup_data()
 df_list <- as.list(df)

 # Should successfully coerce list to data.frame
 adj <- adjMixBayes(linked.data = df_list)

 expect_s3_class(adj$data_ref$data, "data.frame")
 expect_equal(nrow(adj$data_ref$data), 50)
})

test_that("adjMixBayes: Handling of NULL input", {
 # Allow NULL construction
 adj <- adjMixBayes(linked.data = NULL)

 expect_s3_class(adj, "adjMixBayes")
 expect_true(is.environment(adj$data_ref))
 expect_null(adj$data_ref$data)
})
