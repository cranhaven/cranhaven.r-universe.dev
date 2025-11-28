# Define a sample input data frame
df <- data.frame(A1 = c(1, 2, 3), A2 = c(4, 5, NA), A3 = c(6, 7, 8),
                AICc = c(10, 11, 12),
                 max_vif = c(1.5, 2.1, 4.9), k = c(3, 4, 5),
                 DeltaAICc = c(0, 1, 2), AICWeight = c(0.5, 0.3, 0.2), N = 10)


# Define the test
test_that("akaike_adjusted_rsq function returns the expected output",
          {
            output <- akaike_adjusted_rsq(df)
            testthat::expect_s3_class(output, "data.frame")
          })
