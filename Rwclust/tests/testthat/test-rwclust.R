test_that(
  "rwclust runs error-free with valid input", 
{
  
  df <- data.frame(
    from = c(1,2,3),
    to = c(2,3,1),
    weight = c(1,1,1)
  )

  output <- rwclust(df, iter=3, k=2)
  expect_type(output, "list")
  expect_s3_class(output, "rwclust")

})
