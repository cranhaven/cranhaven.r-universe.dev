strikes <- c ( rep ( 0, 46 ) , rep ( 1, 76 ) , rep ( 2, 24 ) , rep ( 3, 9 ) , rep ( 4, 1 )  )
test_that("correct glm", {
  expect_no_error(glm.good ( strikes ~ 1 , link = "log" ))
})
#> Test passed

strikes <- c ( rep ( 0, 46 ) , rep ( 1, 76 ) , rep ( 2, 24 ) , rep ( 3, 9 ) , rep ( -4, 1 )  )
test_that("negative outcome glm", {
  expect_error(glm.good ( strikes ~ 1 , link = "log" ))
})
#> Test passed

strikes <- c ( rep ( 0, 46 ) , rep ( 1, 76 ) , rep ( 2, 24 ) , rep ( 3, 9 ) , rep ( 1.2, 1 )  )
test_that("non-integer outcome glm", {
  expect_error(glm.good ( strikes ~ 1 , link = "log" ))
})
#> Test passed
