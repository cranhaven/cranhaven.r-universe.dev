context("Test that check_* functions work")

test_that("Checking for damaging content works",{
  
  result <- try({check_damaging("enwiki", 34854345)}, silent = TRUE)
  if(!"try-error" %in% class(result)){
    expect_equal(class(result), "data.frame")
    expect_equal(nrow(result), 1)
    expect_equal(ncol(result), 5)
    expect_equal(complete.cases(result), TRUE)
  }

})

test_that("Checking for good-faith content works",{
  
  result <- try({check_goodfaith("enwiki", 34854345)}, silent = TRUE)
  if(!"try-error" %in% class(result)){
    expect_equal(class(result), "data.frame")
    expect_equal(nrow(result), 1)
    expect_equal(ncol(result), 5)
    expect_equal(complete.cases(result), TRUE)
  }
})

test_that("Error handling works",{
  
  result <- try({check_goodfaith("enwiki", 9999999999)}, silent = TRUE)
  if(!"try-error" %in% class(result)){
    expect_equal(class(result), "data.frame")
    expect_equal(nrow(result), 1)
    expect_equal(ncol(result), 5)
    expect_equal(complete.cases(result), FALSE)
  }
})