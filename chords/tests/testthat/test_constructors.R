library(chords)
context("testing constructors")

test_that("constructors", {
  rds.object <- rdsObjectConstructor()
  expect_equal(length(rds.object),6)
  
  rds.example <- makeRDSExample()
  expect_equal(length(rds.example), 6)
  
  test.snowball <- makeSnowBall(rds.example$rds.sample, seeds=1)
  expect_equal(length(test.snowball$I.t), 400)
  
  rds.object2 <- initializeRdsObject(rds.example$rds.sample)
  expect_equal(length(rds.object2), 6)
})
