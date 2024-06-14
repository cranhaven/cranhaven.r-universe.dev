context("Select Individual Features")

test_that("Percentile function works", {
  result <- selectPercentile(40)(iris,'Species', giniIndex())
  expect_equal(result$featuresSelected, c("Petal.Length","Petal.Width"))
  expect_equal(result$valuePerFeature, c(0.9373333, 0.9372222), tolerance = 1e-4)
  expect_true(all(result$bestFeatures == c(Sepal.Length = 0, Sepal.width = 0, Petal.Length = 1, Petal.Width = 1)))
  expect_equal(dimnames(result$bestFeatures)[[2]], dimnames(data.frame(Sepal.Length = 0, Sepal.Width = 0, Petal.Length = 1, Petal.Width = 1))[[2]])
})

test_that("Threshold function works", {
  result <- selectThreshold(0.9)(iris,'Species',giniIndex())
  expect_equal(result$featuresSelected, c("Petal.Length","Petal.Width"))
  expect_equal(result$valuePerFeature, c(0.9373333, 0.9372222), tolerance = 1e-4)
  expect_true(all(result$bestFeatures == c(Sepal.Length = 0, Sepal.width = 0, Petal.Length = 1, Petal.Width = 1)))
  expect_equal(dimnames(result$bestFeatures)[[2]], dimnames(data.frame(Sepal.Length = 0, Sepal.Width = 0, Petal.Length = 1, Petal.Width = 1))[[2]])
})

test_that("Threshold range function works", {
  result <- selectThresholdRange(0.9)(iris,'Species',giniIndex())
  expect_equal(result$featuresSelected, c("Petal.Length","Petal.Width"))
  expect_equal(result$valuePerFeature, c(0.9373333, 0.9372222), tolerance = 1e-4)
  expect_true(all(result$bestFeatures == c(Sepal.Length = 0, Sepal.width = 0, Petal.Length = 1, Petal.Width = 1)))
  expect_equal(dimnames(result$bestFeatures)[[2]], dimnames(data.frame(Sepal.Length = 0, Sepal.Width = 0, Petal.Length = 1, Petal.Width = 1))[[2]])
})

test_that("Difference function works", {
  result <- selectDifference()(iris,'Species',giniIndex())
  expect_equal(result$featuresSelected, c("Petal.Length","Petal.Width"))
  expect_equal(result$valuePerFeature, c(0.9373333, 0.9372222), tolerance = 1e-4)
  expect_true(all(result$bestFeatures == c(Sepal.Length = 0, Sepal.width = 0, Petal.Length = 1, Petal.Width = 1)))
  expect_equal(dimnames(result$bestFeatures)[[2]], dimnames(data.frame(Sepal.Length = 0, Sepal.Width = 0, Petal.Length = 1, Petal.Width = 1))[[2]])
})

test_that("Slope function works", {
  result <- selectSlope(0.4)(iris,'Species',giniIndex())
  expect_equal(result$featuresSelected, c("Petal.Length","Petal.Width"))
  expect_equal(result$valuePerFeature, c(0.9373333, 0.9372222), tolerance = 1e-4)
  expect_true(all(result$bestFeatures == c(Sepal.Length = 0, Sepal.width = 0, Petal.Length = 1, Petal.Width = 1)))
  expect_equal(dimnames(result$bestFeatures)[[2]], dimnames(data.frame(Sepal.Length = 0, Sepal.Width = 0, Petal.Length = 1, Petal.Width = 1))[[2]])
})


test_that("SelectKBest function works", {
  result <- selectKBest(2)(iris,'Species',giniIndex())
  expect_equal(result$featuresSelected, c("Petal.Length","Petal.Width"))
  expect_equal(result$valuePerFeature, c(0.9373333, 0.9372222), tolerance = 1e-4)
  expect_true(all(result$bestFeatures == c(Sepal.Length = 0, Sepal.width = 0, Petal.Length = 1, Petal.Width = 1)))
  expect_equal(dimnames(result$bestFeatures)[[2]], dimnames(data.frame(Sepal.Length = 0, Sepal.Width = 0, Petal.Length = 1, Petal.Width = 1))[[2]])
})