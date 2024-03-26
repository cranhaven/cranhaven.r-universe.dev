context("heatmap")

## loading data
data(metafile96, rawdata96,rawdata384)
rawdata96 <- data2plateformat(rawdata96,platetype = 96)
rawdata384 <- data2plateformat(rawdata384,platetype = 384)

## eg:1 heat map of rawdata96
data<-matrix96(plate2df(rawdata96),"value")
eg1<-heatplate(data,"Plate 1", size=5)
test_that("eg1 heatplate is working", {
  expect_identical(eg1$labels$x, "Column")
  expect_identical(eg1$labels$y, "Row")
  expect_identical(eg1$labels$colour, "value")
  expect_identical(eg1$guides[[1]], "coloursteps")
})

## eg:2 heat map of rawdata96 can also be called as
eg2<-heatplate(as.matrix(rawdata96),"Plate 1", size=5)
test_that("eg2 heatplate is working", {
  expect_identical(eg2$labels$x, "Column")
  expect_identical(eg2$labels$y, "Row")
  expect_identical(eg2$labels$colour, "value")
  expect_identical(eg2$guides[[1]], "coloursteps")
})

## eg:3 heat map of rawdata384
eg3<-heatplate(as.matrix(rawdata384),"Plate 1", size=2)
test_that("eg3 heatplate is working", {
  expect_identical(eg3$labels$x, "Column")
  expect_identical(eg3$labels$y, "Row")
  expect_identical(eg3$labels$colour, "value")
  expect_identical(eg3$guides[[1]], "coloursteps")
})

## eg:4 catagorical map of metafile96 (column:id)
data<-matrix96(metafile96,"id")
eg4<-heatplate(data,"Plate 1", size=5)
test_that("eg4 heatplate is working", {
  expect_identical(eg4$labels$x, "Column")
  expect_identical(eg4$labels$y, "Row")
  expect_identical(eg4$labels$colour, "factor(value)")
  expect_identical(eg4$guides[[1]], NULL)
})
