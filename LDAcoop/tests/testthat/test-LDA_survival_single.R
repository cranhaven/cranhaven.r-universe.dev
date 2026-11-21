test_that("output", {
  data(LDAdata)
  cell.line <- unique(LDAdata$name)[1]
  x <- subset.data.frame(LDAdata, subset = (name==cell.line) & (Group < 2))
  x <- x[,c(4,5,6,3,2,1)]
  act <- LDA_activity(x[,1:4])
  sf <- LDA_survival_single(act.0 = act[[1]],act.x = act[[2]])
  expect_equal(class(sf), "list")
  expect_equal(length(sf$CI.ep),2)
  expect_equal(sf$CI.ep[2]>sf$CI.ep[1],TRUE)
})
