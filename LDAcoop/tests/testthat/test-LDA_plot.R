test_that("plot", {
  data(LDAdata)
  cell.line <- unique(LDAdata$name)[1]
  x <- subset.data.frame(LDAdata, subset = (name==cell.line))
  x <- x[,c(4,5,6,3,2,1)]
  act <- LDA_activity(x[,1:4])
  expect_error(LDA_plot(act))
  LDA_plot(x)
  LDA_plot(x,uncertainty = "act")
  x <- subset.data.frame(LDAdata, subset = (name==cell.line) & (Group==0))
  x <- x[,c(4,5,6,3,2,1)]
  act <- LDA_activity_single(x[,1:3])
  LDA_plot(x)
  LDA_plot(x[,1:3])
  expect_error(LDA_plot(LDA_tab = x[,1:4],uncertainty = 'Taylor'))
  LDA_plot(x,uncertainty.band = T)
})
