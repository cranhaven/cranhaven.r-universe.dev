test_that("table", {
  data(LDAdata)
  cell.line <- unique(LDAdata$name)[1]
  x <- subset.data.frame(LDAdata, subset = (name==cell.line))
  x <- x[,c(4,5,6,3,2,1)]
  expect_equal(class(LDA_table(x[,1:3])),"list")
  x$`S-value` <- as.character(x$`S-value`)
  expect_error(LDA_table(x[,1:4]))
  x <- subset.data.frame(LDAdata, subset = (name==cell.line))
  x <- x[,c(4,5,6,3,2,1)]
  LDA_table(x[,1:4])
})
test_that("strange things", {
  data(LDAdata)
  cell.line <- unique(LDAdata$name)[1]
  x <- subset.data.frame(LDAdata, subset = (name==cell.line))
  x <- x[,c(4,5,6,3,2,1)]
  x.shuff <- rbind(x[x$Group!=0,],x[x$Group==0,])
  expect_warning(LDA_table(x.shuff[,1:4]))
})
test_that("uncertainty choice", {
  data(LDAdata)
  cell.line <- unique(LDAdata$name)[1]
  x <- subset.data.frame(LDAdata, subset = (name==cell.line))
  x <- x[,c(4,5,6,3,2,1)]
  u.ep <- LDA_table(x[,1:4],uncertainty = "ep")
  u.act <- LDA_table(x[,1:4],uncertainty = "act")
  expect_identical(u.ep$act.CI.lb,u.act$act.CI.lb)
  expect_identical(u.ep$SF,u.act$SF)
  expect_true(u.ep$SF.CI.lb[4]!=u.act$SF.CI.lb[4])
})
