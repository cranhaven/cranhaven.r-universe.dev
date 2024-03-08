###############################################
#------------------- tests -------------------#
###############################################
test_that("the result has the correct class", {
  # load the dataset
  data(rock)
  set.seed(9)
  out <- regBBNR(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
  out.frm <- regBBNR(formula = perm ~ ., data = rock)
  
  # Default method
  expect_s3_class(out, "rfdata")
  # Class formula method
  expect_s3_class(out.frm, "rfdata")
})

###############################################
###############################################
###############################################
test_that("the number of idclean plus idnoise equals the number of dataset samples", {
  # load the dataset
  data(rock)
  set.seed(9)
  out <- regBBNR(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
  out.frm <- regBBNR(formula = perm ~ ., data = rock)
  
  # Default method
  expect_true(length(out$idclean) + length(out$idnoise) == nrow(rock))
  # Class formula method
  expect_true(length(out.frm$idclean) + length(out.frm$idnoise) == nrow(rock))
})

###############################################
###############################################
###############################################
test_that("the idclean and idnoise are equal to dataset rownames", {
  # load the dataset
  data(rock)
  set.seed(9)
  out <- regBBNR(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
  out.frm <- regBBNR(formula = perm ~ ., data = rock)
  
  # Default method
  expect_true(any(sort(c(out$idclean,out$idnoise)) == as.integer(rownames(rock))))
  # Class formula method
  expect_true(any(sort(c(out.frm$idclean,out.frm$idnoise)) == as.integer(rownames(rock))))
})

###############################################
###############################################
###############################################
test_that("the original dataset can be correctly reconstructed from the rfdata object", {
  # load the dataset
  data(rock)
  set.seed(9)
  out <- regBBNR(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
  out.frm <- regBBNR(formula = perm ~ ., data = rock)
  
  # Default method
  dataClean <- cbind(out$xclean, out$yclean)
  dataNoisy <- cbind(out$xnoise, out$ynoise)
  colnames(dataClean) = colnames(dataNoisy) = colnames(rock)
  processData <- rbind(dataClean, dataNoisy)
  processData <- processData[order(as.numeric(row.names(processData))), ]
  expect_equal(processData, rock)
  # Class formula method
  dataClean.frm <- cbind(out.frm$xclean, out.frm$yclean)
  dataNoisy.frm <- cbind(out.frm$xnoise, out.frm$ynoise)
  colnames(dataClean.frm) = colnames(dataNoisy.frm) = colnames(rock)
  processData.frm <- rbind(dataClean.frm, dataNoisy.frm)
  processData.frm <- processData.frm[order(as.numeric(row.names(processData.frm))), ]
  expect_equal(processData.frm, rock)
})

###############################################
###############################################
###############################################
test_that("y is a double vector", {
  # load the dataset
  data(rock)
  set.seed(9)
  out <- regBBNR(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
  out.frm <- regBBNR(formula = perm ~ ., data = rock)
  
  # Default method
  dataClean <- cbind(out$xclean, out$yclean)
  expect_true(is.numeric(dataClean[,ncol(dataClean)]))
  # Class formula method
  dataClean.frm <- cbind(out.frm$xclean, out.frm$yclean)
  expect_true(is.numeric(dataClean.frm[,ncol(dataClean.frm)]))
})

###############################################
###############################################
###############################################
test_that("the result has the correct sum.rfdata", {
  # load the dataset
  data(rock)
  set.seed(9)
  out <- regBBNR(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
  out.frm <- regBBNR(formula = perm ~ ., data = rock)
  
  # Default method
  sm <- summary(out, showid = TRUE)
  expect_s3_class(sm, "sum.rfdata")
  # Class formula method
  sm.frm <- summary(out.frm, showid = TRUE)
  expect_s3_class(sm.frm, "sum.rfdata")
})

###############################################
###############################################
###############################################
test_that("the result shown the print of summary", {
  # load the dataset
  data(rock)
  set.seed(9)
  out <- regBBNR(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
  out.frm <- regBBNR(formula = perm ~ ., data = rock)
  
  # Default method
  sm <- summary(out, showid = TRUE)
  expect_output(print(sm))
  # Class formula method
  sm.frm <- summary(out.frm, showid = TRUE)
  expect_output(print(sm.frm))
})

###############################################
###############################################
###############################################
test_that("the result shown the print of summary", {
  # load the dataset
  data(rock)
  set.seed(9)
  out <- regBBNR(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
  out.frm <- regBBNR(formula = perm ~ ., data = rock)
  
  # Default method
  expect_output(print(out))
  # Class formula method
  expect_output(print(out.frm))
})

###############################################
###############################################
###############################################
test_that("Invalid threshold value", {
  # load the dataset
  data(rock)
  set.seed(9)
  
  # Default method
  expect_error(regBBNR(x = rock[,-ncol(rock)], y = rock[,ncol(rock)], t=2))
  # Class formula method
  expect_error(regBBNR(formula = perm ~ ., data = rock, t=2))
})

###############################################
###############################################
###############################################
test_that("Invalid k value", {
  data(rock)
  set.seed(9)
  
  # Default method
  expect_error(regBBNR(x = rock[,-ncol(rock)], y = rock[,ncol(rock)], k=0))
  # Class formula method
  expect_error(regBBNR(formula = perm ~ ., data = rock, k=0))
})

###############################################
###############################################
###############################################
test_that('plot function',{
  
  # load the dataset
  data(rock)
  set.seed(9)
  out <- regBBNR(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
  
  bar_plots <- plot(x = out, var = c(1:4), fun = "mean")
  expect_s3_class(bar_plots, "ggplot")
  
})