test_that("format", {
  data(LDAdata)
  cell.line <- unique(LDAdata$name)[1]
  x <- subset.data.frame(LDAdata, subset = (name==cell.line) & (Group == 0))
  expect_equal(class(LDA_activity_single(x[,4:6])), "LDA_activity_object")
})

test_that("errors",{
  data(LDAdata)
  cell.line <- unique(LDAdata$name)[1]
  x <- subset.data.frame(LDAdata, subset = (name==cell.line) & (Group == 0))
  x <- x[,c(4,5,6,3,2,1)]
  # class
  x.test <- list(x)
  expect_error(LDA_activity_single(x.test))
  # size
  x.test <- x[1:2]
  expect_error(LDA_activity_single(x.test))
  x.test <- x[1:4]
  expect_error(LDA_activity_single(x.test))
  # numeric
  x.test <- x[1:3]
  x.test$`S-value` <- as.character(x.test$`S-value`)
  expect_error(LDA_activity_single(x.test))
  # minimum
  x.test <- x[,1:3]
  x.test$`S-value`[2] <- -3
  expect_error(LDA_activity_single(x.test))
  x.test$`S-value`[2] <- 0
  expect_error(LDA_activity_single(x.test))
  x.test <- x[,1:3]
  x.test$`# Clonal growth`[3] <- x.test$`# Tested`[3] + 1
  expect_error(LDA_activity_single(x.test))
})

test_that("cooperativity",{
  data(LDAdata)
  cell.line <- unique(LDAdata$name)[1]
  x <- subset.data.frame(LDAdata, subset = (name==cell.line) & (Group == 6))
  x <- x[,c(4,5,6,3,2,1)]
  y <- LDA_activity_single(x[,1:3])
  expect_identical(y$p.lin.Model < 1, TRUE)
  x <- subset.data.frame(LDAdata, subset = (name==cell.line) & (Group == 8))
  x <- x[,c(4,5,6,3,2,1)]
  y <- LDA_activity_single(x[,1:3])
  expect_identical(y$p.lin.Model < 1, TRUE)
})

test_that("no signal",{
  x <- data.frame("a" = c(512,256,128,64,32,16,8,4),
                  "b" = c(8,8,8,8,8,8,8,8),
                  "c" = c(0,0,0,0,0,0,0,0))
  colnames(x) <- c("cells","wells","positive")
  expect_error(LDA_activity_single(x))
})

test_that("one postive condition",{
  x <- data.frame("a" = c(512,256,128,64,32,16,8,4),
                  "b" = c(8,8,8,8,8,8,8,8),
                  "c" = c(0,0,0,0,0,0,0,4))
  colnames(x) <- c("cells","wells","positive")
  expect_error(LDA_activity_single(x))
})
