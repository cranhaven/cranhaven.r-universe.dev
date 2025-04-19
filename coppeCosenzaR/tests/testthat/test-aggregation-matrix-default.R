require(testthat)


#load test data
#
# Option
#
ofa1 <- Option.factor.availability(Factor("fator0"), "R")
ofa2 <- Option.factor.availability(Factor("fator1"), "Ex")
ofa3 <- Option.factor.availability(Factor("fator2"), "R")
ofa4 <- Option.factor.availability(Factor("fator3"), "W")
ofa5 <- Option.factor.availability(Factor("fator4"), "Ex")

option.resources1 <- Option.resources(list(ofa1, ofa2, ofa3))
option.resources2 <- Option.resources(list(ofa2, ofa3, ofa4))
option.resources3 <- Option.resources(list(ofa3, ofa4, ofa5))

option1 <- Option("o3",option.resources1)
option2 <- Option("o2",option.resources2)
option3 <- Option("o2",option.resources3)

option.portfolio <- Option.portfolio(list(option1,option3))

#
#
#  Projects
#

pc1 <- Project.criterion(Factor("fator0"), "LC", FALSE)
pc2 <- Project.criterion(Factor("fator1"), "C", FALSE)
pc3 <- Project.criterion(Factor("fator2"), "I", FALSE)
pc4 <- Project.criterion(Factor("fator3"), "C", TRUE)
pc5 <- Project.criterion(Factor("fator4"), "Cr", TRUE)

project.criteria1 <- Project.criteria(list(pc1, pc2, pc3))
project.criteria2 <- Project.criteria(list(pc2, pc3, pc4))
project.criteria3 <- Project.criteria(list(pc3, pc4, pc5))

project1 <- Project("p3",project.criteria1)
project2 <- Project("p2",project.criteria2)
project3 <- Project("p2",project.criteria1)


project.portfolio <- Project.portfolio(list(project1,project2))

#
#
# Factors under consideration
#
factors.of.interest <-
  Factors.of.interest(list(Factor("fator2")))



context("\n\n Aggregation.Matrix.default  Aggregate\n")
test_that("Aggregation.Matrix.default - function Agregate \n", {

  expect_is(Aggregate(new("Aggregation.matrix.default"), "Cr", "G", TRUE, 4), "numeric")
  expect_equal(Aggregate(new("Aggregation.matrix.default"), "Cr", "G", TRUE, 4), -1)
  expect_equal(Aggregate(new("Aggregation.matrix.default"), "Cr", "G", FALSE, 4), 0)

}
)

context("\n\n Aggregation.Matrix.default  AggregateMatrix\n")

test_that("Aggregation.Matrix.default - function AggregateMatrix \n", {

  project.portfolio.as.data.frame <- as.data.frame(project.portfolio)
  project.portfolio.as.data.frame <- project.portfolio.as.data.frame[,3, drop = FALSE]
  project.portfolio.specifics.as.data.frame  <- as.data.frame(project.portfolio,,TRUE)
  project.portfolio.specifics.as.data.frame <- project.portfolio.specifics.as.data.frame[,3, drop = FALSE]
  option.portfolio.as.data.frame  <- as.data.frame(option.portfolio)
  option.portfolio.as.data.frame <- option.portfolio.as.data.frame[,3, drop = FALSE]

  #cat("\n")
  # print(project.portfolio.as.data.frame)
  # cat("\n")
  # print(project.portfolio.specifics.as.data.frame)
  # cat("\n")
  # print(option.portfolio.as.data.frame)




  expect_is(AggregateMatrix(new("Aggregation.matrix.default"),
    project.portfolio.as.data.frame,
    project.portfolio.specifics.as.data.frame,
    option.portfolio.as.data.frame)
    , "data.frame")
}
)

