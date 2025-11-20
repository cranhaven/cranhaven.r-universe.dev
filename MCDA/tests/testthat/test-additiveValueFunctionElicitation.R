test_that("additiveValueFunctionElicitation works", {
  perfTable <- matrix(c( 3,10, 1, 
                         4,20, 2, 
                         2,20, 0, 
                         6,40, 0, 
                        30,30, 3), nrow=5, ncol=3, byrow=TRUE, 
                      dimnames=list(c("RER","METRO1","METRO2","BUS","TAXI"), 
                                    c("Price","Time","Comfort")) )
  altsRnk  <- setNames(c(1,2,2,3,4), rownames(perfTable))
  criteria <- setNames(c("min","min","max"), colnames(perfTable))
  breakpts <- setNames(c(3,4,4), colnames(perfTable))
  x1 <- additiveValueFunctionElicitation(perfTable, criteria, epsilon=.05, 
                                         alternativesRanks=altsRnk)
  expect_equal(x1$Kendall, 1)
})

test_that("additiveValueFunctionElicitation works with pairwise", {
  perfTable <- matrix(c( 3,10, 1, 
                         4,20, 2, 
                         2,20, 0, 
                         6,40, 0, 
                         30,30, 3), nrow=5, ncol=3, byrow=TRUE, 
                      dimnames=list(c("RER","METRO1","METRO2","BUS","TAXI"), 
                                    c("Price","Time","Comfort")) )
  criteria <- setNames(c("min","min","max"), colnames(perfTable))
  altsRnk  <- setNames(c(1,2,2,3,4), rownames(perfTable))
  altsPref <- matrix(c(   "RER", "METRO1", 
                       "METRO2",    "BUS", 
                          "BUS",   "TAXI"), nrow=3, ncol=2, byrow=TRUE)
  altsIndif <- matrix(c("METRO1","METRO2"), nrow=1, ncol=2, byrow=TRUE)
  x1 <- additiveValueFunctionElicitation(perfTable, criteria, epsilon=.05, 
                                         alternativesRanks=altsRnk)
  x1prime   <- additiveValueFunctionElicitation(perfTable, criteria, 
                                                epsilon=.05, 
                                                alternativesPreferences = altsPref, 
                                                alternativesIndifferences = altsIndif)
  expect_equal(x1$valueFunctions$Price  , x1prime$valueFunctions$Price  )
  expect_equal(x1$valueFunctions$Time   , x1prime$valueFunctions$Time   )
  expect_equal(x1$valueFunctions$Comfort, x1prime$valueFunctions$Comfort)
  x2 <- additiveValueFunctionElicitation(perfTable, criteria, epsilon=.05, 
                                         alternativesRanks = altsRnk, 
                                         criteriaIDs = c("Price", "Time"), 
                                         alternativesIDs = c("METRO1","METRO2","TAXI"))
  expect_equal(x2$overallValues[1], x2$overallValues[2], ignore_attr=TRUE)
})


