source("../test_constants.R")

testthat::test_that("column organized",
                    {
                      colOrgData <- stachExtensioncol$ConvertToDataFrame(package, FALSE)

                      expect_equal(ncol(data.frame(colOrgData[1])), 6, info = "Column count is mismatching it should be 6")
                      expect_equal(nrow(data.frame(colOrgData[1])), 62, info = "Row count is mismatching it should be 62")

                      # comparing the first row data
                      df <- data.frame(colOrgData[1])
                      expect_equal(unname(as.list(df[1,])), firstRow, info =
                                     "first row of table is mismatched with the stach data")

                      # comparing the second row data
                      expect_equal(unname(as.list(df[2,])), secondRow, info =
                                     "second row of table is mismatched with the stach data")

                      # comparing count of meta data items
                      colOrgMetadata <- stachExtensioncol$GetMetadata(package)
                      expect_equal(length(colOrgMetadata[[1]]), 18, info = "There is an incorrect amount of Metadata items")

                      # comparing values of meta data items with key and value
                      expect_equal(colOrgMetadata[[1]]$`Grouping Frequency`[[2]],"Industry - Beginning of Period",info = "Mis match of Meta Data Item value with given Key Value pair")
                    })

testthat::test_that("struct column organized",
                    {
                      colOrgData <- stachExtensioncol$ConvertToDataFrame(structpackage, FALSE)

                      expect_equal(ncol(data.frame(colOrgData[1])), 5, info = "Column count is mismatching it should be 5")
                      expect_equal(nrow(data.frame(colOrgData[1])), 2, info = "Row count is mismatching it should be 2")

                      # comparing the first row data
                      df <- data.frame(colOrgData[1])
                      expect_equal(unname(as.list(df[1,])), structfirstrow, info =
                                     "first row of table is mismatched with the stach data")

                      # comparing the second row data
                      expect_equal(unname(as.list(df[2,])), structsecondrow, info =
                                     "second row of table is mismatched with the stach data")

                      # comparing count of meta data items
                      colOrgMetadata <- stachExtensioncol$GetMetadata(structpackage)
                      expect_equal(length(colOrgMetadata[[1]]), 1, info = "There is an incorrect amount of Metadata items")

                      # comparing values of meta data items with key and value
                      expect_equal(colOrgMetadata[[1]]$CalculationId[[1]],"ced27774514a41f499b70b4029646789:",info = "Mis match of Meta Data Item value with given Key Value pair")
                    })
