source("../test_constants.R")

testthat::test_that("simplified row organized",
                    {
                      simpRowOrgData <-
                        stachExtensionrow$ConvertToDataFrame(simplifiedrowpackage, FALSE)

                      expect_equal(ncol(data.frame(simpRowOrgData[1])), 6, info = "Column count is mismatching it should be 6")
                      expect_equal(nrow(data.frame(simpRowOrgData[1])), 62, info = "Row count is mismatching it should be 62")

                      # comparing the first row data
                      df <- data.frame(simpRowOrgData[1])
                      expect_equal(unname(as.list(df[1,])), firstRow, info =
                                     "first row of table is mismatched with the stach data")

                      # comparing the second row data
                      expect_equal(unname(as.list(df[2,])), secondRow, info =
                                     "second row of table is mismatched with the stach data")

                      # comparing count of meta data items
                      simplifiedRowOrgMetadata <- stachExtensionrow$GetMetadata(simplifiedrowpackage)
                      expect_equal(length(simplifiedRowOrgMetadata[[1]]), 18, info = "There is an incorrect amount of Metadata items")

                      # comparing values of meta data items with key and value
                      expect_equal(simplifiedRowOrgMetadata[[1]]$`Grouping Frequency`[[2]],"Industry - Beginning of Period",info = "Mis match of Meta Data Item value with given Key Value pair")
                    })

testthat::test_that("Simplified Row With Struct",
                    {
                      simpRowOrgStructData <-
                        stachExtensionrow$ConvertToDataFrame(simplifiedrowstructpackage, FALSE)

                      expect_equal(ncol(data.frame(simpRowOrgStructData[1])), 5, info = "Column count is mismatching it should be 5")
                      expect_equal(nrow(data.frame(simpRowOrgStructData[1])), 2, info = "Row count is mismatching it should be 2")

                      # comparing the first row data
                      df <- data.frame(simpRowOrgStructData[1])
                      expect_equal(unname(as.list(df[1,])), structfirstrow, info =
                                     "first row of table is mismatched with the stach data")

                      # comparing the second row data
                      expect_equal(unname(as.list(df[2,])), structsecondrow, info =
                                     "second row of table is mismatched with the stach data")

                      # comparing count of meta data items
                      simplifiedRowOrgMetadata <- stachExtensionrow$GetMetadata(simplifiedrowstructpackage)
                      expect_equal(length(simplifiedRowOrgMetadata[[1]]), 1, info = "There is an incorrect amount of Metadata items")

                      # comparing values of meta data items with key and value
                      expect_equal(simplifiedRowOrgMetadata[[1]]$CalculationId[[1]],"78647885fa4f4594b775a1431e62090c:",info = "Mis match of Meta Data Item value with given Key Value pair")
                    })
