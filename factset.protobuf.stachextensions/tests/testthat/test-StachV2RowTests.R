source("../test_constants.R")

testthat::test_that("row organized",
                    {
                      rowOrgData <- stachExtensionrow$ConvertToDataFrame(rowpackage, FALSE)

                      expect_equal(ncol(data.frame(rowOrgData[1])), 4, info = "Column count is mismatching it should be 4")
                      expect_equal(nrow(data.frame(rowOrgData[1])), 635, info = "Row count is mismatching it should be 635")

                      # comparing the first row data
                      df <- data.frame(rowOrgData[1])
                      expect_equal(unname(as.list(df[1, ])), roworgfirstrow, info =
                                     "first row of table is mismatched with the row stach data")

                      # comparing the second row data
                      expect_equal(unname(as.list(df[2, ])), roworgsecondrow, info =
                                     "second row of table is mismatched with the row stach data")

                      # comparing count of meta data items
                      rowOrgMetadata <-
                        stachExtensionrow$GetMetadata(rowpackage)
                      expect_equal(length(rowOrgMetadata[[1]]), 18, info = "There is an incorrect amount of Metadata items")

                      # comparing values of meta data items with key and value
                      expect_equal(
                        rowOrgMetadata[[1]]$`Grouping Frequency`[[2]],
                        "Industry - FactSet - Beginning of Period",
                        info = "Mis match of Meta Data Item value with given Key Value pair"
                      )
                    })
