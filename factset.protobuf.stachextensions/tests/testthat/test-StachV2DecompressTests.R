source("../test_constants.R")

testthat::test_that("column organized utilities, get primary table ids",
                    {
                      ids <-
                        factset.protobuf.stachextensions::V2ColumnOrganizedStachUtilities$public_methods$GetPrimaryTableIds(compressedData)

                      expect_equal(length(ids), 1, info = "Length of ids should be 1")
                      expect_equal(ids[[1]], "a649ec50-7e58-443d-b791-1340e9eebf24", info = "Primary table id should be a649ec50-7e58-443d-b791-1340e9eebf24")
                    })

testthat::test_that("column organized utilities, decompress",
                    {
                      utility <-
                        factset.protobuf.stachextensions::V2ColumnOrganizedStachUtilities$new()

                      package <- utility$Decompress(compressedData)

                      primaryTableId <- "a649ec50-7e58-443d-b791-1340e9eebf24"
                      actualData <-
                        package[["tables"]][[primaryTableId]][["data"]][["columns"]][["1"]][["values"]]

                      expectedData <- c(
                        list(NULL, NULL, NULL),
                        "Americas",
                        "Asia Pacific",
                        "Europe",
                        "Middle East and Africa",
                        list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)
                      )

                      expect_equal(length(actualData), length(expectedData))

                      range <- 1:length(actualData) + 1
                      for (i in range) {
                        expect_equal(actualData[i], expectedData[i])
                      }
                    })
