source("../test_constants.R")

testthat::test_that("row organized with col and row span",
                    {
                      rowcolSpanData <-
                        stachExtensionrow$ConvertToDataFrame(rowcolspanpackage, FALSE)

                      expect_equal(ncol(data.frame(rowcolSpanData[1])), 11, info = "Column count is mismatching it should be 4")

                      # comparing the first row data
                      df <- data.frame(rowcolSpanData[1])
                      expect_equal(unname(as.list(df[3, ])), roworgthirdrow, info =
                                     "first row of table is mismatched with the rowandcolspan stach data")

                      # comparing the second row data
                      expect_equal(unname(as.list(df[4, ])), roworgfourthrow, info =
                                     "second row of table is mismatched with the rowandcolspan stach data")
                      # comparing count of meta data items
                      colOrgMetadata <-
                        stachExtensionrow$GetMetadata(rowcolspanpackage)
                      expect_equal(length(colOrgMetadata[[1]]), 1, info = "There is an incorrect amount of Metadata items")

                      # comparing values of meta data items with key and value
                      expect_equal(colOrgMetadata[[1]]$`m_h`[[1]],
                                   "Risk Analysis",
                                   info = "Mis match of Meta Data Item value with given Key Value pair")

                    })
