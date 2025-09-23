#' @docType class
#' @title V2ColumnOrganizedStachUtilities
#' @description Provide helper functions for column organized stach
#'
#' @importFrom R6 R6Class
#' @export
#' @examples
#'\dontrun{
#' compressedFilePath <- 'Path of your json file'
#' compressedData <- jsonlite::read_json(path=compressedFilePath,
#' auto_unbox=TRUE)
#' stachUtilities <-
#' factset.protobuf.stachextensions::V2ColumnOrganizedStachUtilities
#' primaryTableIds <-
#' stachUtilities$public_methods$GetPrimaryTableIds(compressedData)
#' }
#'
#' @examples
#'\dontrun{
#' compressedFilePath <- 'Path of your json file'
#' compressedData <- jsonlite::read_json(path=compressedFilePath,
#' auto_unbox=TRUE)
#' utility <-
#' factset.protobuf.stachextensions::V2ColumnOrganizedStachUtilities$new()
#' deCompressedData <- utility$Decompress(compressedData)
#' }

V2ColumnOrganizedStachUtilities <- R6::R6Class(
  "V2ColumnOrganizedStachUtilities",
  public = list(
    #' @description  Get Primary Table Ids
    #' @param package  Stach Data which is represented as a Package object
    #' @return Returns list of primary table ids
    GetPrimaryTableIds = function(package) {
      primaryTableIds = package[["primaryTableIds"]]

      return(primaryTableIds)
    },
    #' @description  Get decompress stach data
    #' @param package  Stach Data which is represented as a Package object
    #' @return Returns decompressed stach data
    Decompress = function(package) {
      primaryTableIds <- self$GetPrimaryTableIds(package)

      for (tableId in primaryTableIds) {
        package <- private$DecompressTable(package, tableId)
      }

      return(package)
    }
  ),
  private = list(
    DecompressTable = function(package, primaryTableId) {
      definitionColumns <-
        package[["tables"]][[primaryTableId]][["definition"]][["columns"]]
      dataColumns <-
        package[["tables"]][[primaryTableId]][["data"]][["columns"]]

      for (definitionColumn in definitionColumns) {
        column <- dataColumns[[definitionColumn[["id"]]]]
        if ("ranges" %in% names(column)) {
          column <- private$DecompressColumn(column)
        }

        dataColumns[[definitionColumn[["id"]]]] <- column
      }

      package[["tables"]][[primaryTableId]][["data"]][["columns"]] <-
        dataColumns

      return(package)
    },
    DecompressColumn = function(column) {
      range <- column[["ranges"]]
      compressedValues <- column[["values"]]
      decompressedValues <- c()

      # Arrays start from element 1
      compressedValueIndex <- 1
      for (compressedValue in compressedValues) {
        currDecompressedIndex <- toString(length(decompressedValues))

        if (currDecompressedIndex %in% names(range)) {
          rangeValue <- range[[currDecompressedIndex]]
          loopRange <- 1:rangeValue

          for (i in loopRange) {
            decompressedValues <-
              append(decompressedValues, compressedValues[compressedValueIndex])
          }

        } else {
          decompressedValues <-
            append(decompressedValues, compressedValues[compressedValueIndex])
        }

        compressedValueIndex <- compressedValueIndex + 1
      }

      column[["ranges"]] <- NULL
      column[["values"]] <- decompressedValues
      return(column)
    }
  )
)
