
#' @docType class
#' @title V2RowOrganizedStachExtension
#' @description The purpose of this class is to provide the helper methods for converting stach(row organized) to Tabular format
#' and to get meta data from stach
#'
#' @importFrom R6 R6Class
#' @export
#' @examples
#'\dontrun{
#' package <- 'Stach data which is converted into RowOrganized Package'
#' stachExtensionrow <-
#' factset.protobuf.stachextensions::V2RowOrganizedStachExtension$new()
#' dataFrame <- stachExtensionrow$ConvertToDataFrame(package)
#' }

V2RowOrganizedStachExtension <- R6::R6Class(
  "V2RowOrganizedStachExtension",
  public = list(
    #' @description  This function is used for converting stach to Tabular format
    #' @param package  Stach Data which is represented as a Package object
    #' @param mergeHeaders Accepts Type as BOOLEAN,by default takes TRUE.
    #'  If the value is TRUE, headers will be merged with column data as output in dataframe
    #'  If the value is FALSE, headers will be added to column data as output in dataframe
    #' @return Returns the List of data frame for the stach data

    ConvertToDataFrame = function(package, mergeHeaders) {
      if (missing(mergeHeaders))
      {
        mergeHeaders <- TRUE
      }
      dataFramesList <- list()
      noofTables <- length(package$tables)
      for (tblindx in 1:noofTables)
      {
        for (primaryTableid in package$tables[[tblindx]]$key) {
          rowOrgDataFrame <-
            private$GenerateTableforRowOrg(package, primaryTableid, mergeHeaders)
          dataFramesList[[primaryTableid]] <- rowOrgDataFrame
        }
      }
      dataFramesList
    },

    #' @description  This function is used to get meta data from stach
    #' @param package Stach Data which is represented as a Package object
    #' @return Returns the List of metadata for the stach data

    GetMetadata = function(package) {
      metadataList <- list()
      noofTables <- length(package$tables)
      for (tblindx in 1:noofTables)
      {
        for (primaryTableid in package$tables[[tblindx]]$key) {
          metadata <-
            private$GenerateMetadataforRowOrg(package, primaryTableid)
          metadataList[[primaryTableid]] <- metadata
        }
      }
      metadataList
    }
  ),
  private = list(
    GenerateTableforRowOrg = function(package, primaryTableId, mergeHeaders) {
      tableData <-
        V2StachUtilities$public_methods$GetMappingObject(package$tables, primaryTableId)$value
      headers <- list()
      totalColCount <- length(tableData$definition$columns)
      primaryTabledef <- tableData$definition$columns
      headercolumnIds <-
        lapply(tableData$definition$columns, function(seriesDef)
          seriesDef$id)
      firstRow <- tableData$data$rows[[1]]$row_type
      headerRowType <-
        factset.protobuf.stach.v2.RowOrganizedPackage.Row.RowType$Header

      #If first row is not header row, considering it as simplified row format.
      if (firstRow != headerRowType)
      {
        for (i in 1:totalColCount)
        {
          if (nchar(primaryTabledef[[i]]$description) > 1)
          {
            headers <- append(headers, primaryTabledef[[i]]$description)
          }
          else{
            headers <- append(headers, primaryTabledef[[i]]$name)
          }
        }
      }

      rowCount <- length(tableData$data$rows)
      rowdataList <- list()
      defColCount <- length(tableData$definition$columns)
      # List to store the info about the values that needs to be spread based on rowspan
      rowSpanSpreadList <- list()
      rowSpanDataFrame <- data.frame()

      for (rowindex in 1:rowCount)
      {
        curRowType <- tableData$data$rows[[rowindex]]$row_type
        actualPosition <-
          0 # The actual column position (by considering the rowspan, colspan spread).

        if (curRowType == headerRowType)
        {
          headerRowValues <- tableData$data$rows[[rowindex]]$cells$values
          headerRowCount <- length(headerRowValues)
          headerCellArray <-
            tableData$data$rows[[rowindex]]$header_cell_details

          # Checking and adding values at the start of the row if any based on the rowspan info
          # available from the previously processed rows.
          if (!is.null(rowSpanDataFrame))
          {
            spreadValuesList <-
              private$CheckAddRowSpanItems(actualPosition, rowindex, rowSpanDataFrame)
            if (length(spreadValuesList) >= 1)
            {
              actualPosition = actualPosition + length(spreadValuesList)
              for (splstval in 1:length(spreadValuesList))
              {
                headers <- append(headers, spreadValuesList[splstval])
              }
            }
          }


          for (i in 1:headerRowCount)
          {
            rowSpan <- headerCellArray[[i]]$value$rowspan
            colSpan <- headerCellArray[[i]]$value$colspan
            if (rowSpan == 0)
            {
              rowSpan <- 1
            }
            if (colSpan == 0)
            {
              colSpan <- 1
            }

            hdrRowVal <-
              V2StachUtilities$public_methods$ValueToString(headerRowValues[[i]])

            # If rowspan > 1, the value needs to be spread across multiple rows at the current position.
            # storing the position, rowspan number and the actual value to the rowSpanSpreadList.
            if (rowSpan > 1)
            {
              rowSpanSpreadList <-
                append(
                  rowSpanSpreadList,
                  list(
                    Position = actualPosition,
                    RowSpan = rowSpan,
                    ColSpan = colSpan,
                    cellvalue = hdrRowVal
                  )
                )
            }
            for (v in 1:colSpan)
            {
              stringValue <-
                V2StachUtilities$public_methods$ValueToString(headerRowValues[[i]])
              headers <- append(headers, stringValue)
              actualPosition <- actualPosition + 1

              # After incrementing column position from above line, checking if any value has to be
              # added at the new column position based on row spread list

              spreadValuesList <-
                private$CheckAddRowSpanItems(actualPosition, rowindex, rowSpanDataFrame)
              if (length(spreadValuesList) >= 1)
              {
                actualPosition = actualPosition + length(spreadValuesList)
                for (splstval in 1:length(spreadValuesList))
                {
                  headers <- append(headers, spreadValuesList[splstval])
                }
              }

            }
          }
          if (length(rowSpanSpreadList) > 1)
          {
            rowSpanSpreadListdata <- unlist(rowSpanSpreadList)
            rowSpanDataMatrix <-
              matrix(rowSpanSpreadListdata,
                     ncol = 4,
                     byrow = TRUE)
            rowSpanDataFrame <-
              data.frame(rowSpanDataMatrix, stringsAsFactors = FALSE)
            names(rowSpanDataFrame) <-
              list("Position", "RowSpan", "ColSpan", "CellValue")
          }
        }
        else
        {
          rowDataMap <- tableData$data$rows[[rowindex]]$values$fields

          # here fields are of type list.
          # In order to read the value using columnid from fields,
          # keys and values of fields are converted to key-value pair using data frame.

          fieldsKeys <- c() # stores the fields keys (example:"col_0")
          fieldsValues <- c() # stores the fields values (example:"Utilities")

          for(rowDataMapIndex in 1:length(rowDataMap)){
            fieldsKeys <- append(fieldsKeys, rowDataMap[[rowDataMapIndex]]$key)
            columnValue <- rowDataMap[[rowDataMapIndex]]$value
            stringValue <-
              V2StachUtilities$public_methods$ValueToString(columnValue)
            fieldsValues <- append(fieldsValues, stringValue)
          }

          # Stored the fields key-value in a data frame("col_0":"Utilities")
          fieldsKeyValue = data.frame(row.names=fieldsKeys , val=fieldsValues)

          #Reading the value from fieldsKeyValue data frame using columnid of columndefinition(example:"col_0")
          for (colIndex in 1:defColCount)
          {
            rowVal <- fieldsKeyValue[tableData$definition$columns[[colIndex]]$id,]
            rowdataList <- append(rowdataList, rowVal)
          }
        }
      }
      rowdata <- list()
      rowdata <- unlist(rowdataList)
      rowDataMatrix <-
        matrix(rowdata,
               ncol = length(headercolumnIds),
               byrow = TRUE)
      rowDataFrame <-
        data.frame(rowDataMatrix, stringsAsFactors = FALSE)

      headers <- unlist(headers)
      headerDataMatrix <-
        matrix(headers, ncol = totalColCount, byrow = TRUE)
      headerDataFrame <-
        data.frame(headerDataMatrix, stringsAsFactors = FALSE)

      header <- NULL
      for (n in 1:ncol(headerDataFrame))
      {
        chkSameVal <-
          identical(headerDataFrame[1, n], headerDataFrame[2, n])
        # In case of multi headers file format
        # make header name as unique if the two headers names are same
        if (chkSameVal == TRUE)
        {
          header <- append(header, headerDataFrame[1, n])
        }
        else
        {
          header <-
            append(header, paste(headerDataFrame[, n], collapse = "_"))
        }
      }
      # If the user wants headers also in the column data lets bind header and row data into single data frame
      if (mergeHeaders == FALSE)
      {
        rowDataFrame <- rbind(headerDataFrame, rowDataFrame)
      }

      names(rowDataFrame) <- header

      return(rowDataFrame)
    },

    # Helper method to store the master data of all the rows and which is used for constructing nested headers
    CheckAddRowSpanItems = function(pos, dfrowindex, rowSpanDataFrame)
    {
      posVal <- which(rowSpanDataFrame$Position == pos)
      lenPosVal <- length(posVal)
      rowSpanItemList <- list()
      recOutput <- list()
      dfRowSpan <- rowSpanDataFrame[posVal, 2]
      dfColSpan <- rowSpanDataFrame[posVal, 3]
      if ((lenPosVal > 0) && (dfrowindex <= dfRowSpan))
      {
        rowCellVal <- rowSpanDataFrame[posVal, 4]
        if (length(rowCellVal) > 0)
        {
          rowSpanItemList <- append(rowSpanItemList, rowCellVal)
          for (u in 1:dfColSpan)
          {
            pos = pos + u
          }

          recOutput <-
            private$CheckAddRowSpanItems(pos, dfrowindex, rowSpanDataFrame)
          rowSpanItemList <- append(rowSpanItemList, recOutput)

          return(rowSpanItemList)
        }
      }
    },

    # Helper method to retrieve metadata based on table id
    GenerateMetadataforRowOrg = function(package, primaryTableId) {
      tableData <-
        V2StachUtilities$public_methods$GetMappingObject(package$tables, primaryTableId)$value
      metadataItemsCount <- length(tableData$data$table_metadata)

      metadataValues <- list()
      metadataKeys <- list()
      metadataKeys <-
        lapply(tableData$data$table_metadata, function(seriesDef)
          seriesDef$key)

      for (i in 1:metadataItemsCount)
      {
        metadataItemCount <-
          length(tableData$data$table_metadata[[i]]$value$value$list_value$values)
        # Check the metadata item data type
        # if the count > 0 : the meta data item value is of type - array
        if (metadataItemCount > 0)
        {
          vector <- NULL
          for (j in 1:metadataItemCount)
          {
            metadataKeyValue <-
              tableData$data$table_metadata[[i]]$value$value$list_value$values[[j]]
            stringValue <-
              V2StachUtilities$public_methods$ValueToString(metadataKeyValue)
            vector <- c(vector, stringValue)
          }
          convertedList <-
            as.list(vector) # converting vector to list
          metadataValues <-
            append(metadataValues, list(convertedList))
        }
        else
        {
          vector <- NULL
          metadataKeyValue <-
            tableData$data$table_metadata[[i]]$value$value
          stringValue <-
            V2StachUtilities$public_methods$ValueToString(metadataKeyValue)
          vector <- c(vector, stringValue)
          convertedList <-
            as.list(vector) # converting vector to list
          metadataValues <-
            append(metadataValues, list(convertedList))
        }
      }
      metadata <- setNames(metadataValues, metadataKeys)
      return(metadata)
    }
  )
)
