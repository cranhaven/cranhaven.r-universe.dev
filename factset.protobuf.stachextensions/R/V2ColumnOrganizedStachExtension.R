

#' @docType class
#' @title V2ColumnOrganizedStachExtension
#' @description The purpose of this class is to provide the helper methods for converting stach(column organized) to Tabular format
#' and to get meta data from stach
#'
#' @importFrom R6 R6Class
#' @export
#' @examples
#'\dontrun{
#' package <- 'Stach data which is converted into ColumnOrganized Package'
#' stachExtensioncol <-
#' factset.protobuf.stachextensions::V2ColumnOrganizedStachExtension$new()
#' dataFrame <- stachExtensioncol$ConvertToDataFrame(package)
#' }

V2ColumnOrganizedStachExtension <- R6::R6Class(
  "V2ColumnOrganizedStachExtension",
  public = list(
    #' @description  This function is used for converting stach to Tabular format
    #' @param package  Stach Data which is represented as a Package object
    #' @param mergeHeaders Accepts Type as BOOLEAN,by default takes TRUE.
    #' If the value is TRUE, headers will be merged with column data as output in dataframe
    #' If the value is FALSE, headers will be added to column data as output in dataframe
    #' @return Returns the List of data frame for the stach data

    ConvertToDataFrame = function(package, mergeHeaders) {
      if (missing(mergeHeaders))
      {
        mergeHeaders <- TRUE
      }
      dataFramesList <- list()
      for (primaryTableid in package$primary_table_ids) {
        colOrgDataFrame <-
          private$GenerateTableforColOrg(package, primaryTableid, mergeHeaders)
        dataFramesList[[primaryTableid]] <- colOrgDataFrame
      }
      dataFramesList
    },

    #' @description  This function is used to get meta data from stach
    #' @param package Stach Data which is represented as a Package object
    #' @return Returns the List of metadata for the stach data

    GetMetadata = function(package) {
      metadataList <- list()
      for (primaryTableid in package$primary_table_ids) {
        metadata <-
          private$GenerateMetadataforColOrg(package, primaryTableid)
        metadataList[[primaryTableid]] <- metadata
      }
      metadataList
    }
  ),

  private = list(
    GenerateTableforColOrg = function(package, primaryTableId, mergeHeaders) {
      primaryTable <-
        V2StachUtilities$public_methods$GetMappingObject(package$tables, primaryTableId)$value
      headerTableId <- primaryTable$definition$header_table_id
      headerTable <-
        V2StachUtilities$public_methods$GetMappingObject(package$tables, headerTableId)$value
      columnIds <-
        lapply(primaryTable$definition$columns, function(seriesDef)
          seriesDef$id)
      headercolumnIds <-
        lapply(headerTable$definition$columns, function(seriesDef)
          seriesDef$id)

      headerRowCount <- length(headerTable$data$rows)
      headerDataRowList <-
        lapply(headerTable$data$rows, function(seriesDef)
          seriesDef$id)
      headerDataRowList <- unlist(headerDataRowList)

      #Constructs the header data

      headers <- list()
      defColCount <- length(primaryTable$definition$columns)
      primaryTabledef <- primaryTable$definition$columns
      if (nchar(headerTableId) > 0)
      {
        for (columnId in headercolumnIds)
        {
          for (j in 1:defColCount)
          {
            if (primaryTabledef[[j]]$is_dimension == TRUE)
            {
              headers <- append(headers, primaryTabledef[[j]]$name)
              next
            }
            seriesData <-
              V2StachUtilities$public_methods$GetMappingObject(headerTable$data$columns, columnId)$value
            lstheaderData <- as.list(seriesData$values)
            index <-
              match(primaryTabledef[[j]]$header_id, headerDataRowList)
            headers <-
              append(
                headers,
                V2StachUtilities$public_methods$ValueToString(lstheaderData$values[[index]])
              )
          }
        }

      } else{
        for (j in 1:defColCount)
        {
          if (nchar(primaryTabledef[[j]]$description) > 0)
          {
            headers <- append(headers, primaryTabledef[[j]]$description)
          }
          else{
            headers <- append(headers, primaryTabledef[[j]]$name)
          }
        }
      }
      headers <- unlist(headers)
      headerDataMatrix <-
        matrix(headers, ncol = length(columnIds), byrow = TRUE)
      headerDataFrame <-
        data.frame(headerDataMatrix, stringsAsFactors = FALSE)
      rowCount <- length(primaryTable$data$rows)
      if (rowCount == 0)
      {
        #For Column Stach Extension File the format is different
        #as the data is represented without rows
        lstRowcount <-
          primaryTable$data$columns[[1]]$value$values$values
        rowCount <- length(lstRowcount)
      }

      # Constructs the column data
      columnData <- list()

      for (datacolumnId in columnIds)
      {
        colseriesData <-
          V2StachUtilities$public_methods$GetMappingObject(primaryTable$data$columns, datacolumnId)$value
        lstcolData <- as.list(colseriesData$values)
        seriesDefinitionColumn <-
          V2StachUtilities$public_methods$GetMappingObject(primaryTable$definition$columns, datacolumnId, keyname = "id")

        for (j in 1:rowCount)
        {
          columnValue <- lstcolData$values[[j]]
          stringValue <-
            V2StachUtilities$public_methods$ValueToString(columnValue)
          columnData <- append(columnData, stringValue)
        }
      }

      columnData <- unlist(columnData)
      columnDataMatrix <-
        matrix(columnData, ncol = length(columnIds), byrow = FALSE)
      columnDataFrame <-
        data.frame(columnDataMatrix, stringsAsFactors = FALSE)

      header <- NULL
      for (n in 1:ncol(headerDataFrame))
      {
        chkSameVal <-
          identical(headerDataFrame[1, n], headerDataFrame[2, n])
        #In case of multi headers file format
        #make header name as unique if the two headers names are same
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
        columnDataFrame <- rbind(headerDataFrame, columnDataFrame)
      }

      names(columnDataFrame) <- header
      return(columnDataFrame)
    },

    # Helper method to retrieve metadata based on table id
    GenerateMetadataforColOrg = function(package, primaryTableId) {
      tableData <-
        V2StachUtilities$public_methods$GetMappingObject(package$tables, primaryTableId)$value

      metadataItemsCount <- length(tableData$data$metadata$items)

      metadataValues <- list()
      metadataKeys <- list()
      metadataKeys <-
        lapply(tableData$data$metadata$items, function(seriesDef)
          seriesDef$key)

      for (i in 1:metadataItemsCount)
      {
        metadataItemCount <-
          length(tableData$data$metadata$items[[i]]$value$value$list_value$values)
        # Check the metadata item data type
        # if the count > 0 : the meta data item value is of type - array
        if (metadataItemCount > 0)
        {
          vector <- NULL
          for (j in 1:metadataItemCount)
          {
            metadataKeyValue <-
              tableData$data$metadata$items[[i]]$value$value$list_value$values[[j]]
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
          metadataKeyValue <- tableData$data$metadata$items[[i]]$value$value
          stringValue <-
            V2StachUtilities$public_methods$ValueToString(metadataKeyValue)
          metadataValues <- append(metadataValues, stringValue)
        }
      }
      metadataItems <-
        setNames(metadataValues, metadataKeys) # holds all the key value pairs

      metadataLocationsCount <-
        length(tableData$data$metadata$locations$table)
      metadataLocations <-
        tableData$data$metadata$locations$table
      metadata <- list()
      for (j in 1:metadataLocationsCount)
      {
        location <- metadataLocations[[j]]
        metadataItem <- metadataItems[[location]]
        metadataItemLocCount <- length(metadataItem)
        # add the value to metadata list only if the particular location exists in metadataItems list
        if (!is.null(metadataItem))
        {
          vector <- NULL
          for (k in 1:metadataItemLocCount)
          {
            metadataLocationVal <- metadataItems[[location]][k]
            vector <- c(vector, metadataLocationVal)
          }
          convertedList <- as.list(vector)
          metadata <- append(metadata, list(convertedList))
        }
      }
      metadata <- setNames(metadata, metadataKeys)
      return(metadata)
    }
  )
)
