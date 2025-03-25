#' Combine CHN tables
#'
#' This function reads in a table of data on core housing needs by municipality
#' and creates a combined table that separates out each variable in the data into
#' its own column. The resulting table is exported as a CSV file if write_output
#' is TRUE.
#'
#' @param input_path Path to the input CSV file.
#' @param output_path Path to save the output CSV file. If NULL, the file will be
#'                    saved in the same directory as the input file with the name
#'                    "combined_table.csv".
#' @param write_output Logical. Should the combined table be written to a file?
#'                      Default is TRUE.
#'
#' @return A data frame containing the combined data.
#'
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr slice
#' @importFrom utils read.csv write.csv
#'
#' @name prepare_core_housing_needs_tables
#'
#' @export
#'
prepare_core_housing_needs_tables <- function(input_path, output_path = NULL, write_output = TRUE) {

  # Read in table
  masterTable <- read.csv(input_path, header = FALSE)

  # Remove first 8 rows and fourth column
  masterTable <- masterTable[-c(1:8),]
  masterTable <- masterTable[,-4]
  num_cols <- ncol(masterTable)

  for (i in 6:num_cols)
  {
    masterTable[2,i] <- masterTable[1,i]
  }
  colnames(masterTable) <- masterTable[2,]
  masterTable <- masterTable[-c(1,2),]

  # Initialize final dataframe
  cNames <- c("Municipality", "With mortage", "Without Mortgage", "Subsidized housing", "Not subsidized housing",
              "Dwelling provided by the local government, First Nation or Indian band", "Shelter Cost to Income Ratio",
              "Dwelling Condition", "Housing Suitability", "Core Housing Need", "Data_Source", "_lastupdate")

  exportCombined <- data.frame(matrix(nrow = 0, ncol = length(cNames)))
  colnames(exportCombined) <- cNames

  # Fill in category rows
  for (x in 1:nrow(masterTable))
  {
    if(masterTable[x,5] == "")
    {
      #Finds last row of data
      last_row <- x-1
      break
    }
    if(masterTable[x,1] != "")
    {
      oneName <- masterTable[x,1]
    }
    if(masterTable[x,2] != "")
    {
      twoName <- masterTable[x,2]
    }

    if(masterTable[x,3] != "")
    {
      threeName <- masterTable[x,3]
    }

    if (masterTable[x,4] != "")
    {
      fourName <- masterTable[x,4]
    }

    fourName <- gsub("[0-9]", "", fourName)

    masterTable[x,1] <- oneName
    masterTable[x,2] <- twoName
    masterTable[x,3] <- threeName
    masterTable[x,4] <- fourName
  }

  # Trim metadata from bottom of table
  masterTable <- masterTable[1:last_row,]

  rowStart <- 1
  rowEnd <- 1

  # Iterate down shelter column
  for (a in 2:nrow(masterTable))
  {
    if(masterTable[a,1] != masterTable[rowStart,1] | a == nrow(masterTable))
    {
      if(masterTable[a,1] != masterTable[rowStart,1])
      {
        rowEnd <- a-1
      }
      if(a == nrow(masterTable))
      {
        rowEnd <- a
      }

      # Splits table into section
      shelterTable <- masterTable[rowStart:rowEnd,]

      shelterName <- masterTable[rowStart,1]

      rowStart <- a

      shRowStart <- 1

      # Iterate down dwelling column
      for (b in 2:nrow(shelterTable))
      {
        if(shelterTable[b,2] != shelterTable[shRowStart,2] | b == nrow(shelterTable))
        {
          if(shelterTable[b,2] != shelterTable[shRowStart,2])
          {
            shRowEnd <- b-1
          }
          if(b == nrow(shelterTable))
          {
            shRowEnd <- b
          }

          dwellingTable <- shelterTable[shRowStart:shRowEnd,]
          dwellingName <- shelterTable[shRowStart,2]

          shRowStart <- b


          dwRowStart <- 1
          # Iterate down housing column
          for (c in 2:nrow(dwellingTable))
          {
            if(dwellingTable[c,3] != dwellingTable[dwRowStart,3] | c == nrow(dwellingTable))
            {
              if(dwellingTable[c,3] != dwellingTable[dwRowStart,3])
              {
                dwRowEnd <- c-1
              }
              if(c == nrow(dwellingTable))
              {
                dwRowEnd <- c
              }

              housingTable <- dwellingTable[dwRowStart:dwRowEnd,]
              housingName <- dwellingTable[dwRowStart,3]

              dwRowStart <- c


              hoRowStart <- 1

              # Iterate down core table
              for(d in 2:nrow(housingTable))
              {
                if (housingTable[d,4] != housingTable[hoRowStart,4] | d == nrow(housingTable))
                {
                  if (housingTable[d,4] != housingTable[hoRowStart,4])
                  {
                    hoRowEnd <- d-1
                  }
                  if (d == nrow(housingTable))
                  {
                    hoRowEnd <- d
                  }

                  coTable <- housingTable[hoRowStart:hoRowEnd,]
                  coName <- housingTable[hoRowStart,4]

                  coTable <- coTable[, -c(1:4)]

                  # Create table to export
                  exportTable <- t(coTable) # Transpose table
                  exportTable <- as.data.frame(exportTable)

                  # Set column names
                  colnames(exportTable) <- exportTable[1,]
                  exportTable <- exportTable[-1, ]
                  exportTable <- rownames_to_column(exportTable, var = "Municipality")

                  # Add today's date to _lastupdate column
                  exportTable$`_lastupdate` <- as.Date(Sys.time())

                  # Add metadata columns
                  exportTable[, "Shelter Cost to Income Ratio"] <- shelterName
                  exportTable[, "Dwelling Condition"] <- dwellingName
                  exportTable[, "Housing Suitability"] <- housingName
                  exportTable[, "Core Housing Need"] <- coName
                  exportTable[, "Data_Source"] <- "StatsCan"

                  # Clean municipality strings
                  for (e in 1:nrow(exportTable))
                  {
                    muniStr <- exportTable[e,1]

                    # Check if municipality name has two periods
                    has_two_periods <- grepl("\\..*\\.", muniStr)

                    # Remove first period and everything after the second period
                    if (has_two_periods)
                    {
                      muniStr <- gsub("^([^.]+)\\.", "\\1 ", muniStr)
                      muniStr <- gsub("\\..*", "", muniStr)
                    } else
                    {
                      muniStr <- gsub("\\..*", "", muniStr)
                    }

                    # Remove any text after " i"
                    muniStr <- gsub(" i.*", "", muniStr)
                    exportTable[e,1] <- muniStr
                  }

                  # Add today's date to _lastupdate column of exportTable
                  today <- format(Sys.Date(), "%Y-%m-%d")
                  exportTable[,"_lastupdate"] <- today

                  # Format _lastupdate column as yyyy-mm-dd
                  exportTable[,"_lastupdate"] <- format(as.Date(exportTable[,"_lastupdate"]), "%Y-%m-%d")

                  # Append exportTable to exportCombined
                  exportCombined <- rbind(exportCombined, exportTable)

                  # Print message
                  #message(paste("Created table:", coName))

                  # Update hoRowStart to begin next iteration at the correct row
                  hoRowStart <- d
                }
              }
            }
          }
        }
      }
    }
  }

  # Add today's date to _lastupdate column of combined table
  today <- format(Sys.Date(), "%Y-%m-%d")
  exportCombined[,"_lastupdate"] <- today

  # Format _lastupdate column as yyyy-mm-dd
  exportCombined[,"_lastupdate"] <- format(as.Date(exportCombined[,"_lastupdate"]), "%Y-%m-%d")

  # Write to file
  if(write_output){
    if(is.null(output_path)){
      output_path <- paste0(dirname(input_path), "/", "CoreHousingNeeds_CombinedTable.csv")
    }
    write.csv(exportCombined, output_path, row.names = FALSE)

    # Print completion message
    message("Tables have been created, combined, and saved to file.")
    message(paste("Output path:", output_path))
  } else {
    # Print message
    message("Tables have been created and combined, but output was not saved to file.")
  }

  # Return the combined table
  return(invisible(exportCombined))
}
