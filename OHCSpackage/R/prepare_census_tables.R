#' Prepare Census Tables
#'
#' This function takes a path to a directory containing CSV files of census data tables, and
#' a path to an output directory. It cleans the data tables and exports them as Excel files
#' to the output directory.
#'
#' @param input_path Path to directory containing input CSV files
#' @param output_path Path to output directory for Excel files
#' @param write_output Logical, indicating whether or not to write the output to file (default: FALSE)
#'
#' @examples
#' temp_dir <- tempdir()
#' file_path <- paste0(temp_dir, "/", "outputs/")
#' prepare_census_tables(system.file("extdata/census_data",
#'  package = "OHCSpackage"), file_path, write_output = TRUE)
#'
#' @return If write_to_file is TRUE, a list of file paths for the Excel filesw will be produced.
#'
#' @importFrom readr read_csv
#' @importFrom openxlsx write.xlsx
#' @importFrom tibble rownames_to_column
#' @importFrom utils read.csv write.csv
#' @export
prepare_census_tables <- function(input_path, output_path, write_output = FALSE) {
  # Set Topic to NULL
  Topic <- NULL
  # Check if output folder exists or can be created
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
    message("Output folder created at: ", output_path)
  } else {
    message("Output folder already exists at: ", output_path)
  }

  # List all CSV files in the folder
  input_files <- list.files(input_path, pattern = ".csv$", full.names = TRUE)

  # Convert input files to data frames
  input_table_list <- lapply(input_files, read.csv, header = FALSE)
  # Check if all tables have the same amount of rows
  check_row <- nrow(input_table_list[[1]])
  run_script <- TRUE

  for (table in input_table_list) {
    rows <- nrow(table)

    if (rows != check_row) {
      run_script <- FALSE
      break
    }
  }

  # columns that contain no/ unneeded variables
  flag_indexes <- c(3, 5, 7, 9, 11, 13, 14)

  count <- 1

  # Clean Tables
  clean_tables_list <- lapply(input_table_list, function(input_table) {

    # Remove unneeded rows
    input_table_cleaned <- input_table[-c(1,3),]

    # Remove total_flag columns
    input_table_cleaned <- input_table_cleaned[,-flag_indexes]

    # Move down muni's, remove row 1, row 1 to colnames
    num_cols <- ncol(input_table_cleaned)

    for (i in 3:num_cols) {
      input_table_cleaned[2,i] <- input_table_cleaned[1,i]
      muniStr <- strsplit(input_table_cleaned[2,i],",")
      suppressWarnings(input_table_cleaned[2,i] <- muniStr[1])
    }

    colnames(input_table_cleaned) <- input_table_cleaned[2,]
    input_table_cleaned <- input_table_cleaned[-c(1,2),]

    num_row <- nrow(input_table_cleaned)

    rInd <- 1

    # Remove bottom metadata
    while (input_table_cleaned[rInd,2] != "") {
      rInd <- rInd + 1

      if (rInd > num_row) {
        break
      }
    }
    input_table_cleaned <- input_table_cleaned[-c(rInd:num_row),]

    # Combine tables
    if(count == 1) {
      censusTable <- cbind(input_table_cleaned[,c(1,2)])
    }
    censusTable <- cbind(censusTable,input_table_cleaned[,c(3:num_cols)])

    count <- count + 1

    return(list(input_table_cleaned, censusTable))
  })

  # Extract cleaned tables and censusTable from list
  cleaned_tables <- lapply(clean_tables_list, function(x) x[[1]])
  censusTable <- do.call(cbind, lapply(clean_tables_list, function(x) x[[2]]))

  # Split tables by Topic
  censusTableList <- split(censusTable, f = censusTable$Topic)

  # Write cleaned tables to Excel files
  table_num <- length(censusTableList)
  num <- 1

  for (i in censusTableList) {
    elementStr <- names(censusTableList)[num]
    elementStr <- gsub(" ", "_", elementStr)
    elementStr <- gsub("[()]", "_", elementStr)
    elementStr <- gsub("-", "_", elementStr)
    listItem <- censusTableList[[num]]
    filePath <- paste(output_path, elementStr, ".xlsx", sep = "")
    listItem <- subset(listItem, select = -Topic)
    trItem <- t(listItem)
    trItem <- as.data.frame(trItem)
    colnames(trItem) <- trItem[1,]
    trItem <- trItem[-c(1,7,8),]
    newItem <- trItem
    newItem <- cbind(Municipality = rownames(trItem), trItem)
    trItem <- newItem
    today <- Sys.Date()
    trItem[, "data_source"] <- "StatsCan"
    trItem[, "_lastupdate"] <- today
    for (j in 1:nrow(trItem)) {
      trItem[j,1] <- gsub(".", " ", trItem[j,1], fixed = TRUE)
    }
    tryCatch({
      write.xlsx(trItem, filePath, rowNames = FALSE)
      message(paste("Created File", num, "of", table_num, ":", elementStr))
    }, error = function(e) {
      warning("Error in creating file '", elementStr, "': ", e$message)
    })
        num <- 1 + num
  }
}


