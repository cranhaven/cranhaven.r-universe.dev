#' Separate Real-Time Data into separate dataframes.
#'
#' If multiple real-time reads were exported from MARS, separate_raw will parse
#' them out and separate them. It will also export to an Excel file with each
#' real-time data having its own sheet.
#'
#' @param file An Excel file exported from MARS.
#' @param num_rows Number of rows in the header to ignore.
#' @param export_name The name of the original file or an orignal name.
#'
#' @return An Excel file with separated raw real-time data.
#'
#' @export
# Separates the raw run files in the .xlsx file.
separate_raw <- function(file, num_rows, export_name) {
  if (is.character(file)) { # Read the Excel file into R.
    data <- read_excel(file, sheet = 2)
  } else if (is.data.frame(file)) {
    data <- file
  } else {
    stop("Please enter either .xlsx string or dataframe. ")
  }

  # Remove metadata.
  tidy_data <- data[-(1:num_rows - 1), -1] |>
    na.omit(tidy_data)


  # Set the first row as column names.
  col_names <- tidy_data[1, ]
  tidy_data <- tidy_data[-1, ]
  colnames(tidy_data) <- col_names

  # Add leading "0" before single digits in column names.
  colnames(tidy_data) <- gsub(" X(\\d)$", " X0\\1", colnames(tidy_data))

  # Identify and handle duplicate column names.
  dup_cols <- colnames(tidy_data)[duplicated(colnames(tidy_data))]
  if (length(dup_cols) > 0) {
    # Add suffix to duplicate column names
    for (col in dup_cols) {
      indices <- which(colnames(tidy_data) == col)
      colnames(tidy_data)[indices] <- paste0(col, "_", indices)
    }
  }

  # Rename the first column as "Time"
  tidy_data <- tidy_data |>
    rename("Time" = 1)

  # Convert tidy_data to numeric.
  tidy_data <- as.data.frame(sapply(tidy_data, as.numeric))

  # Rearrange columns to group replicates of the same sample
  tidy_data <- tidy_data |>
    select("Time", order(colnames(tidy_data), decreasing = FALSE))

  # Remove suffixes from column names
  colnames(tidy_data) <- gsub("_\\d+$", "", colnames(tidy_data))

  # Designate the integers used to calculate how the data will be cut
  cycles <- length(unique(tidy_data[["Time"]])) # Number of cycles
  num_rows <- cycles # This will change after sending
  # one data type to a data frame
  reads <- length(which(tidy_data[["Time"]] == 0)) # Number of types of data (e.g. Raw,
  # Normalized, or Derivative)

  # Create a data frame with only the "Time" column with no duplicates
  time_df <- data.frame(unique(tidy_data[["Time"]])) |>
    rename("Time" = 1)

  # Create separate data frames for different read types
  i <- 1
  while (i <= reads) {
    if (num_rows == cycles) {
      df <- cbind(time_df, tidy_data[(num_rows - cycles):num_rows, -1])
      assign(paste0("df", i), df)
      num_rows <- num_rows + cycles
    } else {
      df <- cbind(time_df, tidy_data[(1 + num_rows - cycles):num_rows, -1])
      assign(paste0("df", i), df)
      num_rows <- num_rows + cycles
    }
    i <- i + 1
  }

  # Export the organized data
  existing_file <- openxlsx::loadWorkbook(export_name)

  # Function to write a data frame to a new sheet in the workbook
  write_to_sheet <- function(df, sheet_name, existing_file) {
    openxlsx::addWorksheet(existing_file, sheetName = sheet_name)
    openxlsx::writeData(existing_file,
      sheet = sheet_name, df, startRow = 1,
      startCol = 1, rowNames = FALSE
    )
  }

  # Write each data frame to a new sheet in the workbook
  i <- 1
  while (i <= reads) {
    df_name <- paste0("df", i)
    df <- get(df_name)
    sheet_name <- paste0("Data", i)
    write_to_sheet(df, sheet_name, existing_file)
    i <- i + 1
  }
  # rm(df, df_name, i, reads, sheet_name)

  # Save the modified file
  openxlsx::saveWorkbook(existing_file, file, overwrite = TRUE)

  # Open the file for the user to view
  if (Sys.info()["sysname"] == "Windows") {
    shell.exec(file)
  } else if (Sys.info()["sysname"] == "Darwin") {
    system(paste("open", file))
  }
}
