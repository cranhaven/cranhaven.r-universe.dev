#' Reshape Barcode Abundance Data to Frequency Format
#'
#' Transforms raw barcode abundance data into a tidy long-format data frame, computing summary statistics
#' for each barcode (ID), including maximum, initial, final, and average frequencies across time points.
#'
#' This function expects a data frame with three columns: `ID`, `Time`, and `Reads`.
#' Frequencies are computed by normalizing the `Reads` across all barcodes for each time point.
#'
#' @param input_data A data frame with exactly three columns: `ID` (character or factor),
#'   `Time` (numeric), and `Reads` (numeric). Each row corresponds to a measurement for one barcode at one time point.
#'
#' @return A tidy data frame with columns: `ID`, `max`, `start`, `final`, `mean`, `Time`, and `Frequency`.
#'   Frequencies are normalized across all barcodes per time point. The result is ordered by decreasing `max` frequency.
#'
#' @export
#' @name reshapeData
#' 
#' @examples
#' # Load demo barcode count data (installed with the package)
#' demo_file <- system.file("extdata", "demo_input.csv", package = "doblin")
#' input_dataframe <- readr::read_csv(demo_file, show_col_types = FALSE)
#'
#' # Reshape data to long-format with normalized frequencies +
#' # sort data by descending maximum frequency
#' reshaped_df <- reshapeData(input_dataframe)

reshapeData <- function(input_data) {

  ## Error message if input_data doesn't have the appropriate format
  testing_colnames <- identical(colnames(input_data), c("ID", "Time", "Reads"))
  if (testing_colnames == FALSE){
    stop("# The input data format has not been respected. Make sure your input file contains 3 columns named: ID, Time and Reads.")
  }

  ## Error message if the values of 'Reads' are not numeric
  if (is.numeric(input_data$Reads) == FALSE){
    stop("# The values in 'Reads' column must be numeric.")
  }

  ## Error message if the values of 'Reads' are not numeric
  if (is.numeric(input_data$Time) == FALSE){
    stop("# The values in 'Time' column must be numeric.")
  }

  # "table" uses "input_data"'s IDs as IDs, Time as variables and Reads as values
  table = reshape2::dcast(input_data, ID ~ Time, value.var = 'Reads')

  # converts a 'data.table' into a 'matrix'
  m = as.matrix(table[,-1])

  # converts a 'matrix' into a 'dataframe'
  mat = as.data.frame(sweep(m,2,colSums(m,na.rm = TRUE),`/`)) # we normalize READS to get the frequencies/abundances
  mat$ID = table$ID
  mat[,"mean"] = apply(mat[,-ncol(mat), drop=F],1, mean,na.rm=TRUE)
  mat[,"max"] = apply(mat[,-c(ncol(mat),ncol(mat)-1), drop=F],1, max,na.rm=TRUE)
  mat$start = mat[,1]
  mat$final = mat[,ncol(mat)-4]

  # reshaping mat dataframe into long-format data
  df = reshape2::melt(mat,id.vars = c('ID','max','start','final','mean'))
  names(df)[names(df) == 'variable'] <- 'Time'
  names(df)[names(df) == 'value'] <- 'Frequency'
  df$Time = as.numeric(levels(df$Time))[df$Time]
  df$ID = as.factor(df$ID)

  # Set NAs to 0
  if (any(is.na(df$Frequency))){
    df[is.na(df$Frequency),]$Frequency = 0
  }

  df = df[order(df$max, decreasing = TRUE),]

  return(df)
}

