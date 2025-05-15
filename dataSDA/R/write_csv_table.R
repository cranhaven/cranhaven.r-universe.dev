#' Write Symbolic Data Table
#'
#' @name write_csv_table
#' @aliases write_csv_table
#' @description This function write (save) a symbolic data table from a CSV data file.
#' @usage write_csv_table(data, file, output)
#' @param data The conventional data.
#' @param file The name of the CSV file.
#' @param output This is an experimental argument, with default TRUE, and can be ignored by most users.
#' @returns Write in CSV file the symbolic data table.
#' @importFrom utils write.table
#' @examples
#' data(mushroom)
#' mushroom.set <- set_variable_format(data = mushroom, location = 8, var = "Species")
#' mushroom.tmp <- RSDA_format(data = mushroom.set, sym_type1 = c("I", "S"),
#'                             location = c(25, 31), sym_type2 = c("S", "I", "I"),
#'                             var = c("Species", "Stipe.Length_min", "Stipe.Thickness_min"))
#' mushroom.clean <- clean_colnames(data = mushroom.tmp)
#' # We can save the file in CSV to RSDA format as follows:
#' write_csv_table(data = mushroom.clean, file = "mushroom_interval.csv", output = FALSE)
#' @export

write_csv_table <- function(data, file, output = TRUE){
  if (output == TRUE){
    utils::write.table(data, file, sep = ";", row.names = T, col.names = T)
  }
  if (output == FALSE){
    df <- data
  }
}
