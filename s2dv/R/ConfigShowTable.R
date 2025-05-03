#'Show Configuration Tables And Definitions
#'
#'These functions show the tables of supported datasets and definitions in a 
#'configuration object obtained via ConfigFileCreate() or ConfigFileOpen().
#'
#'@param configuration Configuration object obtained from ConfigFileCreate() 
#'  or ConfigFileOpen().
#'@param dataset_type In ConfigShowTable(), 'dataset_type' tells whether the 
#'  table to show is of experimental datasets or of observational datasets.
#'  Can take values 'experiments' or 'observations'.
#'@param line_numbers 'line_numbers' is an optional vector of numbers as long 
#'  as the number of entries in the specified table. Intended for internal use.
#'
#'@seealso [ConfigApplyMatchingEntries()], [ConfigEditDefinition()], 
#'  [ConfigEditEntry()], [ConfigFileOpen()], [ConfigShowSimilarEntries()], 
#'  [ConfigShowTable()].
#'@return These functions return nothing.
#'
#'@examples
#'# Create an empty configuration file
#'config_file <- paste0(tempdir(), "/example.conf")
#'ConfigFileCreate(config_file, confirm = FALSE)
#'# Open it into a configuration object
#'configuration <- ConfigFileOpen(config_file)
#'# Add an entry at the bottom of 4th level of file-per-startdate experiments 
#'# table which will associate the experiment "ExampleExperiment2" and variable 
#'# "ExampleVariable" to some information about its location.
#'configuration <- ConfigAddEntry(configuration, "experiments", "last", 
#'                 "ExampleExperiment2", "ExampleVariable", 
#'                 "/path/to/ExampleExperiment2/", 
#'                 "ExampleVariable/ExampleVariable_$START_DATE$.nc")
#'# Edit entry to generalize for any variable. Changing variable needs .
#'configuration <- ConfigEditEntry(configuration, "experiments", 1, 
#'                 var_name = ".*", 
#'                 file_path = "$VAR_NAME$/$VAR_NAME$_$START_DATE$.nc")
#'# Show tables, lists and definitions
#'ConfigShowTable(configuration, 'experiments')
#'ConfigShowDefinitions(configuration)
#'
#'@rdname ConfigShowTable
#'@export
ConfigShowTable <- function(configuration, dataset_type, line_numbers = NULL) {
  table_name <- dataset_type
  header <- paste("| Matches in", gsub("_", " ", table_name), "|")
  .message(paste(rep("-", nchar(header) - 1), collapse = ''))
  .message(header)
  .message(paste(rep("-", nchar(header) - 1), collapse = ''))
  .message("#dataset_name, var_name[, main_path[, file_path[, nc_var_name[, suffix[, var_min[, var_max]]]]]]")

  if (is.null(line_numbers)) {
    line_numbers <- 1:length(unlist(configuration[[table_name]], recursive = FALSE))
  }
  line_number <- 1

  level <- 1
  invisible(lapply(configuration[[table_name]], 
    function(x) {
      .message(paste("# Level", level, "#"))
      lapply(x, 
        function(y) {
          cat(paste(line_numbers[line_number], ": ", paste(unlist(y), collapse = ', '), "\n", sep = ''))
          line_number <<- line_number + 1
        }
      )
      level <<- level + 1 
    }
  ))
}
#'@rdname ConfigShowTable
#'@export
ConfigShowDefinitions <- function(configuration) {
  defaults <- grep("^DEFAULT_", names(configuration$definitions))
  invisible(lapply(as.vector(paste(names(configuration$definitions)[defaults], paste(unlist(configuration$definitions)[defaults], "\n", sep = ''), sep = " = ")), cat))
  invisible(lapply(as.vector(paste(names(configuration$definitions)[-defaults], paste(unlist(configuration$definitions)[-defaults], "\n", sep = ''), sep = " = ")), cat))
}
