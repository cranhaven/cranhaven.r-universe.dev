#'Add Modify Or Remove Variable Definitions In Configuration
#'
#'These functions help in adding, modifying or removing variable definitions 
#'in a configuration object obtained with \code{\link{ConfigFileOpen}} or 
#'\code{\link{ConfigFileCreate}}. ConfigEditDefinition() will add the 
#'definition if not existing.
#'
#'@param configuration Configuration object obtained wit ConfigFileOpen() or 
#'  ConfigFileCreate().
#'@param name Name of the variable to add/modify/remove.
#'@param value Value to associate to the variable.
#'@param confirm Flag to stipulate whether to ask for confirmation if the 
#'  variable is being modified. Takes by default TRUE.
#'
#'@return A modified configuration object is returned.
#'@seealso [ConfigApplyMatchingEntries()], [ConfigEditDefinition()], 
#'  [ConfigEditEntry()], [ConfigFileOpen()], [ConfigShowSimilarEntries()], 
#'  [ConfigShowTable()].
#'@examples
#'# Create an empty configuration file
#'config_file <- paste0(tempdir(), "/example.conf")
#'ConfigFileCreate(config_file, confirm = FALSE)
#'# Open it into a configuration object
#'configuration <- ConfigFileOpen(config_file)
#'# Add an entry at the bottom of 4th level of file-per-startdate experiments 
#'# table which will associate the experiment "ExampleExperiment2" and variable 
#'# "ExampleVariable" to some information about its location.
#'configuration <- ConfigAddEntry(configuration, "experiments", 
#'                 "last", "ExampleExperiment2", "ExampleVariable", 
#'                 "/path/to/ExampleExperiment2/", 
#'                 "ExampleVariable/ExampleVariable_$START_DATE$.nc")
#'# Edit entry to generalize for any variable. Changing variable needs .
#'configuration <- ConfigEditEntry(configuration, "experiments", 1, 
#'                 var_name = ".*", 
#'                 file_path = "$VAR_NAME$/$VAR_NAME$_$START_DATE$.nc")
#'# Now apply matching entries for variable and experiment name and show the 
#'# result
#'match_info <- ConfigApplyMatchingEntries(configuration, 'tas', 
#'              exp = c('ExampleExperiment2'), show_result = TRUE)
#'
#'@rdname ConfigEditDefinition
#'@export
ConfigEditDefinition <- function(configuration, name, value, confirm = TRUE) {
  continue <- TRUE
  if (name %in% names(configuration$definitions)) {
    if (confirm) {
      while (continue != 'y' && continue != 'n') {
        continue <- readline("WARNING: The definition already exists. It will be replaced. Continue? (y/n)\n")
      }
      continue <- ifelse(continue == 'y', TRUE, FALSE)
    }
  }
  if (continue) {
    configuration$definitions[[name]] <- value
  }
  
  configuration
}
#'@rdname ConfigEditDefinition
#'@export
ConfigRemoveDefinition <- function(configuration, name) {
  configuration$definitions[[name]] <- NULL

  configuration
}
