#'Add, Remove Or Edit Entries In The Configuration
#'
#'ConfigAddEntry(), ConfigEditEntry() and ConfigRemoveEntry() are functions 
#'to manage entries in a configuration object created with ConfigFileOpen().\cr
#'Before adding an entry, make sure the defaults don't do already what you 
#'want (ConfigShowDefinitions(), ConfigShowTable()).\cr
#'Before adding an entry, make sure it doesn't override and spoil what other 
#'entries do (ConfigShowTable(), ConfigFileOpen()).\cr
#'Before adding an entry, make sure there aren't other entries that already 
#'do what you want (ConfigShowSimilarEntries()).
#'
#'@param configuration Configuration object obtained via ConfigFileOpen() 
#'  or ConfigFileCreate() that will be modified accordingly.
#'@param dataset_type Whether to modify a table of experimental datasets or 
#'  a table of observational datasets. Can take values 'experiments' or 
#'  'observations' respectively.
#'@param position 'position' tells the index in the table of the entry to 
#'  edit or remove. Use ConfigShowTable() to see the index of the entry.
#'  In ConfigAddEntry() it can also take the value "last" (default), that will 
#'  put the entry at the end of the corresponding level, or "first" at the 
#'  beginning. See ?ConfigFileOpen for more information.
#'  If 'dataset_name' and 'var_name' are specified this argument is ignored in 
#'  ConfigRemoveEntry().
#'@param dataset_name,var_name,main_path,file_path,nc_var_name,suffix,varmin,varmax
#'  These parameters tell the dataset name, variable name, main path, ..., of 
#'  the entry to add, edit or remove.\cr 'dataset_name' and 'var_name' can take 
#'  as a value a POSIX 1003.2 regular expression (see ?ConfigFileOpen).\cr
#'  Other parameters can take as a value a shell globbing expression 
#'  (see ?ConfigFileOpen).\cr
#'  'dataset_name' and 'var_name' take by default the regular expression '.*' 
#'  (match any dataset and variable name), and the others take by default '*' 
#'  (associate to the pair 'dataset_name' and 'var_name' all the defined 
#'  default values. In this case '*' has a special behaviour, it won't be 
#'  used as a shell globbing expression. See ?ConfigFileOpen and 
#'  ?ConfigShowDefinitions).\cr
#'  'var_min' and 'var_max' must be a character string.\cr
#'  To define these values, you can use defined variables via $VARIABLE_NAME$ 
#'  or other entry attributes via $ATTRIBUTE_NAME$. See ?ConfigFileOpen for 
#'  more information.
#'  
#'@return The function returns an accordingly modified configuration object. 
#'  To apply the changes in the configuration file it must be saved using 
#'  ConfigFileSave().
#'
#'@seealso ConfigApplyMatchingEntries, ConfigEditDefinition, ConfigEditEntry, 
#'  ConfigFileOpen, ConfigShowSimilarEntries, ConfigShowTable
#'@examples
#'# Create an empty configuration file
#'config_file <- paste0(tempdir(), "/example.conf")
#'ConfigFileCreate(config_file, confirm = FALSE)
#'# Open it into a configuration object
#'configuration <- ConfigFileOpen(config_file)
#'# Add an entry at the bottom of 4th level of file-per-startdate experiments 
#'# table which will associate the experiment "ExampleExperiment" and variable 
#'# "ExampleVariable" to some information about its location.
#'configuration <- ConfigAddEntry(configuration, "experiments", 
#'                 "last", "ExampleExperiment", "ExampleVariable", 
#'                 "/path/to/ExampleExperiment/", 
#'                 "ExampleVariable/ExampleVariable_$START_DATE$.nc")
#'# Add another entry
#'configuration <- ConfigAddEntry(configuration, "experiments",
#'                 "last", "ExampleExperiment2", "ExampleVariable", 
#'                 "/path/to/ExampleExperiment2/",
#'                 "ExampleVariable/ExampleVariable_$START_DATE$.nc")
#'# Edit second entry to generalize for any variable. Changing variable needs .
#'configuration <- ConfigEditEntry(configuration, "experiments", 2, 
#'                 var_name = ".*",
#'                 file_path = "$VAR_NAME$/$VAR_NAME$_$START_DATE$.nc")
#'# Remove first entry
#'configuration <- ConfigRemoveEntry(configuration, "experiments",
#'                 "ExampleExperiment", "ExampleVariable")
#'# Show results
#'ConfigShowTable(configuration, "experiments")
#'# Save the configuration
#'ConfigFileSave(configuration, config_file, confirm = FALSE)
#'@rdname ConfigEditEntry
#'@export
ConfigEditEntry <- function(configuration, dataset_type, position, dataset_name = NULL, var_name = NULL, main_path = NULL, file_path = NULL, nc_var_name = NULL, suffix = NULL, varmin = NULL, varmax = NULL) {
  if (!(dataset_type %in% c('experiments', 'observations'))) {
    stop("Error: 'dataset_type' must be one of 'experiments' or 'observations'")
  }

  table_name <- dataset_type
  
  all_entries <- length(unlist(configuration[[table_name]], recursive = FALSE))
  if (position < 1 || position > all_entries) {
    stop("Error: 'position' must be in the range [1, # of table entries]")
  }

  found <- FALSE
  level <- 1
  index_of_first <- 1
  while (!found && level < 5) {
    if (position <= (index_of_first + length(configuration[[table_name]][[level]]) - 1)) {
      found <- TRUE
    } else {
      index_of_first <- index_of_first + length(configuration[[table_name]][[level]])
      level <- level + 1
    }
  }
  position <- position - index_of_first + 1

  edited_values <- c(1:8)[c(!is.null(dataset_name), !is.null(var_name), !is.null(main_path), !is.null(file_path), !is.null(nc_var_name), !is.null(suffix), !is.null(varmin), !is.null(varmax))]
  configuration[[table_name]][[level]][[position]][edited_values] <- c(dataset_name, var_name, main_path, file_path, nc_var_name, suffix, varmin, varmax)

  configuration
}
#'@rdname ConfigEditEntry
#'@export
ConfigAddEntry <- function(configuration, dataset_type, position = 'last', dataset_name = ".*", var_name = ".*", main_path = "*", file_path = "*", nc_var_name = "*", suffix = "*", varmin = "*", varmax = "*") {
  table_name <- dataset_type
  if (dataset_name == ".*") {
    if (var_name == ".*") {
      level <- 1
    } else {
      level <- 3
    }
  } else {
    if (var_name == ".*") {
      level <- 2
    } else {
      level <- 4
    }
  }

  index_of_first <- 0
  index_of_last <- 0
  for (i in 1:level) {
    index_of_first <- index_of_first + ifelse(i == 1, 1, length(configuration[[table_name]][[i - 1]]))
    index_of_last <- index_of_last + length(configuration[[table_name]][[i]])
  }

  if (position == 'last') {
    position <- index_of_last - index_of_first + 1 + 1
  } else if (position == 'first') {
    position <- 1
  } else {
    if (position < index_of_first || position > index_of_last + 1) {
      stop("'position' must be in the range [index of first table entry in corresponding level, index of last table entry in corresponding level + 1]")
    }
    position <- position - index_of_first + 1
  }

  if (dataset_type == 'experiments' || dataset_type == 'observations') {
    configuration[[table_name]][[level]] <- append(configuration[[table_name]][[level]], list(c(dataset_name, var_name, main_path, file_path, nc_var_name, suffix, varmin, varmax)), after = position - 1)
  } else {
    stop("'dataset_type' must be one of 'experiments' or 'observations'")
  }

  configuration
}
#'@rdname ConfigEditEntry
#'@export
ConfigRemoveEntry <- function(configuration, dataset_type, dataset_name = NULL, var_name = NULL, position = NULL) {
  table_name <- dataset_type
  if (!is.null(dataset_name) && !is.null(var_name)) {
    if (dataset_name == ".*") {
      if (var_name == ".*") {
        level <- 1
      } else {
        level <- 3
      }
    } else {
      if (var_name == ".*") {
        level <- 2
      } else {
        level <- 4
      }
    }

    position <- which(unlist(lapply(configuration[[table_name]][[level]], "[", 1)) == dataset_name &
                         unlist(lapply(configuration[[table_name]][[level]], "[", 2)) == var_name)[1]
    if (is.na(position)) {
      stop("No entry found that matches 'dataset_name' and 'var_name'.")
    }
  } else {
    if (is.null(position)) {
      stop("At least ('dataset_name', 'var_name') or 'position' must be specified.")
    }

    all_entries <- length(unlist(configuration[[table_name]], recursive = FALSE))
    if (position < 1 || position > all_entries) {
      stop("'position' must be in the range [1, # of table entries]")
    }

    found <- FALSE
    level <- 1
    index_of_first <- 1
    while (!found && level < 5) {
      if (position <= (index_of_first + length(configuration[[table_name]][[level]]) - 1)) {
        found <- TRUE
      } else {
        index_of_first <- index_of_first + length(configuration[[table_name]][[level]])
        level <- level + 1
      }
    }
    position <- position - index_of_first + 1
  }

  configuration[[table_name]][[level]][[position]] <- NULL

  configuration
}

