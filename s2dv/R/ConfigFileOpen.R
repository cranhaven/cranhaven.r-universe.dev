#'Functions To Create Open And Save Configuration File
#'
#'These functions help in creating, opening and saving configuration files.
#'
#'@param file_path Path to the configuration file to create/open/save. 
#'@param silent Flag to activate or deactivate verbose mode.
#'  Defaults to FALSE (verbose mode on).
#'@param configuration Configuration object to save in a file.
#'@param confirm Flag to stipulate whether to ask for confirmation when 
#'  saving a configuration file that already exists.\cr
#'  Defaults to TRUE (confirmation asked).
#'@param stop TRUE/FALSE whether to raise an error if not all the mandatory 
#'  default variables are defined in the configuration file.
#'
#'@details
#'ConfigFileOpen() loads all the data contained in the configuration file 
#'specified as parameter 'file_path'.
#'Returns a configuration object with the variables needed for the 
#'configuration file mechanism to work.
#'This function is called from inside the Load() function to load the 
#'configuration file specified in 'configfile'.\cr\cr
#'ConfigFileCreate() creates an empty configuration file and saves it to 
#'the specified path. It may be opened later with ConfigFileOpen() to be edited.
#' Some default values are set when creating a file with this function, you 
#'can check these with ConfigShowDefinitions().\cr\cr
#'ConfigFileSave() saves a configuration object into a file, which may then 
#'be used from Load().\cr\cr
#'Two examples of configuration files can be found inside the 'inst/config/' 
#'folder in the package:
#'  \itemize{
#'    \item{BSC.conf: configuration file used at BSC-CNS. Contains location 
#'    data on several datasets and variables.}
#'    \item{template.conf: very simple configuration file intended to be used as 
#'    pattern when starting from scratch.}
#'  }
#'How the configuration file works:\cr
#'~~~~~~~~~~~~~~~~~~~~~~~~~~~~\cr
#'It contains one list and two tables.\cr
#'Each of these have a header that starts with '!!'. These are key lines and 
#'should not be removed or reordered.\cr
#'Lines starting with '#' and blank lines will be ignored.
#'The list should contains variable definitions and default value definitions.\cr
#'The first table contains information about experiments.\cr
#'The third table contains information about observations.\cr
#'Each table entry is a list of comma-separated elements.\cr
#'The two first are part of a key that is associated to a value formed by the 
#'other elements.\cr
#'The key elements are a dataset identifier and a variable name.\cr
#'The value elements are the dataset main path, dataset file path, the 
#'variable name inside the .nc file, a default suffix (explained below) and a 
#'minimum and maximum vaues beyond which loaded data is deactivated.\cr
#'Given a dataset name and a variable name, a full path is obtained 
#'concatenating the main path and the file path.\cr
#'Also the nc variable name, the suffixes and the limit values are obtained.\cr
#'Any of the elements in the keys can contain regular expressions[1] that will 
#'cause matching for sets of dataset names or variable names.\cr
#'The dataset path and file path can contain shell globbing expressions[2] 
#'that will cause matching for sets of paths when fetching the file in the 
#'full path.\cr
#'The full path can point to an OPeNDAP URL.\cr
#'Any of the elements in the value can contain variables that will be replaced 
#'to an associated string.\cr
#'Variables can be defined only in the list at the top of the file. \cr
#'The pattern of a variable definition is\cr
#'VARIABLE_NAME = VARIABLE_VALUE\cr
#'and can be accessed from within the table values or from within the variable 
#'values as\cr
#'  $VARIABLE_NAME$\cr
#'For example:\cr
#'  FILE_NAME = tos.nc\cr
#'  !!table of experiments\cr
#'  ecmwf, tos, /path/to/dataset/, $FILE_NAME$\cr
#'There are some reserved variables that will offer information about the 
#'store frequency, the current startdate Load() is fetching, etc:\cr
#'  $VAR_NAME$, $START_DATE$, $STORE_FREQ$, $MEMBER_NUMBER$\cr
#'  for experiments only: $EXP_NAME$\cr
#'  for observations only: $OBS_NAME$, $YEAR$, $MONTH$, $DAY$\cr
#'Additionally, from an element in an entry value you can access the other 
#'elements of the entry as:\cr
#'  $EXP_MAIN_PATH$, $EXP_FILE_PATH$, \cr$VAR_NAME$, $SUFFIX$, $VAR_MIN$, $VAR_MAX$\cr
#'\cr
#'The variable $SUFFIX$ is useful because it can be used to take part in the 
#'main or file path. For example: '/path/to$SUFFIX$/dataset/'.\cr
#'It will be replaced by the value in the column that corresponds to the 
#'suffix unless the user specifies a different suffix via the parameter 
#''suffixexp' or 'suffixobs'.\cr
#'This way the user is able to load two variables with the same name in the 
#'same dataset but with slight modifications, with a suffix anywhere in the 
#'path to the data that advices of this slight modification.\cr\cr
#'The entries in a table will be grouped in 4 levels of specificity:
#'  \enumerate{
#'    \item{
#'General entries:\cr
#' - the key dataset name and variable name are both a regular expression 
#'matching any sequence of characters (.*) that will cause matching for any 
#'pair of dataset and variable names\cr
#'   Example:  .*, .*, /dataset/main/path/, file/path, nc_var_name, suffix, 
#'var_min, var_max
#'    }
#'    \item{
#'Dataset entries:\cr
#' - the key variable name matches any sequence of characters\cr
#'   Example:  ecmwf, .*, /dataset/main/path/, file/path, nc_var_name, 
#'  suffix, var_min, var_max
#'    }
#'    \item{
#'Variable entries:\cr
#' - the key dataset name matches any sequence of characters\cr
#'   Example:  .*, tos, /dataset/main/path/, file/path, nc_var_name, 
#'  suffix, var_min, var_max
#'    }
#'    \item{
#'  Specific entries:\cr
#' - both key values are specified\cr
#'   Example:  ecmwf, tos, /dataset/main/path/, file/path, nc_var_name, 
#'  suffix, var_min, var_max
#'    }
#'  }
#'Given a pair of dataset name and variable name for which we want to know the 
#'full path, all the rules that match will be applied from more general to 
#'more specific.\cr
#'If there is more than one entry per group that match a given key pair, 
#'these will be applied in the order of appearance in the configuration file 
#'(top to bottom).\cr\cr
#'An asterisk (*) in any value element will be interpreted as 'leave it as is 
#'or take the default value if yet not defined'.\cr
#'The default values are defined in the following reserved variables:\cr
#'  $DEFAULT_EXP_MAIN_PATH$, $DEFAULT_EXP_FILE_PATH$, $DEFAULT_NC_VAR_NAME$, 
#'$DEFAULT_OBS_MAIN_PATH$, $DEFAULT_OBS_FILE_PATH$, $DEFAULT_SUFFIX$, 
#'$DEFAULT_VAR_MIN$, $DEFAULT_VAR_MAX$, \cr
#'$DEFAULT_DIM_NAME_LATITUDES$, $DEFAULT_DIM_NAME_LONGITUDES$, \cr
#'$DEFAULT_DIM_NAME_MEMBERS$\cr\cr
#'Trailing asterisks in an entry are not mandatory. For example\cr
#'  ecmwf, .*, /dataset/main/path/, *, *, *, *, *\cr
#'will have the same effect as\cr
#'  ecmwf, .*, /dataset/main/path/ \cr\cr
#'A double quote only (") in any key or value element will be interpreted as 
#''fill in with the same value as the entry above'.
#'
#'@return 
#'ConfigFileOpen() returns a configuration object with all the information for 
#'  the configuration file mechanism to work.\cr
#'ConfigFileSave() returns TRUE if the file has been saved and FALSE otherwise.\cr
#'ConfigFileCreate() returns nothing.
#'
#'@seealso ConfigApplyMatchingEntries, ConfigEditDefinition, 
#'  ConfigEditEntry, ConfigFileOpen, ConfigShowSimilarEntries, ConfigShowTable
#'@references
#'[1] \url{https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html}\cr
#'[2] \url{https://tldp.org/LDP/abs/html/globbingref.html}
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
#'# Finally save the configuration file.
#'ConfigFileSave(configuration, config_file, confirm = FALSE)
#'
#'@rdname ConfigFileOpen
#'@export
ConfigFileOpen <- function(file_path, silent = FALSE, stop = FALSE) {
  if (!silent) {
    .message(paste("Reading configuration file:", file_path))
  }
  # Read the data from the configuration file.
  ## Remove comments, tabulations, spaces, empty lines, ...
  all_lines <- readLines(file_path)
  all_lines <- gsub("\t", "", all_lines)
  all_lines <- gsub(" ", "", all_lines)
  all_lines <- all_lines[-grep("^#", all_lines)]
  all_lines <- all_lines[-grep("^$", all_lines)]
  ## Detect key lines
  key_positions <- grep("^!!", all_lines)

  ## Check that the format of the configuration file is right.
  if (length(key_positions) != 3) {
    stop('Error: The configuration file is corrupted or outdated: the key lines do not match the expected pattern.')
  }

  ## Start parsing the configuration.
  # The variables that are used in the configuration filed are kept in 
  # 'definitions', an associative array (key-value array or dictionary).
  definitions <- list()
  ## Parse the variables definitions in the whole configuration file
  if (key_positions[1] + 1 < key_positions[2]) {
    all_definitions <- all_lines[(key_positions[1] + 1):(key_positions[2] - 1)]
  } else {
    all_definitions <- c()
  }
  if (length(grep("=", all_definitions)) == length(all_definitions)) {
    for (definition in all_definitions) {
      if (length(which(strsplit(definition, "")[[1]] == "=")) == 1) {
        var_name <- strsplit(definition, "=")[[1]][1]
        tmp_value <- strsplit(definition, "=")[[1]][2]
        var_value <- ifelse(is.na(tmp_value), "", tmp_value)
        if ((length(which(strsplit(var_value, "")[[1]] == "$")) %% 2) == 0) {
          definitions[[var_name]] <- var_value
        } else {
          stop('Error: The configuration file is corrupted: there are incorrect variable definition lines in the definition zone. A closing "$" symbol may be missing.')
        }
      } else {
        stop('Error: The configuration file is corrupted: there are incorrect definition lines in the definition zone.')
      }
    }
  } else {
    stop('Error: The configuration file is corrupted: there are malformed definition lines in the definition zone.')
  }
  mandatory_definitions <- c("DEFAULT_EXP_MAIN_PATH", "DEFAULT_EXP_FILE_PATH", 
                             "DEFAULT_NC_VAR_NAME", "DEFAULT_SUFFIX", "DEFAULT_VAR_MIN", 
                             "DEFAULT_VAR_MAX", "DEFAULT_OBS_MAIN_PATH", 
                             "DEFAULT_OBS_FILE_PATH", "DEFAULT_DIM_NAME_LONGITUDES",
                             "DEFAULT_DIM_NAME_LATITUDES", "DEFAULT_DIM_NAME_MEMBERS")
  if (any(!(mandatory_definitions %in% names(definitions)))) {
    .warning("Some of the mandatory variables below are not defined in the configuration file. You can add them with ConfigFileOpen(), ConfigEditDefinition() and ConfigFileSave() or by editing the configuration file by hand, as specified in ?ConfigFileOpen.")
    if (stop) {
      stop(paste(mandatory_definitions, collapse = ', '))
    } else {
      .warning(paste(mandatory_definitions, collapse = ', '))
    }
  }

  # Parse the entries in the tables
  ## These are the indices of the key positions in the vector of key positions
  tables_key_positions <- c(2, 3)
  current_table <- 1
  for (table_key_position in tables_key_positions) {
    datasets <- list(c(), c(), c(), c())

    if (table_key_position == 2) {
      id <- 'EXP'
    } else {
      id <- 'OBS'
    }
    default_values <- c(paste0("$DEFAULT_", id, "_MAIN_PATH$"), paste0("$DEFAULT_", id, "_FILE_PATH$"), "$DEFAULT_NC_VAR_NAME$", '$DEFAULT_SUFFIX$', '$DEFAULT_VAR_MIN$', '$DEFAULT_VAR_MAX$')
    previous_values <- c(".*", ".*", default_values)
    table_lines <- c()
    table_end <- ifelse(table_key_position == max(tables_key_positions), length(all_lines), key_positions[table_key_position + 1] - 1)
    if ((key_positions[table_key_position] + 1) <= table_end) {
      table_lines <- all_lines[(key_positions[table_key_position] + 1):table_end]
      table_lines <- strsplit(table_lines, ",")
    }

    current_line <- 1
    for (entry in table_lines) {
      if (entry[1] == '"') {
        entry[1] <- previous_values[1]
      }
      if ((length(entry) > 1)) {
        if (entry[2] == '"') {
          entry[2] <- previous_values[2]
        }
      } else {
        stop('Error: The variable column must be defined in all the entries in the tables in the configuration file.')
      }
      if (length(entry) > length(default_values) + 2) {
        stop(paste0("Error: More elements than expected in the entry ", current_line, " in the configuration file."))
      }
      for (value_position in 1:length(default_values)) {
        if ((length(entry) > value_position + 1)) {
          if (entry[value_position + 2] == '"') {
            entry[value_position + 2] <- previous_values[value_position + 2]
          }
        } else {
          entry[value_position + 2] <- '*'
        }
      }
      if (entry[1] == '.*') {
        if (entry[2] == '.*') {
          datasets[[1]] <- c(datasets[[1]], list(entry))
        } else {
          datasets[[3]] <- c(datasets[[3]], list(entry))
        }
      } else {
        if (entry[2] == '.*') {
          datasets[[2]] <- c(datasets[[2]], list(entry))
        } else {
          datasets[[4]] <- c(datasets[[4]], list(entry))
        }
      }
      current_line <- current_line + 1
      previous_values <- entry
    }

    if (current_table == 1) {
      exps <- datasets
    } else if (current_table == 2) {
      obs <- datasets
    }

    current_table <- current_table + 1
  }
  
  if (!silent) {
    .message("Config file read successfully.")
  }

  invisible(list(definitions = definitions, 
                 experiments = exps, 
                 observations = obs))
}

#'@rdname ConfigFileOpen
#'@export
ConfigFileCreate <- function(file_path, confirm = TRUE) {
  success <- ConfigFileSave(list(definitions = list(
    DEFAULT_EXP_MAIN_PATH = "$EXP_NAME$",
    DEFAULT_EXP_FILE_PATH = "$STORE_FREQ$/$VAR_NAME$_$START_DATE$.nc",
    DEFAULT_NC_VAR_NAME = "$VAR_NAME$",
    DEFAULT_SUFFIX = "", DEFAULT_VAR_MIN = "",
    DEFAULT_VAR_MAX = "", DEFAULT_OBS_MAIN_PATH = "$OBS_NAME$",
    DEFAULT_OBS_FILE_PATH = "$STORE_FREQ$/$VAR_NAME$_$YEAR$$MONTH$.nc",
    DEFAULT_DIM_NAME_LONGITUDES = "longitude", DEFAULT_DIM_NAME_LATITUDES = "latitude",
    DEFAULT_DIM_NAME_MEMBERS = "ensemble")), file_path, confirm = confirm)
  if (success) {
    .warning("You have just created an empty configuration file. You can edit it with ConfigAddEntry(). You can edit the defaults according to your needs with the functions ConfigFileOpen(), ConfigEditDefinition() and ConfigFileSave() or edit the file manually as specified in ?ConfigFileOpen.")
  }
}

#'@rdname ConfigFileOpen
#'@export
ConfigFileSave <- function(configuration, file_path, confirm = TRUE) {
  continue <- TRUE
  if (file.exists(file_path)) {
    if (confirm) {
      while (continue != 'y' && continue != 'n') {
        continue <- readline(paste0("WARNING: The configuration file '", file_path, "' already exists. It will be replaced. Continue? (y/n)\n"))
      }
      continue <- ifelse(continue == 'y', TRUE, FALSE)
    }
  }
  if (continue) {
    file_conn <- file(file_path)
    file_text <- c(
"# s2dv configuration file",
"#",
"# Check ?ConfigFileOpen after loading s2dv for detailed ",
"# documentation on this configuration file.",
""
                  )

    file_text <- c(file_text,
      paste(rep("#", nchar("definitions") + 2), collapse = ''),
      paste0("!!definitions"),
      paste(rep("#", nchar("definitions") + 2), collapse = '')
                  )
    defaults <- configuration$definitions[grep("^DEFAULT_", names(configuration$definitions))]
    definitions <- configuration$definitions[-grep("^DEFAULT_", names(configuration$definitions))]
    file_text <- c(file_text, as.vector(paste(names(defaults), unlist(defaults), sep = " = ")))
    file_text <- c(file_text, as.vector(paste(names(definitions), unlist(definitions), sep = " = ")))
    file_text <- c(file_text, "")

    table_names <- c("experiments", "observations")
    for (table_name in table_names) {
      if (table_name == "experiments") {
        dataset_type <- 'exp'
      } else {
        dataset_type <- 'obs'
      }
      file_text <- c(file_text,
"",
  paste(rep("#", nchar(table_name) + 11), collapse = ''),
  paste0("!!table of ", gsub("_", " ", table_name)),
  paste(rep("#", nchar(table_name) + 11), collapse = ''),
  paste0("#", dataset_type, "_name, var_name[, ", dataset_type, "_main_path[, ", dataset_type, "_file_path[, nc_var_name[, suffix[, var_min[, var_max]]]]]]")
                  )
      # Some heavy entry processing still to do here, to put asterisks, empty spaces, double quotes, and reduce options
      file_text <- c(file_text, unlist(lapply(configuration[[table_name]], function (x) lapply(x, function (y) paste(unlist(y), collapse = ", ")))))
    }

    writeLines(file_text, file_conn)
    close(file_conn)
  }

  invisible(continue)
}
