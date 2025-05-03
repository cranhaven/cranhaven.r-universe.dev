#'Find Similar Entries In Tables Of Datasets
#'
#'These functions help in finding similar entries in tables of supported 
#'datasets by comparing all entries with some given information.\cr
#'This is useful when dealing with complex configuration files and not sure 
#'if already support certain variables or datasets.\cr
#'At least one field must be provided in ConfigShowSimilarEntries(). 
#'Other fields can be unspecified and won't be taken into account. If more 
#'than one field is provided, sameness is avreaged over all provided fields 
#'and entries are sorted from higher average to lower.
#'
#'@param configuration Configuration object obtained either from 
#'  ConfigFileCreate() or ConfigFileOpen().
#'@param dataset_name Optional dataset name to look for similars of.
#'@param var_name Optional variable name to look for similars of.
#'@param main_path Optional main path to look for similars of.
#'@param file_path Optional file path to look for similars of.
#'@param nc_var_name Optional variable name inside NetCDF file to look for similars of.
#'@param suffix Optional suffix to look for similars of.
#'@param varmin Optional variable minimum to look for similars of.
#'@param varmax Optional variable maximum to look for similars of.
#'@param n_results Top 'n_results' alike results will be shown only. Defaults 
#'  to 10 in ConfigShowSimilarEntries() and to 5 in ConfigShowSimilarVars().
#'
#'@details
#'Sameness is calculated with string distances as specified by Simon White 
#'in [1].
#'
#'@return These functions return information about the found matches.
#'
#'@seealso ConfigApplyMatchingEntries, ConfigEditDefinition, 
#'  ConfigEditEntry, ConfigFileOpen, ConfigShowSimilarEntries, ConfigShowTable
#'@references
#'[1] Simon White, string seamness: 
#'  \url{http://www.catalysoft.com/articles/StrikeAMatch.html}
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
#'                 var_name = "Var.*", 
#'                 file_path = "$VAR_NAME$/$VAR_NAME$_$START_DATE$.nc")
#'# Look for similar entries
#'ConfigShowSimilarEntries(configuration, dataset_name = "Exper", 
#'                         var_name = "Vari")
#'
#'@export
ConfigShowSimilarEntries <- function(configuration, dataset_name = NULL, var_name = NULL, main_path = NULL, file_path = NULL, nc_var_name = NULL, suffix = NULL, varmin = NULL, varmax = NULL, n_results = 10) {
  ## Simon White: http://www.catalysoft.com/articles/StrikeAMatch.html
  getBigrams <- function(str) {
    bigramlst <- list()
    for (i in 1:(nchar(str) - 1)) {
      bigramlst[[i]] <- substr(str, i, i + 1)
    }
    return(bigramlst)
  }

  strSimilarity <- function(str1, str2) {
    str1 <- tolower(str1)
    str2 <- tolower(str2)    

    if (is.null(str1)) {
      str1 <- ""
    } else if (is.na(str1)) {
      str1 <- ""
    } 
    if (is.null(str2)) {
      str2 <- ""
    } else if (is.na(str2)) {
      str2 <- ""
    }
    if (nchar(str1) <= 1 && nchar(str2) <= 1) {
      return (ifelse(str1 == str2, 1, 0))
    } else if (nchar(str1) == 1) {
      return (ifelse(grepl(str1, str2, fixed = TRUE), 1, 0))
    } else if (nchar(str2) == 1) {
      return (ifelse(grepl(str2, str1, fixed = TRUE), 1, 0))
    } else if (nchar(str1) == 0 || nchar(str2) == 0) {
      return (0)
    } else {
      pairs1 <- getBigrams(str1)
      pairs2 <- getBigrams(str2)
      unionlen <- length(pairs1) + length(pairs2)
      hit_count <- 0
      for (x in 1:length(pairs1)) {
        for(y in 1:length(pairs2)) {
          if (pairs1[[x]] == pairs2[[y]]) {
            hit_count <- hit_count + 1
          }
        }
      }
      return ((2.0 * hit_count) / unionlen)
    }
  }
  
  strSimilarityVec <- Vectorize(strSimilarity, c('str1', 'str2'), USE.NAMES = FALSE)

  all_tables <- c('experiments', 'observations')
  all_fields <- c('dataset_name', 'var_name', 'main_path', 'file_path', 'nc_var_name', 'suffix', 'varmin', 'varmax')
  selected_fields <- which(unlist(lapply(as.list(match.call())[all_fields], function (x) !is.null(x))))
  values <- unlist(as.list(match.call())[all_fields[selected_fields]], use.names = FALSE)

  if (length(selected_fields) < 1) {
    stop("There must be at least one selected field ('dataset_name', 'var_name', 'main_path', 'file_path', 'nc_var_name', 'suffix', 'varmin' or 'varmax').")
  }

  similarities <- list()
  for (table in all_tables) {
    similarities[[table]] <- vector("list", 4)
    for (level in 1:4) {
      if (length(configuration[[table]][[level]]) > 0) {
        similarities[[table]][[level]] <- unlist(lapply(configuration[[table]][[level]], function(x) mean(strSimilarityVec(x[selected_fields], values))))
      }
    }
  }
 
  n_results <- min(n_results, length(unlist(similarities)))
  threshold <- sort(unlist(similarities, use.names = FALSE), decreasing = TRUE)[n_results]
  n_minimums <- sum(sort(unlist(similarities, use.names = FALSE), decreasing = TRUE)[1:n_results] == threshold)
  
  matches <- list()
  n_picked_minimums <- 0
  for (table in all_tables) {
    matches[[table]] <- list()
    line_numbers <- c()
    offset <- 0
    for (level in 1:4) {
      matches_to_pick <- which(similarities[[table]][[level]] > threshold)
      if (n_picked_minimums < n_minimums) {
        minimums <- which(similarities[[table]][[level]] == threshold)
        if (length(minimums) + n_picked_minimums > n_minimums) {
          minimums <- minimums[1:(n_minimums - n_picked_minimums)]
        }
        matches_to_pick <- c(matches_to_pick, minimums)
        n_picked_minimums <- n_picked_minimums + length(minimums)
      }
      line_numbers <- c(line_numbers, matches_to_pick + offset)
      offset <- offset + length(similarities[[table]][[level]])
      matches[[table]][[level]] <- configuration[[table]][[level]][matches_to_pick]
    }
    dataset_type <- ifelse(grepl('experiments', table), 'experiments', 'observations')
    ConfigShowTable(matches, dataset_type, line_numbers)
  }
  
  invisible(matches)
}
