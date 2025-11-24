# Obtain the code and name for the desired levels of a classification.
# Input parameters: input level (level at which the classification is), desired levels, file with the classification as retrieved from KLASS.
#' Title
#'
#' @param input_level Classification level from the original dataset.
#' @param output_level Classification level for which the codes and names are desired by the user.
#' @param klass_data Classification file retrieved from KLASS.
#'
#' @return Data frame with the input and extra desired classification levels. Includes codes and names for each level.
#' @keywords internal
Levels <- function(input_level, output_level, klass_data) {
  # Create a variable for each level of the classification, starting from one variable which contains all levels.
  dt <- subset(klass_data, select = c("code", "parentCode", "level"))
  dt$level <- as.numeric(dt$level)
  klass_codes <- ConvertTable(dt)


  # Add codes and names in the same file (for now, called klass_codes).
  dt <- subset(klass_data, select = c("code", "name", "level"))
  max_level <- max(klass_data$level)
  dt <- subset(klass_data, select = c("code", "name"))
  for (temp_level in 1:max_level) {
    klass_codes <- merge(klass_codes, dt, by.x = c(paste("level", temp_level, sep = "")), by.y = c("code"))
    names(klass_codes)[length(names(klass_codes))] <- paste("name", temp_level, sep = "")
  }


  # Add all the desired levels to the output file.
  output_level[[length(output_level) + 1]] <- input_level # Add input level to list of desired levels, to get all output levels.

  # Check if all the requested levels are valid.
  for (i in output_level) {
    if (i > max(klass_data$level)) {
      stop("You are trying to retrieve a classification level that is more detailed than what is available. Please choose a valid classification level.")
    }
  }

  levelcode1 <- paste("level", output_level, sep = "")
  levelcode2 <- paste("name", output_level, sep = "")
  levelscode <- c(levelcode1, levelcode2)
  output_file <- klass_codes[, levelscode]
  output_file <- unique(output_file)


  return(output_file)
}
