#' Clean and standardize column names in a data.frame
#'
#' @description
#' This function standardizes column names in a data.frame to make them syntactically
#' valid and consistent. It converts uppercase to lowercase, removes spaces and special
#' characters, replaces accents, and ensures names are unique and valid for R. It is an
#' alternative to the \code{janitor::clean_names()} function implemented in base R.
#'
#' @param df A data.frame or tibble whose column names you want to clean.
#' @param case The case format for the resulting names. Options:
#'   \code{"snake"} (default): names_with_underscores
#'   \code{"lower_camel"}: namesWithCamelCase
#'   \code{"upper_camel"}: NamesWithCamelCase
#'   \code{"screaming_snake"}: NAMES_WITH_UNDERSCORES
#' @param replace_special_chars Logical. If \code{TRUE} (default), replaces accented
#'   and special characters with their ASCII equivalents (e.g., "a with accent" becomes "a").
#' @param unique_names Logical. If \code{TRUE} (default), ensures the resulting names
#'   are unique by adding numeric suffixes to duplicates.
#'
#' @return A data.frame with the same data as the input but with clean and standardized
#'   column names according to the specified parameters.
#'
#' @details
#' The cleaning process includes:
#' \itemize{
#'   \item Converting everything to lowercase (except in camel or screaming_snake formats)
#'   \item Replacing accents and common special characters with their ASCII equivalents
#'   \item Removing parentheses and their content
#'   \item Replacing non-alphanumeric characters with underscores
#'   \item Removing redundant underscores (leading, trailing, or duplicated)
#'   \item Ensuring names don't start with numbers (adding "x" at the beginning)
#'   \item Applying the selected case format
#'   \item Ensuring names are unique by adding numeric suffixes
#' }
#'
#'
#' @keywords internal
clean_names_rdbr <- function(df,
                             case = c("snake", "lower_camel", "upper_camel", "screaming_snake"),
                             replace_special_chars = TRUE,
                             unique_names = TRUE) {

  # Validate arguments
  case <- match.arg(case)
  if (!is.data.frame(df)) {
    stop("The 'df' argument must be a data.frame")
  }

  # Get column names
  col_names <- names(df)

  # Function to clean each name
  clean_name <- function(name) {
    # Convert to lowercase
    name <- tolower(name)

    # Replace accents and common special characters
    if (replace_special_chars) {
      # Using unicode escape sequences instead of non-ASCII characters
      accents <- c("\u00e1", "\u00e9", "\u00ed", "\u00f3", "\u00fa", "\u00fc", "\u00f1",
                   "\u00e0", "\u00e8", "\u00ec", "\u00f2", "\u00f9",
                   "\u00e2", "\u00ea", "\u00ee", "\u00f4", "\u00fb",
                   "\u00e4", "\u00eb", "\u00ef", "\u00f6")
      replacements <- c("a", "e", "i", "o", "u", "u", "n",
                        "a", "e", "i", "o", "u",
                        "a", "e", "i", "o", "u",
                        "a", "e", "i", "o")

      for (i in seq_along(accents)) {
        name <- gsub(accents[i], replacements[i], name)
      }
    }

    # Remove parentheses and their content
    name <- gsub("\\([^)]*\\)", "", name)

    # Replace all non-alphanumeric characters with underscores
    name <- gsub("[^a-z0-9]", "_", name)

    # Remove leading underscores
    name <- gsub("^_+", "", name)

    # Remove trailing underscores
    name <- gsub("_+$", "", name)

    # Replace multiple consecutive underscores with a single one
    name <- gsub("_+", "_", name)

    # Ensure the name is syntactically valid in R
    if (grepl("^[0-9]", name)) {
      name <- paste0("x", name)
    }

    # Apply format according to the requested case
    if (case == "lower_camel") {
      parts <- strsplit(name, "_")[[1]]
      name <- parts[1]
      if (length(parts) > 1) {
        for (i in 2:length(parts)) {
          parts[i] <- paste0(toupper(substr(parts[i], 1, 1)), substr(parts[i], 2, nchar(parts[i])))
          name <- paste0(name, parts[i])
        }
      }
    } else if (case == "upper_camel") {
      parts <- strsplit(name, "_")[[1]]
      name <- ""
      for (i in 1:length(parts)) {
        parts[i] <- paste0(toupper(substr(parts[i], 1, 1)), substr(parts[i], 2, nchar(parts[i])))
        name <- paste0(name, parts[i])
      }
    } else if (case == "screaming_snake") {
      name <- toupper(name)
    }

    return(name)
  }

  # Apply the cleaning function to all names
  new_names <- sapply(col_names, clean_name)

  # Ensure names are unique
  if (unique_names && any(duplicated(new_names))) {
    for (i in which(duplicated(new_names))) {
      count <- sum(new_names[1:i] == new_names[i])
      new_names[i] <- paste0(new_names[i], "_", count)
    }
  }

  # Assign the new names
  names(df) <- new_names

  return(df)
}
