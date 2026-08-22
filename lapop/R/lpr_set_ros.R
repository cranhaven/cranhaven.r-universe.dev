#######################################

# LAPOP Set Response Options Language #

#######################################

#' Set Response Option (ROS) labels for variables in AmericasBarometer datasets
#'
#' This function extracts formatted response option labels for AmericasBarometer
#' variables, using label tables stored as attributes. The labels are formatted
#' with their corresponding numeric codes and can be pulled in multiple languages.
#'
#' @param data A data frame loaded using readstata13 containing label.table attributes.
#' @param lang_id Language identifier for the labels ("en" for English,
#'        "es" for Spanish, "pt" for Portuguese). Default is "en" (English).
#' @param attribute_name The name of the attribute where the formatted response
#'        options string will be stored. Default is "roslabel".
#'
#' @return The input data frame with response option labels added to variables as attributes
#'
#' @details
#' The function looks for label tables stored as attributes of the data frame,
#' with names following the pattern "VARNAME_lang_id" (e.g., "ing4_en" for English
#' labels of variable ing4). Each label table should be a named numeric vector where
#' names are the response labels and values are the corresponding codes.
#'
#' Special codes (values >= 1000) are excluded from the response options string.
#'
#' @examples
#'
#' require(lapop); data(bra23)
#'
#' # Apply the function
#' bra23 <- lpr_set_ros(bra23) # Default English
#' bra23 <- lpr_set_ros(bra23, lang_id = "es", attribute_name = "respuestas") # Spanish
#' bra23 <- lpr_set_ros(bra23, lang_id = "pt", attribute_name = "ROsLabels_pt") # Portuguese
#'
#' # View the resulting attribute
#' attr(bra23$ing4, "roslabel")
#' attr(bra23$ing4, "respuestas")
#' attr(bra23$ing4, "ROsLabels_pt")
#'
#' @export

# Define the flexible label formatting function
lpr_set_ros <- function(data, lang_id = "en", attribute_name = "roslabel") {
  for (VAR in names(data)) {
    label_table_name <- paste0(VAR, "_", lang_id)

    # Extract available language codes from label names (e.g., "_pt", "_es", "_en")
    matches <- regexpr("(?<=_)[a-z]{2}$", label_table_name, perl = TRUE)
    lang_suffixes <- regmatches(label_table_name, matches)
    lang_suffixes <- lang_suffixes[matches != -1] # names that do not end in a valid 2-letter language code, c
    # causing regexpr() to return -1 and regmatches() to insert NA when coerced to a character vector.
    lang_suffixes <- lang_suffixes[!is.na(lang_suffixes) & lang_suffixes != ""] # Clean those NAs

    # Warn if requested lang_id not found
    if (!(lang_id %in% lang_suffixes)) {
      warning(sprintf(
        "Language '%s' not found in label tables. Available options: %s",
        lang_id, paste(unique(lang_suffixes), collapse = ", ")
      ))
      return(data)
    }

    # Check if the label table exists for the given language
    if (label_table_name %in% names(attr(data, "label.table"))) {
      label_table <- attr(data, "label.table")[[label_table_name]]
      valid_labels <- label_table[label_table < 1000]  # Exclude special codes

      formatted <- sapply(seq_along(valid_labels), function(i) {
        paste0("(", valid_labels[i], ") ", names(valid_labels)[i])
      })

      # Determine the prefix depending on language
      prefix <- switch(lang_id,
                       en = "Response Options: ",
                       es = "Opciones de Respuesta: ",
                       pt = "Alternativas de Resposta: ",
                       "")  # fallback

      attr(data[[VAR]], attribute_name) <- paste0(prefix, paste(formatted, collapse = " "))
    }
  }

  return(data)
}
