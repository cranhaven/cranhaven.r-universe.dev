#' @title Get variable labels or other metadata from a data frame in
#' opendataformat.
#'
#' @description Get access to information about the dataset
#' and variables via the R-Studio Viewer or the web browser.
#'
#' @param input R data frame (df) or variable from an R data frame (df$var).
#'
#' @param type The metadata type you want to retrieve.Possible options are
#' "label", "description", "url", "type", "valuelabels",
#' or "languages".
#'
#' @param language Select the language in which the labels of the variables
#' will be displayed. If no language is selected, the current/active language
#' of the data frame will be used.
#' \itemize{
#'   \item By default the language that is set to current is displayed
#'   (\code{language = "current"}). \item You can select the language
#'   by language code, e.g. \code{language = "en"}.
#' }
#'
#' @return Documentation.
#'
#' @examples
#' # get example data from the opendataformat package
#' df <- get(data("data_odf"))
#' # view the variable labels for all variables in English
#' getmetadata_odf(input = df, type = "label", language = "en")
#'
#' # view the value labels for variable bap87 in English
#' getmetadata_odf(input = df$bap87, type = "valuelabel", language = "en")
#'
#' # view the description for variable bap87 in English
#' getmetadata_odf(input = df$bap87, type = "description", language = "en")
#'
#' @export

getmetadata_odf <- function(input,
                            type,
                            language = "active") {
  if (language == "active" || language == "current") {
    lang <- attr(input, "lang")
  } else {
    lang <- language
  }
  if (length(lang) > 1) stop("Input for language invalid. 
                           Please specify only one language.")
  if (!(lang %in% attr(input, "languages"))) {
    stop("Input for language invalid.")
  }

  #check spelling of retrieve-parameter
  if (type == "label" || type == "labels" ||
      type == "Labels" || type == "Label") {
    type <- paste0("label_", lang)
  }
  if (type == "description" || type == "descriptions" ||
      type == "Description" || type == "Descriptions") {
    type <- paste0("description_", lang)
  }
  if (type  ==  "urls" || type  ==  "URL") {
    type <- "url"
  }
  if (type  ==  "types") {
    type <- "type"
  }
  if (type == "valuelabels" || type == "valuelabel" ||
      type == "value labels" || type == "value label" ||
      type == "Valuelabels" || type == "Valuelabel" ||
      type == "Value labels" || type == "Value Label") {
    type <- "valuelabels"
  }

  #check if type is valid
  if (!(type %in% c(paste0("label_", lang), "type",
                    paste0("description_", lang),
                    "url", "languages", "valuelabels"))) {
    stop(paste0("Function input type ", type, " is not valid"))
  }

  output <- c()
  output_names <- c()
  if ("data.frame" %in% class(input)) {
    if (type != "valuelabels") {
      output_names <- colnames(input)
      for (var in colnames(input)) {
        output <- c(output, attr(input[[var]], type))
      }
      names(output) <- output_names
    } else {
      output <- list()
      for (var in colnames(input)) {
        output[[var]] <- attr(input[[var]], paste0("labels_", lang))
      }
    }

  } else {
    if (type != "valuelabels") {
      output_names <- attr(input, "name")
      output <- attr(input, type)
      names(output) <- output_names
    } else {
      output <- attr(input, paste0("labels_", lang))
    }
  }
  return(output)
}
