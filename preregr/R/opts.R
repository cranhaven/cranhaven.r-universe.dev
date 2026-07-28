#' Options for the preregr package
#'
#' The `preregr::opts` object contains three functions to set, get, and reset
#' options used by the preregr package. Use `preregr::opts$set` to set options,
#' `preregr::opts$get` to get options, or `preregr::opts$reset` to reset specific or
#' all options to their default values.
#'
#' It is normally not necessary to get or set `preregr` options.
#'
#' The following arguments can be passed:
#'
#' \describe{
#'   \item{...}{For `preregr::opts$set`, the dots can be used to specify the options
#'   to set, in the format `option = value`, for example, `utteranceMarker = "\n"`. For
#'   `preregr::opts$reset`, a list of options to be reset can be passed.}
#'   \item{option}{For `preregr::opts$set`, the name of the option to set.}
#'   \item{default}{For `preregr::opts$get`, the default value to return if the
#'   option has not been manually specified.}
#' }
#'
#' The following options can be set:
#'
#' \describe{
#'   \item{quridPrefix}{The prefix for quasi-unique record identifiers (QURIDs).}
#'
#'   \item{Two}{Second item}
#' }
#'
#' @aliases opts set get reset
#'
#' @usage opts
#'
#' @examples ### Get the default "silence versus chattiness" setting
#' preregr::opts$get(silent);
#'
#' ### Set it to show all messages
#' preregr::opts$set(silent = FALSE);
#'
#' ### Check that it worked
#' preregr::opts$get(silent);
#'
#' ### Reset this option to its default value
#' preregr::opts$reset(silent);
#'
#' ### Check that the reset worked, too
#' preregr::opts$get(silent);
#'
#' @export
opts <- list();

opts$set <- function(...) {
  dots <- list(...);
  dotNames <- names(dots);
  names(dots) <-
    paste0("preregr.", dotNames);
  if (all(dotNames %in% names(opts$defaults))) {
    do.call(options,
            dots);
  } else {
    stop("Option '", option, "' is not a valid (i.e. existing) option for preregr!");
  }
}

opts$get <- function(option, default=FALSE) {
  option <- as.character(substitute(option));
  if (!option %in% names(opts$defaults)) {
    stop("Option '", option, "' is not a valid (i.e. existing) option for preregr!");
  } else {
    return(getOption(paste0("preregr.", option),
                     opts$defaults[[option]]));
  }
}

opts$reset <- function(...) {
  optionNames <-
    unlist(lapply(as.list(substitute(...())),
                  as.character));
  if (length(optionNames) == 0) {
    do.call(opts$set,
            opts$defaults);
  } else {
    prefixedOptionNames <-
      paste0("preregr.", optionNames);
    if (all(optionNames %in% names(opts$defaults))) {
      do.call(opts$set,
              opts$defaults[optionNames]);
    } else {
      invalidOptions <-
        !(optionNames %in% names(opts$defaults));
      stop("Option(s) ", vecTxtQ(optionNames[invalidOptions]),
           "' is/are not a valid (i.e. existing) option for preregr!");
    }
  }
}

opts$defaults <-
  list(

    ### Used throughout
    debug = FALSE,

    ### Regular expressions for Google Sheets
    gSheetId_extractionRegex =
      "^https://docs\\.google\\.com/spreadsheets/d/([a-zA-Z0-9_-]*)(/.*)?$",

    gSheetId_to_exportLink =
      "https://docs.google.com/spreadsheets/d/%s/export?format=xlsx",

    itemCols = c(section_id_Col = "section_id",
                 id_Col = "item_id",
                 label_Col = "item_label",
                 description_Col = "item_description",
                 valueTemplate_Col = "item_valueTemplate",
                 validValues_Col = "item_validValues",
                 validation_Col = "item_validation",
                 error_Col = "item_error"),

    valueTemplateCols = c(identifierCol = "identifier",
                          descriptionCol = "description",
                          validValuesCol = "validValues",
                          defaultCol = "default",
                          examplesCol = "examples",
                          validationCol = "validation",
                          errorCol = "error"),

    defaultGraphTheme = list(
      c("fontname", "Arial", "graph"),
      c("fontname", "Arial", "node"),
      c("fontname", "Arial", "edge"),
      c("layout", "dot", "graph"),
      c("rankdir", "LR", "graph"),
      c("outputorder", "edgesfirst", "graph"),
      c("fixedsize", "false", "node"),
      c("shape", "box", "node"),
      c("style", "filled", "node"),
      c("color", "#000000", "node"),
      c("color", "#888888", "edge"),
      c("dir", "none", "edge"),
      c("headclip", "false", "edge"),
      c("tailclip", "false", "edge"),
      c("fillcolor", "#FFFFFF", "node")
    ),

    diagrammerSanitization = list(c("\\\"", "`"),
                                  c("\\'", "`"),
                                  c("\\\\", "/"),
                                  c("[^a-zA-Z0-9;)(,._/`-]", " ")),

    ### Headings
    defaultHeadingLevel = 3,

    ### For validation
    validation_replacementDelimiters = c("{{", "}}"),

    ### Used throughout for working with files
    encoding = "UTF-8",
    preventOverwriting = TRUE,

    ### Used throughout for suppressing messages
    silent = FALSE

  );
