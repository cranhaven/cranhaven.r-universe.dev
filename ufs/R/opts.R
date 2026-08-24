#' Options for the ufs package
#'
#' The `ufs::opts` object contains three functions to set, get, and reset
#' options used by the ufs package. Use `ufs::opts$set` to set options,
#' `ufs::opts$get` to get options, or `ufs::opts$reset` to reset specific or
#' all options to their default values.
#'
#' It is normally not necessary to get or set `ufs` options.
#'
#' The following arguments can be passed:
#'
#' \describe{
#'   \item{...}{For `ufs::opts$set`, the dots can be used to specify the options
#'   to set, in the format `option = value`, for example,
#'   `tableOutput = c("console", "viewer")`. For
#'   `ufs::opts$reset`, a list of options to be reset can be passed.}
#'   \item{option}{For `ufs::opts$set`, the name of the option to set.}
#'   \item{default}{For `ufs::opts$get`, the default value to return if the
#'   option has not been manually specified.}
#' }
#'
#' The following options can be set:
#'
#' \describe{
#'
#'   \item{tableOutput}{Where to show some tables.}
#'
#' }
#'
#' @aliases opts set get reset
#'
#' @usage opts
#'
#' @examples ### Get the default columns in the variable view
#' ufs::opts$get("tableOutput");
#'
#' ### Set it to a custom version
#' ufs::opts$set(tableOutput = c("values", "level"));
#'
#' ### Check that it worked
#' ufs::opts$get("tableOutput");
#'
#' ### Reset this option to its default value
#' ufs::opts$reset("tableOutput");
#'
#' ### Check that the reset worked, too
#' ufs::opts$get("tableOutput");
#'
#' @export
opts <- list();

opts$set <- function(...) {
  dots <- list(...);
  dotNames <- names(dots);
  names(dots) <-
    paste0("ufs.", dotNames);
  if (all(dotNames %in% names(opts$defaults))) {
    do.call(options,
            dots);
  } else {
    stop("Option ", vecTxtQ(dotNames), " is/are not a valid (i.e. existing) option for ufs!");
  }
}

opts$get <- function(option, default=FALSE) {
  option <- as.character(substitute(option));
  if (!(option %in% names(opts$defaults))) {
    stop("Option '", option, "' is not a valid (i.e. existing) option for ufs!");
  } else {
    return(getOption(paste0("ufs.", option),
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
      paste0("ufs.", optionNames);
    if (all(optionNames %in% names(opts$defaults))) {
      do.call(opts$set,
              opts$defaults[optionNames]);
    } else {
      invalidOptions <-
        !(optionNames %in% names(opts$defaults));
      stop("Option(s) ", vecTxtQ(optionNames[invalidOptions]),
           "' is/are not a valid (i.e. existing) option for ufs!");
    }
  }
}

opts$ez <- list();
opts$ez$figSize <-
  function(size = c("A4", "slide", "A4slide"),
           margin = 1,
           marginUnit = "cm",
           portrait = FALSE,
           setOption = TRUE,
           setFontSize = TRUE,
           fontSizeMultiplier = 1.5) {

    if ("a4slide" %in% tolower(size[1])) {
      width <- 29.7/2.54;
      height <- (29.7 / (16/9))/2.54;
    } else if ("slide" %in% tolower(size[1])) {
      width <- 40/3;
      height <- 7.5;
    } else if ("a4" %in% tolower(size[1])) {
      width <- 29.7/2.54;
      height <- 21/2.54;
    }

    if (tolower(marginUnit) == "mm")
      margin <- margin / 100;
    if (tolower(marginUnit) == "cm")
      margin <- margin / 2.54;

    if (portrait) {
      width <- height - 2*margin;
      height <- width - 2*margin;
    } else {
      width <- width - 2*margin;
      height <- height - 2*margin;
    }

    if (setOption) {
      ufs::opts$set(ggSaveFigWidth = width,
                    ggSaveFigHeight = height,
                    ggSaveUnits = "in");
    }

    if (setFontSize) {
      ufs::opts$set(ggBaseSize = round(fontSizeMultiplier * max(c(height, width))));
    }

    return(invisible(c(width = width,
                       height = height)));

  }

### Helper function to check the cairo type
ggsaveDefaultType <- function() {
  if (length(capabilities("cairo") > 0) && capabilities("cairo")) {
    return("cairo");
  } else {
    return(NULL);
  }
}

# opts$ez$list <-
#   function(showValue = FALSE) {
#     optionNames <-
#       names(ufs::opts$defaults);
#     optionValues <-
#       do.call(ufs::opts$get,
#               list(optionNames));
#     if (showValue) {
#     } else {
#     }
#     return(
#   }

opts$defaults <-
  list(

    ### Regular expressions to use when parsing LimeSurvey
    ### labels in processLSvarLabels

    labelExtractionRegExPair = c("\\[(.*)\\].*", "\\1"),

    leftAnchorRegExPairs = list(
      c(".*[[:graph:]]\\s*([A-Z][a-z][^|]*)\\s*\\|\\s*(.+)",
        "\\1"),
      c(".*\\S\\.\\.\\.(\\S[^|]+)\\|(.+)",
        "\\1"),
      c(".*\\.([^|]+)\\|(.+)",
        "\\1"),
      c(".*\u2026(\\S[^|]+)\\s*\\|\\s*(.+)",
        "\\1"),
      c(".*:([^|]+)\\s*\\|\\s*(.+)",
        "\\1"),
      c(".*\\?([^|]+)\\s*\\|\\s*(.+)",
        "\\1"),
      c(".*\\S\u2026(\\S[^|]+)\\|(.+)",
        "\\1")
    ),

    rightAnchorRegExPairs = list(
      c(".*[[:graph:]]\\s*([A-Z][a-z][^|]*)\\s*\\|\\s*(.+)",
        "\\2"),
      c(".*\\.\\.\\.([^|]+)\\|(.+)",
        "\\2"),
      c(".*\\.([^|]+)\\|(.+)",
        "\\2"),
      c(".*:([^|]+)\\s*\\|\\s*(.+)",
        "\\2"),
      c(".*\u2026([^|]+)\\s*\\|\\s*(.+)",
        "\\2"),
      c(".*\\?([^|]+)\\s*\\|\\s*(.+)",
        "\\2"),
      c(".*\\S\u2026(\\S[^|]+)\\|(.+)",
        "\\2")
    ),

    ### ggSave default
    ggSaveFigWidth = 8,
    ggSaveFigHeight = 8,
    ggSaveUnits = "in",
    ggSaveDPI = 300,

    ggBaseSize = 11,

    ggsaveParams = list(units='cm',
                        dpi=300,
                        type=ggsaveDefaultType()),

    ### Default heading level, for convenience
    defaultHeadingLevel = 3,

    ### Where to print tables; 'console', 'viewer', and/or
    ### one or more filenames in existing directories
    tableOutput = c("console", "viewer"),

    ### CSS for tableOutput
    tableOutputCSS = paste0("<style>",
                            "p,th,td{font-family:sans-serif}",
                            "td{padding:3px;vertical-align:top;}",
                            "tr:nth-child(even){background-color:#f2f2f2}",
                            "</style>"),

    ### color to use for the background when exporting to html
    exportHTMLbackground = "white",

    ### Whether to use cat() to print the knitr fragment in knitAndSave.
    knitAndSave.catPlot = FALSE,
    knitFig.catPlot = FALSE,

    ggSavePreventType = TRUE,

    ### Careless options
    carelessDict = list(c("_chr", ""),
                        c("irv", "IRV "),
                        c("longstring", "Longstring"),
                        c("mahalanobis", "Mahalanobis Distance"),
                        c("responseTime", "Response Time")),

    ### Whether you want extra information, as for debugging
    debug = FALSE

  )

