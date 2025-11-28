#' Options for the rosetta package
#'
#' The `rosetta::opts` object contains three functions to set, get, and reset
#' options used by the rosetta package. Use `rosetta::opts$set` to set options,
#' `rosetta::opts$get` to get options, or `rosetta::opts$reset` to reset specific or
#' all options to their default values.
#'
#' It is normally not necessary to get or set `rosetta` options.
#'
#' The following arguments can be passed:
#'
#' \describe{
#'   \item{...}{For `rosetta::opts$set`, the dots can be used to specify the options
#'   to set, in the format `option = value`, for example,
#'   `varViewCols = c("values", "level")`. For
#'   `rosetta::opts$reset`, a list of options to be reset can be passed.}
#'   \item{option}{For `rosetta::opts$set`, the name of the option to set.}
#'   \item{default}{For `rosetta::opts$get`, the default value to return if the
#'   option has not been manually specified.}
#' }
#'
#' The following options can be set:
#'
#' \describe{
#'
#'   \item{varViewCols}{The order and names of the columns to include in the
#'   variable view.}
#'
#'   \item{showLabellerWarning}{Whether to show a warning if labeller labels
#'   are encountered.}
#'
#' }
#'
#' @aliases opts set get reset
#'
#' @usage opts
#'
#' @examples ### Get the default columns in the variable view
#' rosetta::opts$get(varViewCols);
#'
#' ### Set it to a custom version
#' rosetta::opts$set(varViewCols = c("values", "level"));
#'
#' ### Check that it worked
#' rosetta::opts$get(varViewCols);
#'
#' ### Reset this option to its default value
#' rosetta::opts$reset(varViewCols);
#'
#' ### Check that the reset worked, too
#' rosetta::opts$get(varViewCols);
#'
#' @export
opts <- list();

opts$set <- function(...) {
  dots <- list(...);
  dotNames <- names(dots);
  names(dots) <-
    paste0("rosetta.", dotNames);
  if (all(dotNames %in% names(opts$defaults))) {
    do.call(options,
            dots);
  } else {
    stop("Option '", option, "' is not a valid (i.e. existing) option for rosetta!");
  }
}

opts$get <- function(option, default=FALSE) {
  option <- as.character(substitute(option));
  if (!option %in% names(opts$defaults)) {
    stop("Option '", option, "' is not a valid (i.e. existing) option for rosetta!");
  } else {
    return(getOption(paste0("rosetta.", option),
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
      paste0("rosetta.", optionNames);
    if (all(optionNames %in% names(opts$defaults))) {
      do.call(opts$set,
              opts$defaults[optionNames]);
    } else {
      invalidOptions <-
        !(optionNames %in% names(opts$defaults));
      stop("Option(s) ", vecTxtQ(optionNames[invalidOptions]),
           "' is/are not a valid (i.e. existing) option for rosetta!");
    }
  }
}

opts$defaults <-
  list(

    ### Column names in the variable view. Names that can be set are:
    ### colname (variable name), index (index of variable in dataframe),
    ### values (values of that variable), valids (nr of valid values),
    ### NAs (nr of missing values), level ("measurement level"),
    ### class (R class of the column).
    varViewCols = c('index', 'label', 'values',
                    'level', 'valids', 'NAs', 'class'),

    ### Whether to show varView's labeller warning
    showLabellerWarning = TRUE,

    ### Where to print tables; 'console', 'viewer', and/or
    ### one or more filenames in existing directories
    tableOutput = c("console", "viewer"),

    ### Whether you want extra information, as for debugging
    debugging = FALSE,

    ### RColorBrewer::brewer.pal(8, 'Set1')
    dlvPlotCompCols = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                        "#FF7F00", "#FFFF33", "#A65628", "#F781BF"),

    ### viridis::viridis(3)
    viridis3 = c("#440154FF", "#21908CFF", "#FDE725FF")

  )

