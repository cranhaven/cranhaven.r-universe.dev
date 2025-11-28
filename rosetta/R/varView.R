#' Variable View
#'
#' This function provides an overview of the variables in a
#' dataframe, allowing efficient inspection of the factor levels,
#' ranges for numeric variables, and numbers of missing values.
#'
#' @param data The dataframe containing the variables to view.
#' @param columns The columns to include.
#' @param varViewCols The columns of the variable view.
#' @param varViewRownames Whether to set the variable names as
#' row names of the variable view dataframe that is returned.
#' @param maxLevels For factors, the maximum number of levels to
#' show.
#' @param truncLevelsAt For factors levels, the number of characters
#' at which to truncate.
#' @param showLabellerWarning Whether to show a warning if labeller
#' labels are encountered.
#' @param output A character vector containing one or more of
#' "`console`", "`viewer`", and one or more filenames in existing
#' directories. If `output` contains `viewer` and RStudio is used,
#' the variable view is shown in the RStudio viewer.
#' @param x The varView data frame to print.
#' @param ... Any additional arguments are passed along to
#' the [print.data.frame()] function.
#'
#' @return A dataframe with the variable view.
#'
#' @author Gjalt-Jorn Peters & Melissa Gordon Wolf
#'
#' @examples ### The default variable view
#' rosetta::varView(iris);
#'
#' ### Only for a few variables in the dataset
#' rosetta::varView(iris, columns=c("Sepal.Length", "Species"));
#'
#' ### Set some variable and value labels using the `labelled`
#' ### standard, which is also used by `haven`
#' dat <- iris;
#' attr(dat$Sepal.Length, "label") <- "Sepal length";
#' attr(dat$Sepal.Length, "labels") <-
#'   c('one' = 1,
#'     'two' = 2,
#'     'three' = 3);
#'
#' ### varView automatically recognizes and shows these, adding
#' ### a 'label' column
#' rosetta::varView(dat);
#'
#' ### You can also specify that you only want to see some columns
#' ### in the variable view
#' rosetta::varView(dat,
#'                  varViewCols = c('label', 'values', 'level'));
#'
#'
#' @export
varView <- function(data,
                    columns = names(data),
                    varViewCols = rosetta::opts$get(varViewCols),
                    varViewRownames = TRUE,
                    maxLevels = 10,
                    truncLevelsAt = 50,
                    showLabellerWarning = rosetta::opts$get(showLabellerWarning),
                    output = rosetta::opts$get('tableOutput')) {

  ### Get original database name
  datasetName <-
    deparse(substitute(data));

  if (!all(columns %in% names(data))) {
    stop("You specified one or more columns that ",
         "don't actually exist in the dataset you passed ('",
         datasetName,
         "'). Specifically, the non-existent columns are: ",
         vecTxtQ(setdiff(columns, names(data))), ".");
  }

  ### Get indices of columns
  varIndices <-
    match(columns, names(data))

  ### Extract only the columns we want
  data <- data[, columns,
               drop=FALSE];

  ### Classes & "measurement levels"
  varClasses <-
    unlist(lapply(data,
                  function(x) {
                    return(
                      vecTxt(
                        class(x)
                      )
                    );
                  }));
  factors <-
    unlist(lapply(data,
                  is.factor));

  ordinalVars <-
    factors &
    unlist(lapply(data,
                  is.ordered));
  nominalVars <-
    factors & !ordinalVars;
  measurementLevels <-
    ifelse(ordinalVars,
           "ordinal",
           ifelse(nominalVars,
                  "nominal",
                  "continuous"));

  ### Get variable labels
  varLabels <-
    unlist(lapply(data,
                  function(x) {
                    res <-
                      attr(x, "label");
                    res <-
                      ifelse(is.null(res),
                             "",
                             res);
                    return(res);
                  }));
  varLabels <- trimws(varLabels);

  ### The values
  valuesOverview <-
    unlist(lapply(data,
                  function(x) {

                    ### Check for value labels set by e.g. `haven`
                    valueLabelValues <-
                      attr(x, "labels");
                    valueLabelLabels <-
                      names(valueLabelValues);
                    nLevels <- length(valueLabelValues);

                    if (is.factor(x)) {
                      ### First process factors
                      lvls <- levels(x);
                      vals <- seq_along(lvls);
                      nLevels <- length(lvls);
                      lvls <-
                        ifelse(nchar(lvls) <= truncLevelsAt,
                               lvls,
                               paste0(substr(lvls, 1, truncLevelsAt),
                                      "..."));
                      lvls <- lvls[1:min(nLevels, maxLevels)];
                      vals <- vals[1:min(nLevels, maxLevels)];
                      res <-
                        vecTxt(paste0(trimws(lvls), " (",
                                           vals, ")"));
                      if (nLevels > maxLevels) {
                        res <- paste0(res,
                                      " ... and ",
                                      nLevels - maxLevels,
                                      " more levels.");
                      }
                      return(res);
                    } else if (is.character(x)) {
                      ### The process character variables
                      nValue <- length(unique(stats::na.omit(x)));
                      ncharRange <- range(nchar(unique(stats::na.omit(x))),
                                          na.rm=TRUE);
                      return(paste0("A string variables with ",
                                    nValue, " different values of ",
                                    min(ncharRange), " to ",
                                    max(ncharRange), " characters."));
                    } else if (!is.null(nLevels) && (nLevels>0)) {
                      ### If these are set, use these
                      lvls <- valueLabelLabels[1:min(nLevels, maxLevels)];
                      vals <- valueLabelValues[1:min(nLevels, maxLevels)];
                      res <-
                        vecTxt(paste0(trimws(lvls), " (",
                                           vals, ")"));
                      if (nLevels > maxLevels) {
                        res <- paste0(res,
                                      " ... and ",
                                      nLevels - maxLevels,
                                      " more levels.");
                      }
                      return(res);
                    } else {
                      varMin <- round(min(x, na.rm=TRUE), 2);
                      varMax <- round(max(x, na.rm=TRUE), 2);
                      nValue <- length(unique(stats::na.omit(x)));
                      return(paste0(nValue, " unique values ",
                                    "ranging from ", varMin,
                                    " to ", varMax, "."));
                    }
                  }));

  ### Check which variables have `labelled` value labels specified
  labellerValueLabelsSet <-
    unlist(lapply(data,
                  function(x) {
                    return(!is.null(attr(x, "labels")));
                  }));
  measurementLevels <-
    ifelse(labellerValueLabelsSet,
           "ambiguous*",
           measurementLevels);

  ### Missing and valid values
  NAs <- unlist(lapply(data,
                       function(x) {
                         return(sum(is.na(x)));
                       }));
  valids <- nrow(data) - NAs;

  ### Remove variable labels if none are present
  if (all(varLabels=="")) {
    varViewCols <- setdiff(varViewCols, "label");
  }

  if (varViewRownames) {
    res <- data.frame(colname  = columns,
                      row.names = columns,
                      stringsAsFactors = FALSE);
  } else {
    res <- data.frame(colname  = columns,
                      row.names=NULL,
                      stringsAsFactors = FALSE);
  }

  ### Combine in a data.frame
  res <-
    cbind(res,
          data.frame(label = trimws(varLabels),
                     index = varIndices,
                     values = trimws(valuesOverview),
                     valids = trimws(valids),
                     NAs = trimws(NAs),
                     class = trimws(varClasses),
                     level = trimws(measurementLevels),
                     row.names=trimws(row.names(res)),
                     stringsAsFactors = FALSE));

  ### Select the specified columns and return the result
  res <- res[, varViewCols,
             drop = FALSE];
  class(res) <- c('rosettaVarView', class(res));
  attr(res, "dataName") <- datasetName;
  if (any(labellerValueLabelsSet)) {
    attr(res, "labellerValueLabelsSet") <- labellerValueLabelsSet;
  } else {
    attr(res, "labellerValueLabelsSet") <- FALSE;
  }
  attr(res, "output") <- output;

  return(res);

}

#' @rdname varView
#' @method print rosettaVarView
#' @export
print.rosettaVarView <- function(x,
                                 output = attr(x, "output"),
                                 ...) {
  labellerValueLabelsSet <-
    attr(x, 'labellerValueLabelsSet');
  res <- list(pre =
                ifelse(is.null(attr(x, "dataName")),
                       NULL,
                       paste0("Variable view for '", attr(x, "dataName"), "':")),
              input=x);
  class(res$input) <- 'data.frame';
  if ((length(labellerValueLabelsSet) == 1) &&
       (labellerValueLabelsSet == FALSE)) {
    res <-
      c(res,
        list(post = NULL));
  } else {
    res$post <-
      paste0("\n* Note that value labels were set conform ",
             "the `labeller` package convention, for example as a result ",
             "of importing a dataset (from SPSS, STATA or SAS) ",
             "using the `haven` package. These variables (",
             vecTxtQ(row.names(x)[labellerValueLabelsSet]),
             ") are considered continuous by R, but the assignment ",
             "of value labels implies that the numeric values ",
             "represent categories, and if that is the case, ",
             "these variables should be stored as factors in R.");
  }
  return(ufs::exportToHTML(input = res,
                           output = output));
}

