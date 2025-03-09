#' Expand categorical attribute variables to a series of dichotomous variables
#'
#' @param data The data frame, normally the `$qdt` data frame that
#' exists in the object returned by a call to [parse_sources()].
#' @param attributes The name of the attribute(s) to expand.
#' @param valueLabels It's possible to use different names for the created
#' variables than the values of the attributes. This can be set with the
#' `valueLabels` argument. If only one attribute is specified, pass a named
#' vector for `valueLabels`, and if multiple attributes are specified, pass
#' a named list of named vectors, where the name of each vector corresponds to
#' an attribute passed in `attributes`. The names of the vector elements
#' must correspond to the values of the attributes (see the example).
#' @param prefix,suffix The prefix and suffix to add to the variables names
#' that are returned.
#' @param glue The glue to paste the first part ad the second part of the
#' composite variable name together.
#' @param falseValue,trueValue The values to set for rows that, respectively,
#' do not match and do match an attribute value.
#' @param valueFirst Whether to insert the attribute value first, or the
#' attribute name, in the composite variable names.
#' @param append Whether to append the columns to the supplied data
#' frame or not.
#'
#' @return A data.frame
#' @export
#'
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(examplePath, "example-1.rock");
#'
#' ### Parse single example source
#' parsedExample <- rock::parse_source(exampleFile);
#'
#' ### Create a categorical attribute column
#' parsedExample$qdt$age_group <-
#'   c(rep(c("<18", "18-30", "31-60", ">60"),
#'         each=19),
#'     rep(c("<18", ">60"),
#'         time = c(3, 4)));
#'
#' ### Expand to four logical columns
#' parsedExample$qdt <-
#'   rock::expand_attributes(
#'     parsedExample$qdt,
#'     "age_group",
#'     valueLabels =
#'       c(
#'         "<18" = "youngest",
#'         "18-30" = "youngish",
#'         "31-60" = "oldish",
#'         ">60" = "oldest"
#'        ),
#'     valueFirst = FALSE
#' );
#'
#' ### Show some of the result
#' table(parsedExample$qdt$age_group,
#'       parsedExample$qdt$age_group__youngest);
#' table(parsedExample$qdt$age_group,
#'       parsedExample$qdt$age_group__oldish);
expand_attributes <- function(data,
                              attributes,
                              valueLabels = NULL,
                              prefix="",
                              glue="__",
                              suffix="",
                              falseValue = 0,
                              trueValue = 1,
                              valueFirst = TRUE,
                              append = TRUE) {

  if (!(all(attributes %in% names(data)))) {
    stop("You specified one or more attributes that don't exist in the ",
         "data you passed: ",
         vecTxtQ(attributes[!(attributes %in% names(data))]),
         ".");
  }

  if (!is.null(valueLabels)) {
    if (length(attributes) == 1) {
      if (!is.vector(valueLabels)) {
        stop("If specifying only one attribute and passing `valueLabels`,",
             "the latter must be a named vector!");
      }
    } else if (is.list(valueLabels)) {
      if (!(all(names(valueLabels) %in% attributes))) {
        stop("If passing `valueLabels`, it must be a named list of named ",
             "vectors, where the name of each vector corresponds to an ",
             "attribute passed in `attributes`, and the names of each ",
             "vector element corresponds to the values of the attributes.");
      }
    } else {
      stop("If passing `valueLabels`, it must be a named list of named ",
           "vectors, where the name of each vector corresponds to an ",
           "attribute passed in `attributes`, and the names of each ",
           "vector element corresponds to the values of the attributes.");
    }
  }

  for (currentAttribute in attributes) {
    uniqueValues <-
      sort(unique(data[, currentAttribute]));
    if (is.null(valueLabels)) {
      labelTranslation <-
        stats::setNames(
          uniqueValues,
          nm = uniqueValues
        );
    } else {
      if (is.list(valueLabels)) {
        labelTranslation <-
          valueLabels[[currentAttribute]];
      } else {
        labelTranslation <-
          valueLabels;
      }
    }
    if (valueFirst) {
      newVarNames <-
        paste0(
          prefix,
          labelTranslation[uniqueValues],
          glue,
          currentAttribute,
          suffix
        );
    } else {
      newVarNames <-
        paste0(
          prefix,
          currentAttribute,
          glue,
          labelTranslation[uniqueValues],
          suffix
        );
    }
    subRes <-
      data.frame(
        stats::setNames(
          lapply(
            uniqueValues,
            function(x) {
              return(ifelse(data[, currentAttribute] == x,
                            trueValue,
                            falseValue));
            }
          ),
          nm = newVarNames
        )
      );
    if (!(exists('res')) || is.null(res)) {
      res <- subRes;
    } else {
      res <-
        cbind(
          res,
          subRes
        );
    }
  }

  if (append) {
    return(cbind(data, res));
  } else {
    return(res);
  }

}
