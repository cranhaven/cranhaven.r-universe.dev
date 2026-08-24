#' @rdname detStructPreprocessing
#' @export
detStructComputeScales <- function(determinantStructure,
                                   data,
                                   append = TRUE,
                                   separator = "_") {

  if (!("determinantStructure" %in% class(determinantStructure))) {
    stop("The first argument must be a determinant structure object!");
  }

  if (!("data.frame" %in% class(data))) {
    stop("The second argument must be a dataframe!");
  }

  ### Get behavior regex
  ### Get all behaviorRegExes that are set (should only be one)
  behaviorRegEx <- data.tree::Get(nodes=data.tree::Traverse(determinantStructure,
                                                            traversal='level',
                                                            filterFun=function(x) return(!is.null(x$behaviorRegEx))),
                                  attribute='behaviorRegEx');

  ### Remove any duplicates and select the first one in case there are more
  behaviorRegEx <- unique(behaviorRegEx);

  if (length(behaviorRegEx) > 1) {
    warning("The determinant structure you specified has more than one behavior regular expression defined. Only using the first one, '",
            behaviorRegEx[1], "'.");
  }

  behaviorRegEx <- behaviorRegEx[1];

  ### Get all variables names of all 'product halves'
  scalables <- data.tree::Get(nodes=data.tree::Traverse(determinantStructure,
                                                        traversal='level',
                                                        filterFun=function(x) {
                                                          return(x$type == 'determinantVar');
                                                        }),
                              attribute="varNames",
                              simplify=FALSE);

  ### Remove superfluous level in between
  scalables <- lapply(scalables, unlist);

  ### Add behavior before variable names
  names(scalables) <- paste0(behaviorRegEx, separator, names(scalables));

  if (getOption('ufs.debug', FALSE)) {
    message("I will create these variables: ", ufs::vecTxtQ(names(scalables)), ".\n");
  }

  ### Add new variable names to determinant structure
  determinantStructure$Set(scaleVarName = names(scalables),
                           filterFun=function(x) {
                             return(x$type == 'determinantVar');
                           });

  nameSelection <- c(names(data), names(scalables));

  data <- ufs::makeScales(data, scalables);

  if (append) {
    return(data[, nameSelection]);
  } else {
    return(data[, names(scalables)]);
  }

}
