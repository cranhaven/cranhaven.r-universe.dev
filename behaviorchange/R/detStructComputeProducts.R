#' @rdname detStructPreprocessing
#' @export
detStructComputeProducts <- function(determinantStructure,
                                     data,
                                     append = TRUE) {

  ### Get all variables names of all 'product halves'
  multiplicables <-
    data.tree::Get(nodes=data.tree::Traverse(determinantStructure,
                                             traversal='level',
                                             filterFun=function(x) {
                                               return(!is.null(x$type) &&
                                                        isTRUE(x$type == 'subdeterminantProducts'));
                                             }),
                   attribute="varNames",
                   simplify=FALSE);

  if (getOption('ufs.debug', FALSE)) {
    message("Debugging message:\n  Extracted the following products to compute:\n",
            paste0(utils::capture.output(utils::str(multiplicables)), collapse="\n"), "\n");
  }

  ### Remove those that don't occur in both lists
  actualMultiplicables <- lapply(1:length(multiplicables), function(i) {
    varNameVectors <- list(gsub(paste0('(.*)', names(multiplicables[[i]][1]), '(.*)'),
                                "\\1\\2",
                                multiplicables[[i]][[1]]),
                           gsub(paste0('(.*)', names(multiplicables[[i]][2]), '(.*)'),
                                "\\1\\2",
                                multiplicables[[i]][[2]]));
    varNameVectors <- list(sort(multiplicables[[i]][[1]][varNameVectors[[1]]
                                                         %in% varNameVectors[[2]]]),
                           sort(multiplicables[[i]][[2]][varNameVectors[[2]]
                                                         %in% varNameVectors[[1]]]));
    return(varNameVectors);
  });

  if (getOption('ufs.debug', FALSE)) {
    message("Debugging message:\n  After checking for product elements that did not occur in the list, these remained:\n",
            paste0(utils::capture.output(utils::str(actualMultiplicables)), collapse="\n"), "\n");
  }

  ### Copy dataframe, but drop all variables
  resDat <- data[, FALSE];

  for (currentSetIndex in 1:length(actualMultiplicables)) {
    ### Identifying but of this product
    productIdentifier <- names(multiplicables)[currentSetIndex];
    ### In the variable names, replace the bit specific to
    ### each half of the product with the name of the
    ### product as specified in the determinant structure
    newNames <- gsub(paste0('(.*)', names(multiplicables[[currentSetIndex]][1]), '(.*)'),
                     paste0("\\1",  productIdentifier, "\\2"),
                     actualMultiplicables[[currentSetIndex]][[1]]);

    if (getOption('ufs.debug', FALSE)) {
      message("Debugging message:\n  Set new names: ",
              ufs::vecTxtQ(newNames), "\n");
    }

    data.tree::Do(nodes=data.tree::Traverse(determinantStructure,
                                            filterFun=function(x) {
                                              return(x$name==productIdentifier);
                                            }),
                  fun=function(currentNode) {
      currentNode$productVarNames <- newNames;
    });

    ### Loop through the product halves and compute and store the products
    for (i in 1:length(actualMultiplicables[[currentSetIndex]][[1]])) {

      if (!all(c(actualMultiplicables[[currentSetIndex]][[1]][i],
                 actualMultiplicables[[currentSetIndex]][[2]][i]) %in%
               names(data))) {
        stop(paste0("One or both of these variables that you specified to ",
                    "multiply to obtain determinant structure product terms ",
                    "does not exist: ",
                    actualMultiplicables[[currentSetIndex]][[1]][i],
                    " and ",
                    actualMultiplicables[[currentSetIndex]][[2]][i]), ".");
      }

      resDat[, newNames[i]] <-
        data[, actualMultiplicables[[currentSetIndex]][[1]][i]] *
        data[, actualMultiplicables[[currentSetIndex]][[2]][i]];
    }

  }

  ### Return the results
  if (append) {
    return(cbind(data, resDat));
  } else {
    return(resDat);
  }

}
