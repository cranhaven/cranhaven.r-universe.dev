#' @title Random Forest Prediction
#' @description 
#'   Random Forest prediction via \code{\link[ranger]{predict.ranger}}. Predicts response variables
#'   from predictor variables, using \code{ranger} objects. All \code{ranger} objects have to work
#'   on the same set of prediction variables. These variables are exactly available in the
#'   prediction dataset. A response is not necessary, it will be predicted via this function.
#' @template dataset
#' @template predictors
#' @templateVar packagelink \code{\link[ranger]{predict.ranger.forest}}
#' @template robject
#' @template returnResults
#' @templateVar packagelink \code{\link[ranger]{ranger}}
#' @template threedots
#' @return
#'   Logical [\code{TRUE}] invisibly and outputs to Cornerstone or, 
#'   if \code{return.results = TRUE}, \code{\link{list}} of 
#'   resulting \code{\link{data.frame}} objects:
#'   \item{predictions}{
#'     Dataset to brush with predicted values for \code{dataset}. The original input and other
#'     columns can be added to this dataset through the menu \code{Columns -> Add from Parent ...}.
#'   }
#' @seealso \code{\link{randomForest}}
#' @export
randomForestPredict = function(dataset = cs.in.dataset()
                               , preds = cs.in.predictors(), robject = cs.in.Robject()
                               , return.results = FALSE
                               , ...
                               ) {
  # convert dataset to data.table
  dtDataset = as.data.table(dataset)
  
  # sanity checks
  assertCharacter(preds, any.missing = FALSE, min.len = 1)
  assertDataTable(dtDataset)
  assertSetEqual(names(dtDataset), preds)
  # check protected names in dataset, conflicts with data.table usage are possible
  assertDisjunct(names(dtDataset), c("pred", "preds", "resp", "resps", "group", "groups", "brush", "brushed"))
  assertDataTable(dtDataset[, preds, with = FALSE], any.missing = FALSE)
  assertList(robject, any.missing = FALSE, min.len = 1)
  assertFlag(return.results)
  
  # update to valid names
  preds = make.names(preds)
  colnames(dtDataset) = make.names(colnames(dtDataset))
  
  # remove outer list layer with names
  names(robject) = NULL
  rfs.per.list = vapply(robject, length, integer(1))
  robject = unlist(robject, recursive = FALSE)
  # get response names
  resps = names(robject)
  if (length(rfs.per.list) > 1) {
    resps = paste(resps, rep(seq_along(rfs.per.list), rfs.per.list), sep = ".")
    names(robject) = resps
  }
  
  # check variable names of rf with preds
  for (resp in resps) {
    assertSetEqual(robject[[resp]]$independent.variable.names, preds)
  }
  
  # init resulting data.table
  ndata = nrow(dtDataset)
  predictions = data.table(logical(ndata))
  colnames(predictions) = paste(c("V", resps), collapse = "")
  # predict all random forests
  for (resp in resps) {
    if (testClass(robject[[resp]], "ranger") | testClass(robject[[resp]], "ranger.forest")) {
      predictions[, (resp) := stats::predict(robject[[resp]], dtDataset, ...)$predictions]
    }
  }
  # delete init column
  predictions[, (paste(c("V", resps), collapse = "")) := NULL]
  
  # Export to Cornerstone
  cs.out.dataset(predictions, "Predictions", brush = TRUE)
  
  # return results
  if (return.results) {
    res = list(predictions = predictions)
    return(res)
  } else {
    invisible(TRUE)
  }
}
  