#' @title Mosaic Plot
#' @description
#'   Plots (extended) mosaic displays via \code{\link[vcd]{mosaic}}.
#'   The last response variable is highlighted.
#'   A high-dimensional contingency table is calculated via \code{\link[vcd]{structable}}
#'   from the given dataset.
#'   Flat contingency table splits predictors horizontally and optional responses vertically.
#' @template dataset
#' @template predictors
#' @template responses
#' @templateVar packagelink \code{\link[vcd]{mosaic}}
#' @template returnResults
#' @template threedots
#' @export
#' @return
#'   Logical [\code{TRUE}] invisibly and outputs to Cornerstone or, 
#'   if \code{return.results = TRUE}, \code{\link{list}} of 
#'   resulting \code{\link{data.frame}} objects:
#'   \item{long.contingency}{Contingency table in long format.}
#' @examples
#' # Draw mosaic plot from 'titanic' data:
#' mosaicPlot(titanic, c("Class", "Age", "Sex", "Survived"))
#' res = mosaicPlot(titanic, c("Class", "Age"), c("Sex", "Survived"), return.results = TRUE)
#' print(res)
mosaicPlot = function(dataset = cs.in.dataset()
                      , preds = cs.in.predictors(), resps = cs.in.responses()
                      , return.results = FALSE
                      , ...
                      ) {
  # sanity checks
  assertDataFrame(dataset)
  assertCharacter(preds, min.len = 1)
  assertCharacter(resps, null.ok = TRUE)
  assertSetEqual(names(dataset), c(preds, resps))
  # check protected names in dataset, conflicts with data.table usage are possible
  assertDisjunct(names(dataset), c("pred", "preds", "resp", "group", "groups", "resps", "brush", "brushed"))
  
  # convert to data.table
  dtDataset = as.data.table(dataset)
  # update to valid names
  preds = make.names(preds)
  resps = make.names(resps)
  names(dtDataset) = make.names(names(dtDataset))
  
  # create formula
  frml = stats::as.formula(paste0(paste0(resps, collapse = "+"), "~", paste0(preds, collapse = "+")))
  # calculate contingency table
  conttable =  vcd::structable(frml, data = dtDataset)
  long.conttable = as.data.table(conttable)
  
  # color decision
  # no resps or up to two levels, blue red
  # resps with more than two levels, rainbow
  def.col = c("blue", "red")
  if (length(resps) > 0) {
    if (nlevels(dtDataset[[tail(resps, 1)]]) > 2) {
      def.col = grDevices::rainbow
    }
  } 
  # plots to Cornerstone
  cs.out.png("Mosaic Plot (PNG)")
  vcd::mosaic(conttable, highlighting = tail(resps, 1)
              , highlighting_fill = def.col
              , ...
              )
  cs.out.emf("Mosaic Plot (EMF)")
  vcd::mosaic(conttable, highlighting = tail(resps, 1)
              , highlighting_fill = def.col
              , ...
              )
  # export to Cornerstone
  cs.out.dataset(long.conttable, "Contingency Table")
  
  # return results
  if (return.results) {
    res = list(long.contingency = long.conttable)
    return(res)
  } else {
    invisible(TRUE)
  }
}
