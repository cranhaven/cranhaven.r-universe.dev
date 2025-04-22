#' @title Reshape Grouped Data to Long
#' @description
#'   Reshaping grouped data via \code{\link[data.table:melt.data.table]{melt}} to 'long' format. The
#'   responses are merged in one column, with its column name in an additional column.
#'   This column is split into multiple columns, if a split character is given.
#'   All predictors are merged multiple times corresponding to the number or responses.
#' @template dataset
#' @template predictors
#' @template responses
#' @template scriptvars
#' @template returnResults
#' @templateVar packagelink \code{\link[data.table:melt.data.table]{melt}}
#' @template threedots
#' @details
#'   One script variables is summarized in \code{scriptvars} list:\cr
#'   \describe{
#'     \item{split}{[\code{character(1)}]\cr
#'       Split character to split response names into multiple columns. Default is \dQuote{_}.}
#'   }
#' @return
#'   Logical [\code{TRUE}] invisibly and outputs to Cornerstone or, 
#'   if \code{return.results = TRUE}, \code{\link{list}} of 
#'   resulting \code{\link{data.frame}} object:
#'   \item{reshapeLong}{Dataset with reshaped data.}
#' @export
#' @examples
#' # Data to transform:
#' library(data.table)
#' dtTest = data.table(i_1 = c(1:4, NA, 5), i_2 = c(51, 61, NA , 71, 81, 91)
#'                     , f1 = factor(sample(c(letters[1:3], NA), 6, TRUE))
#'                     , f2 = factor(c("z", "a", "x", "c", "x", "x"), ordered = TRUE)
#'                     )
#' # Reshape to long format:
#' reshapeLong(dtTest, c("i_1", "i_2"), c("f1", "f2"), list(split = "_"), return.results = TRUE)
reshapeLong = function(dataset = cs.in.dataset()
                       , preds = cs.in.predictors(), resps = cs.in.responses()
                       , scriptvars = cs.in.scriptvars()
                       , return.results = FALSE
                       , ...
                       ) {
  # sanity checks
  assertDataFrame(dataset)
  assertCharacter(preds, any.missing = FALSE)
  assertCharacter(resps, any.missing = FALSE, min.len = 1)
  assertSetEqual(names(dataset), c(preds, resps))
  # check protected names in dataset, conflicts with data.table usage are possible
  assertDisjunct(names(dataset), c("pred", "preds", "resp", "resps", "group", "groups", "brush", "brushed"))
  assertList(scriptvars, len = 1)
  assertString(scriptvars$split, min.chars = 0)
  assertFlag(return.results)
  
  # convert to data.table
  dtDataset = as.data.table(dataset)
  # update to valid names
  preds = make.names(preds)
  resps = make.names(resps)
  colnames(dtDataset) = make.names(colnames(dtDataset))
  
  # due to non-sense notes in R CMD check
  variable = NULL

  # melt data to long dataset
  # id.vars = preds, value.var = resps
  res = data.table::melt(  data = dtDataset
                         , id.vars = preds
                         , measure.vars = resps
                         , ...
                         )
  if (nchar(scriptvars$split) > 0) {
    res.vars = res[, tstrsplit(variable, scriptvars$split)]
    if (ncol(res.vars) > 1) {
      colnames(res.vars) = paste0("variable", seq_along(res.vars))
      res = cbind(res, res.vars)
      res[, variable := NULL]
      setcolorder(res, c(preds, colnames(res.vars), "value"))
    }
  }
  
  # export to Cornerstone
  cs.out.dataset(res, "Long Data")
  
  # return results
  if (return.results) {
    res = list(reshapeLong = res)
    return(res)
  } else {
    invisible(TRUE)
  }
}
