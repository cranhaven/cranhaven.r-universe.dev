#' @title Transpose Data
#' @description
#'   Transpose data via \code{\link[data.table]{transpose}}.
#'   All predictors, responses, groups, and auxiliaries are transpose.
#' @template dataset
#' @template groups
#' @template scriptvars
#' @template returnResults
#' @templateVar packagelink \code{\link[data.table]{transpose}}
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
#'   \item{reshapeTranspose}{Dataset with transposed data.}
#' @export
#' @examples
#' # Data to transform:
#' library(data.table)
#' dtTest = data.table(i_1 = c(1:4, NA, 5), i_2 = c(51, 61, NA , 71, 81, 91))
#' # Reshape to long format:
#' reshapeTranspose(dtTest, groups = character(0), list(convert.numeric = TRUE), return.results = TRUE)
reshapeTranspose = function(dataset = cs.in.dataset()
                            , groups = cs.in.groupvars()
                            , scriptvars = cs.in.scriptvars()
                            , return.results = FALSE
                            , ...
                            ) {
  # sanity checks
  assertDataFrame(dataset)
  assertCharacter(groups, any.missing = FALSE, max.len = 1)
  # check protected names in dataset, conflicts with data.table usage are possible
  assertDisjunct(names(dataset), c("pred", "preds", "resp", "resps", "group", "groups", "brush", "brushed"))
  assertList(scriptvars, len = 1)
  assertFlag(scriptvars$convert.numeric)
  assertFlag(return.results)
  
  # convert to data.table
  dtDataset = as.data.table(dataset)
  # check groups
  if (length(groups) == 0)
    groups = NULL
  
  # transpose data
  res = data.table::transpose(  l = dtDataset
                              , keep.names = "colnames"
                              , make.names = groups
                              , ...
                              )
  # convert data to numeric
  if (scriptvars$convert.numeric) {
    res = res[, lapply(.SD, function(x) gsub(",", ".", x)), by = colnames]
    res = suppressWarnings(
      res[, lapply(.SD, as.numeric), by = colnames]
    )
  }
  
  # export to Cornerstone
  cs.out.dataset(res, "Transposed Data")
  
  # return results
  if (return.results) {
    res = list(reshapeTranspose = res)
    return(res)
  } else {
    invisible(TRUE)
  }
}
