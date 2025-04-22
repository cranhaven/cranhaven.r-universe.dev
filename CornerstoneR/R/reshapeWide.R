#' @title Reshape Grouped Data to Wide
#' @description
#'   Reshaping grouped data via \code{\link[data.table:dcast.data.table]{dcast}} to 'wide' format with
#'   rows for each unique combination of group variables. The response are arranged in
#'   separate columns for each datum in predictors. If a combination of groups identifies
#'   multiple rows, the number of rows in a group is returned to CS for the whole dataset
#'   instead of the response variable value.
#' @template dataset
#' @template predictors
#' @template responses
#' @template groups
#' @template auxiliaries
#' @template scriptvars
#' @template returnResults
#' @templateVar packagelink \code{\link[data.table:dcast.data.table]{dcast}}
#' @template threedots
#' @details
#'   One script variables is summarized in \code{scriptvars} list:\cr
#'   \describe{
#'     \item{drop}{[\code{logical(1)}]\cr
#'       Drop missing combinations (\code{TRUE}) or include all (\code{FALSE}).
#'       Default is \code{TRUE}.\cr
#'       For details see \code{\link[data.table:dcast.data.table]{dcast}}.}
#'   }
#' @return
#'   Logical [\code{TRUE}] invisibly and outputs to Cornerstone or, 
#'   if \code{return.results = TRUE}, \code{\link{list}} of 
#'   resulting \code{\link{data.frame}} object:
#'   \item{reshapeWide}{Dataset with reshaped data.}
#' @export
#' @examples 
#' # Reshape dataset to wide format:
#' reshapeWide(Indometh, "time", "conc", "Subject", character(0)
#'             , list(drop = TRUE, aggr.fun = "mean"), return.results = TRUE
#'             )
reshapeWide = function(dataset = cs.in.dataset()
                       , preds = cs.in.predictors(), resps = cs.in.responses(), groups = cs.in.groupvars()
                       , auxs = cs.in.auxiliaries()
                       , scriptvars = cs.in.scriptvars()
                       , return.results = FALSE
                       , ...
                       ) {
  # sanity checks
  assertDataFrame(dataset)
  assertCharacter(preds, any.missing = FALSE, min.len = 1)
  assertCharacter(resps, any.missing = FALSE, min.len = 1)
  assertCharacter(groups, any.missing = FALSE, min.len = 1)
  assertCharacter(auxs, any.missing = FALSE)
  assertSetEqual(names(dataset), c(preds, resps, groups, auxs))
  # check protected names in dataset, conflicts with data.table usage are possible
  assertDisjunct(names(dataset), c("pred", "preds", "resp", "resps", "group", "groups", "brush", "brushed"))
  assertList(scriptvars, len = 2)
  assertFlag(scriptvars$drop)
  assertString(scriptvars$aggr.fun)
  if (scriptvars$aggr.fun == "")
    scriptvars$aggr.fun = "first"
  assertFlag(return.results)
  
  # convert to data.table
  dtDataset = as.data.table(dataset)
  # update to valid names
  preds = make.names(preds)
  resps = make.names(resps)
  groups = make.names(groups)
  auxs = make.names(auxs)
  colnames(dtDataset) = make.names(colnames(dtDataset))
  
  # define function for min and max by other column
  minby = function(x) x
  maxby = function(x) x
  deletethiscolumn = function(x) head(x, 1)
  
  # comma separated list of functions
  aggr.fun.name = unlist(strsplit(scriptvars$aggr.fun, "[,]"))
  aggr.fun.name = lapply(aggr.fun.name, trimws)
  # separate functions with brackets
  pattern = "^(minby|maxby)\\((.*)\\)$"
  aggr.fun.nameby = regmatches(aggr.fun.name, regexec(pattern, aggr.fun.name, perl = TRUE))
  # replace call by function name
  bln.byfuns = vapply(aggr.fun.nameby, length, integer(1)) > 0
  aggr.fun.name[bln.byfuns] = vapply(aggr.fun.nameby, `[`, character(1), n = 2L)[bln.byfuns]
  # remove empty list entries
  aggr.fun.nameby[!bln.byfuns] = NULL
  aggr.fun.name[bln.byfuns] = NULL
  # check whether columns exist
  assertSubset(vapply(aggr.fun.nameby, tail, character(1), n = 1L), names(dtDataset))
  # add deletethis column, if only one function is chosen
  # workaround to get named columns with data.table directly
  # see https://stackoverflow.com/questions/59409675/how-to-use-data-table-dcast-renaming-on-one-aggregation-function?noredirect=1#comment105012256_59409675
  if (length(aggr.fun.name) == 1)
    aggr.fun.name = c(aggr.fun.name, "deletethiscolumn")
  # evaluate aggregation functions
  aggr.fun = lapply(aggr.fun.name, function(x) eval(parse(text = x)))
  names(aggr.fun) = aggr.fun.name
  # work through all by columns
  res.by = NULL
  for (i in seq_along(aggr.fun.nameby)) {
    # recalculate dataset
    if (aggr.fun.nameby[[i]][2] == "minby")
      fun.min.or.max = which.min
    else
      fun.min.or.max = which.max
    # get row where variable in min/maxby() is min/max
    dtByData = dtDataset[, .SD[fun.min.or.max(get(aggr.fun.nameby[[i]][3]))], by = c(preds, groups)]
    # evaluate aggregate functions
    aggr.funby = lapply(list(aggr.fun.nameby[[i]][2], "deletethiscolumn"), function(x) eval(parse(text = x)))
    names(aggr.funby) = c(aggr.fun.nameby[[i]][2], "deletethiscolumn")
    # cast data to wide dataset with respect to responses
    # preds ~ groups, value.var = resps
    # Differences to standard call below: data, fun.aggregate
    res = data.table::dcast(  data = dtByData
                            , formula = stats::as.formula(paste(paste(preds, collapse = "+"), "~", paste(groups, collapse = "+")))
                            , fun.aggregate = aggr.funby
                            , fill = NA
                            , drop = scriptvars$drop
                            , value.var = resps
                            , ...
                            )
    # delete unnecessary columns
    res[, grep("deletethiscolumn", names(res), value = TRUE) := NULL]
    # safe in res.by
    if (is.null(res.by)) {
      res.by = res
    } else {
      # join by preds
      res.by = res.by[res]
    }
  }
  
  # cast data to wide dataset
  # preds ~ groups, value.var = resps
  res = data.table::dcast(  data = dtDataset
                          , formula = stats::as.formula(paste(paste(preds, collapse = "+"), "~", paste(groups, collapse = "+")))
                          , fun.aggregate = aggr.fun
                          , fill = NA
                          , drop = scriptvars$drop
                          , value.var = resps
                          , ...
                          )
  # delete unnecessary columns
  if (length(grep("deletethiscolumn", names(res))) > 0)
    res[, grep("deletethiscolumn", names(res), value = TRUE) := NULL]
  # join by preds
  if (!is.null(res.by))
    res = res[res.by]
  
  # export to Cornerstone
  cs.out.dataset(res, "Wide Data")
  
  # return results
  if (return.results) {
    res = list(reshapeWide = res)
    return(res)
  } else {
    invisible(TRUE)
  }
}
