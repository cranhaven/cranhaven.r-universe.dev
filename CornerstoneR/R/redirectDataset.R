#' @title Redirect Dataset
#' @description
#'   Redirect input dataset to an output R object.
#' @template dataset
#' @template predictors
#' @template responses
#' @template groups
#' @template auxiliaries
#' @template scriptvars
#' @template returnResults
#' @details
#'   The following script variables are summarized in \code{scriptvars} list:\cr
#'   \describe{
#'     \item{remove.pattern}{[\code{character(1)}]\cr
#'       The given pattern is removed in all variable names via \code{\link{gsub}}.
#'       Leading and / or trailing whitespaces are removed using \code{\link{trimws}}.
#'       Default is \code{""}.}
#'   }
#' @return
#'   Logical [\code{TRUE}] invisibly and outputs to Cornerstone \code{cs.out.Robject} or,
#'   if \code{return.results = TRUE}, \code{\link{list}} of resulting 
#'   \code{\link{data.frame}} objects and \code{character(n)} vectors:
#'   \item{dataset}{Input dataset.}
#'   \item{predictors}{Vector of predictors.}
#'   \item{responses}{Vector of responses.}
#'   \item{groups}{Vector of groups.}
#'   \item{auxiliaries}{Vector of auxiliaries.}
#'   The \code{list} is wrapped in an additional \code{list} to get the same return value
#'   corresponding to \code{cs.in.Robject}.
#' @export
redirectDataset = function(dataset = cs.in.dataset()
                           , preds = cs.in.predictors(), resps = cs.in.responses()
                           , groups = cs.in.groupvars(), auxs = cs.in.auxiliaries()
                           , scriptvars = cs.in.scriptvars()
                           , return.results = FALSE
                        ) {
  # convert dataset to data.table
  dtDataset = as.data.table(dataset)
  
  # sanity checks
  assertCharacter(preds, any.missing = FALSE)
  assertCharacter(resps, any.missing = FALSE)
  assertCharacter(groups, any.missing = FALSE)
  assertCharacter(auxs, any.missing = FALSE)
  assertDataTable(dtDataset)
  assertSetEqual(names(dtDataset), c(preds, resps, groups, auxs))
  # check protected names in dataset, conflicts with data.table usage are possible
  assertDisjunct(names(dtDataset), c("pred", "preds", "resp", "resps", "group", "groups", "brush", "brushed"))
  assertList(scriptvars, len = 1)
  assertString(scriptvars$remove.pattern)
  
  # remove pattern
  preds = trimws(gsub(scriptvars$remove.pattern, "", preds))
  resps = trimws(gsub(scriptvars$remove.pattern, "", resps))
  groups = trimws(gsub(scriptvars$remove.pattern, "", groups))
  auxs = trimws(gsub(scriptvars$remove.pattern, "", auxs))
  colnames(dtDataset) = trimws(gsub(scriptvars$remove.pattern, "", colnames(dtDataset)))
  
  # update to valid names
  preds = make.names(preds)
  resps = make.names(resps)
  groups = make.names(groups)
  auxs = make.names(auxs)
  colnames(dtDataset) = make.names(colnames(dtDataset))

  # check protected names again, conflicts with data.table usage are possible
  assertDisjunct(names(dtDataset), c("pred", "preds", "resp", "resps", "group", "groups", "brush", "brushed"))
  
  # combine information
  robjects = list(dataset = dtDataset
                  , predictors = preds
                  , responses = resps
                  , groups = groups
                  , auxiliaries = auxs
                  )
  # add CSR class
  class(robjects) = c("CSR.redirectedDataset", class(robjects))
  
  # Export to Cornerstone
  cs.out.Robject(robjects, "Redirected Dataset")
  # return results
  if (return.results) {
    return(list(robjects = robjects))
  } else {
    invisible(TRUE)
  }
}