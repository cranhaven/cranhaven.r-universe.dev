#' @title Match Nearest Neighbor Between Two Datasets
#' @description
#'   Match the nearest neighbor from a redirected Cornerstone \code{Robject} dataset 
#'   (\code{\link{redirectDataset}}) to corresponding selected predictor variables.
#'   Predictor variables from both datasets are supposed to be numeric to apply the Euclidean
#'   distance calculated by \code{\link[SpatialTools]{dist2}}.
#'   The function returns a dataset with the nearest neighbor to every observation, matched by 
#'   the predictor variables.
#'   Available response, group, and auxiliary variables from the redirected datasets are passed
#'   through, as well as, selected auxiliary variables.
#'   The calculated distance is attached.
#' @template dataset
#' @template predictors
#' @template auxiliaries
#' @templateVar listlength one
#' @templateVar packagelink \code{\link{redirectDataset}}
#' @template robject
#' @template returnResults
#' @return
#'   Logical [\code{TRUE}] invisibly and outputs to Cornerstone or, 
#'   if \code{return.results = TRUE}, \code{\link{list}} of 
#'   resulting \code{\link{data.frame}} objects:
#'   \item{nearest.neighbor}{
#'     Matched nearest neighbor which consists of predictor and available response, group, and 
#'     auxiliary variables. The calculated distance is attached to this dataset.
#'   }
#'   \item{runtimes}{
#'     Run times for every input R object.
#'   }
#' @export
matchNearestNeighbor = function(dataset = cs.in.dataset()
                                , preds = cs.in.predictors(), auxs = cs.in.auxiliaries()
                                , robject = cs.in.Robject()
                                , return.results = FALSE
                                ) {
  # convert dataset to data.table
  dtDataset = as.data.table(dataset)
  
  # sanity checks
  assertCharacter(preds, any.missing = FALSE, min.len = 1)
  assertCharacter(auxs)
  assertDataTable(dtDataset)
  assertSetEqual(names(dtDataset), c(preds, auxs))
  # check protected names in dataset, conflicts with data.table usage are possible
  assertDisjunct(names(dtDataset), c("pred", "preds", "resp", "resps", "group", "groups", "brush", "brushed"))
  assertDataTable(dtDataset[, preds, with = FALSE], types = "numeric")
  assertList(robject, any.missing = FALSE, min.len = 1)
  assertFlag(return.results)
  
  # update to valid names
  preds = make.names(preds)
  auxs = make.names(auxs)
  colnames(dtDataset) = make.names(colnames(dtDataset))
  
  # due to non-sense notes in R CMD check
  runtime = NULL
  
  # function to calculate position of the minimal distance between a vector and a matrix
  minDistPos = function(coord1, coords2) {
    coord1 = matrix(coord1, nrow = 1)
    dists = SpatialTools::dist2(coord1, coords2)
    # FIXME: calculate number of minima?
    c(min.dist = min(dists), min.dist.pos = which.min(dists))
  }
  
  # init resulting data.tables
  runtimes = data.table(runtime = numeric(length(robject))
                        , unit = character(length(robject))
                        )
  
  # suppress output numbering, if only one redirected dataset available
  if (length(robject) > 1) {
    number.output = TRUE
  } else {
    number.output = FALSE
  }
  # loop robjects
  for (red.i in seq_along(robject)) {
    # Time measurement
    time.start = Sys.time()
    
    # get redirected dataset
    redirectedDataset = robject[[red.i]]
    if (!testClass(redirectedDataset, "CSR.redirectedDataset"))
      break
    redirDT = redirectedDataset$dataset
    assertSubset(preds, choices = names(redirDT))
    assertDataTable(redirDT[, preds, with = FALSE], types = "numeric", any.missing = FALSE)
    # calculate Euclidean distances row-wise to avoid a too large matrix
    # for instance, 10k x 900k needs 67.1 Gb
    coords1 = as.matrix(dtDataset[, preds, with = FALSE])
    coords2 = as.matrix(redirDT[, preds, with = FALSE])
    min.dist.pos = t(apply(coords1, 1, minDistPos, coords2))
    assertMatrix(min.dist.pos, any.missing = FALSE, nrows = nrow(dtDataset), ncols = 2)
    # append nearest neigbor in redirDT and minimal distance
    names(redirDT) = paste0(names(redirDT), ".red", ifelse(number.output, paste0(".", red.i), "ir"))
    dtDataset = cbind(dtDataset, redirDT[min.dist.pos[, 2], ])
    dtDataset[, (paste0("min.distance", ifelse(number.output, paste0(".", red.i), ""))) := min.dist.pos[, 1]]
    
    # End time measurement
    time.diff = Sys.time() - time.start
    runtimes[red.i, `:=` (runtime = as.numeric(time.diff), unit = attr(time.diff, "units"))]
  }
  
  # Export to Cornerstone
  cs.out.dataset(dtDataset, "Nearest Neighbors")
  cs.out.dataset(runtimes, "Runtimes")
  # return results
  if (return.results) {
    return(list(nearest.neighbors = dtDataset, runtimes = runtimes))
  } else {
    invisible(TRUE)
  }
}
