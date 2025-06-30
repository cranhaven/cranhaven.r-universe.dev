`optimizeNetwork` <-
function (observations, predGrid, candidates, method, action,
    nDiff, model, criterion = "MUKV", plotOptim = TRUE, nGridCells,
    nTry, nr_iterations = 10000, formulaString, fun, ...)
{
    if (length(method) == 0)
        stop("'method' is missing")
    if (length(method) > 0) {
        if (method != "spcov" & method != "ssa" & method != "manual")
            stop(paste("The method  ", method, "  is not implemented"))
        if (method == "ssa" & length(criterion) > 0) {
            if (criterion != "MUKV")
                stop("Criterion ", criterion, " is not implemented.")
            if (criterion == "MUKV" & length(predGrid) == 0)
                stop("Missing prediction locations to compute MUKV.")
        }
    }
    if (length(action) == 0)
        stop("No 'action' defined ... choose 'add' or 'del'.")
    if (length(action) > 0) {
        if (action != "add" & action != "del")
            stop("No relevant 'action' defined ... choose 'add' or 'del'.")
        if (action == "add") {
            if (length(candidates) == 0 | !inherits(candidates, "SpatialPolygons"))
                stop("Candidate locations for additionnal measurements should be SpatialPolygons.")
        }
    }
    if (length(nDiff) == 0 | nDiff <= 0)
        stop(paste("nDiff is not well defined", nDiff))
    if (method == "ssa") {
        if (missing(formulaString) || is.null(formulaString)) {
            observations = SpatialPointsDataFrame(observations,
                data = data.frame(dum = rep(1, dim(coordinates(observations))[1])))
            formulaString = dum ~ 1
        }
      
        return(ssaOptim(observations, predGrid, candidates, action,
            nDiff, model, nr_iterations, plotOptim, formulaString, fun = fun,
            ...))
    }
    if (method == "spcov") {
        if (action == "add") {
              return(spCovAdd(observations, candidates, nDiff,
                  nGridCells, plotOptim, nTry))
        }
        if (action == "del") {
            return(spCovDel(observations, candidates, nDiff,
                plotOptim))
        }
    }
    if (method == "manual") {
        if (action == "add") {
            return(addManual(candidates, observations, nDiff))
        }
        if (action == "del") {
            return(delManual(candidates, observations, nDiff))
        }
    }
}
