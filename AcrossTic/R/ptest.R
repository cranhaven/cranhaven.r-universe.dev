ptest <- function (acobj, y, edge.weights, n = 1000)
{
#
# Normally acobj will be a list of class AcrossTic. But we will also
# accept a (two-column) matrix of matches. In that case, we will need
# edge weights. They default to all 1's, but if there has been partial
# matching -- which we detect by a non-constant number of matchers --
# they're required.
#
if (class (acobj) != "AcrossTic" && class (acobj) != "matrix")
    stop ("First argument must be a matrix or an AcrossTic object")
if (class (acobj) == "matrix") {
    if (ncol (acobj) != 2) stop ("Matrix argument must have two columns")
    mat <- acobj
    if (missing (edge.weights)) {
        if (length (table (table (mat))) != 1)
            stop ("Edge weights are required for partial matching")
        edge.weights <- rep (1, nrow (mat))
    }
    acobj <- list (matches = mat, nrow.X = max (mat),
                   edge.weights = edge.weights)
}
#
#
# If y is missing and acobj has no y either, fail. If y is missing, use the
# y and the cross.count statistic from acobj.
#
if (missing (y)) {
    if (is.null (acobj$y))
        stop ("Can't run without 'y' explicit or in AcrossTic object", call.=FALSE)
    y <- acobj$y
    observed.cross.count <- acobj$cross.count
}
    else {
#
# If y is supplied, use it (unless it's the wrong length). Then compute
# the cross.count statistic from this y (even if acobj already had a y).
#
        if (length (y) != acobj$nrow.X) 
            stop (paste ("Y has length", length (y), ", should be", acobj$nrow.X))
        observed.cross.count <- sum (acobj$edge.weights[
                          y[acobj$matches[,1]] != y[acobj$matches[,2]]])
}


if (!any (names (acobj) == "matches"))
    stop ("Input object didn't have matches.")

result <- numeric (n)

for (i in 1:n) {
    y.new <- sample (y)
    cross <- y.new[acobj$matches[,1]] != y.new[acobj$matches[,2]]
    result[i] <- sum (acobj$edge.weights[cross])
}
out <- list (sims = result)
class (out) <- "AcrossTicPtest"
out$observed <- observed.cross.count
out$p.value <- mean (out$sims <= out$observed)
return (out)
}

