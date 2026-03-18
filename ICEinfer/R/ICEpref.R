"ICEpref" <- function (tr, ex, cy, lambda = 1, beta = 1, eta = 3 + 2 * sqrt(2))  
{
    if (lambda <= 0) 
        stop("The Lambda argument to ICEpref must be strictly positive.")
    if (beta <= 0) 
        stop("The Beta argument to ICEpref must be strictly positive.")
    if (eta <= 0) 
        stop("The Eta = Gamma/Beta ratio argument to ICEpref must be strictly positive.")
    if (eta > 3 + 2 * sqrt(2)) 
        cat("\nThese preferences violate the ICE Monotonicity Axiom.\n\n")
    gamma = eta * beta
    n <- length(tr)
    pref <- rep(0, n)
    nt0 <- 0
    mex <- 0
    mcy <- 0
    for (i in 1:n) {
      if (tr[i] != 0 && tr[i] != 1)
        stop("Every element of the Treatment tr-vector must be either 0 or 1 ...Std or New Regimen.")
      nt0 <- nt0 + (1-tr[i])
      mex <- mex + (1-tr[i])*ex[i]
      mcy <- mcy + (1-tr[i])*cy[i]
    }
    if (nt0 == 0) stop("No tr == 0 [Std] observations are included.")
    mex <- mex/nt0           # tr == 0 ex-mean value...
    mcy <- mcy/nt0           # tr == 0 cy-mean value...	
    for (i in 1:n) {
      rad <- sqrt((ex[i]-mex)^2*lambda^2 + (cy[i]-mcy)^2)
      gabs <- abs((ex[i]-mex)*lambda - (cy[i]-mcy))
      if (gabs > 0) gabs <- gabs^gamma
      if (rad == 0) pref[i] <- 0
      else pref[i] <- rad^(beta - gamma) * sign((ex[i]-mex)*lambda-(cy[i]-mcy)) * gabs
    }
	ICEolist <- list(pref = pref, mex = mex, mcy = mcy, n0 = nt0, n1 = n - nt0)
    class(ICEolist) <- "ICEpref"
    ICEolist
}

###########################################################################
#
# NEW Preference Scoring function for Individual Patients receiving 1 of 2 Treatment
# Regimen (tr = 0 ["Std"] or tr = 1 ["New"] using an ICE Economic Preference Map with
# strictly positive and finite parameters lambda = "Shadow Price of Health", beta =
# "Radius Power" and gamma = "Absolute Difference Power". When eta = gamma / beta = 1,
# the Map will be "linear" in the sense of having Constant_Preference contours strictly
# parallel to the cost = effe diagonal line. Furthermore, Maps with beta = 1 have
# "constant" (or "linear") Returns-to-Scale; beta > 1 MAPs have "increasing" Returns to
# Scale; and beta < 1 MAPs have "diminishing" Returns-to-Scale.

