estimateParameters.linearVariogram = function(object, ...) {
	# no parameters to be estimated...
	return(object)
}

spatialPredict.linearVariogram = function(object, nsim = 0, ...) {

  
  params = getIntamapParams(object$params, ...)
  nmax = params$nmax
  nmin = params$nmin
  omax = params$omax
  maxdist = params$maxdist
  beta = params$beta
  debug.level = params$debug.level
  
  object$predictions = krige(object$formulaString,object$observations, 
           object$predictionLocations, vgm(1, "Lin", 0), nsim=nsim, nmax = nmax, 
           nmin = nmin, omax = omax, beta = beta, maxdist = maxdist, debug.level = debug.level)


	return(object)
}

