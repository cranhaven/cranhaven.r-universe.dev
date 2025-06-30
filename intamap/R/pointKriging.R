
estimateParameters.automap = function(object,...) {

  params = getIntamapParams(object$params, ...)
  debug.level = params$debug.level

  dots = list(...)
  observations = object$observations
  depVar = as.character(object$formulaString[[2]])
  if ("model" %in% names(params)) {
    model = params$model
  } else {
    if (dim(coordinates(object$predictionLocations))[1] > 100000) 
            model = c("Sph", "Exp", "Gau") else model = c("Sph", "Exp", "Gau", "Ste")
  }
    
#estimate Anisotropy
  if (params$doAnisotropy) {
    object = estimateAnisotropy(object) 
    if (object$anisPar$doRotation && all(as.character(object$formulaString[[3]])=="1")){
			#rotate Data
				objTemp = object
        objTemp$observations=rotateAnisotropicData(objTemp$observations,objTemp$anisPar)
				#Estimate Variogram Model
 #       if ("model" %in% names(dots)) {
 #   			 afv = autofitVariogram(objTemp$formulaString, objTemp$observations,
 #                  verbose = (debug.level >=2), ...)
 #       } else {
			    afv = autofitVariogram(objTemp$formulaString, objTemp$observations,
                   verbose = (debug.level >=2), model = model, ...)
#			  }
        vario = afv$var_model				
				ovar = var(observations[,depVar]@data)
      	if ((vario$model[2]  == "Gau" | (vario$model[2] == "Ste" && vario$kappa[2] > 2)) 
            && vario$psill[1] <= ovar/1e5 ) vario$psill[1] = ovar/1e5   
        #Combine the isotropic Model with anisotropy parameters
				vario$anis1[2]=1/objTemp$anisPar$ratio
				vario$ang1[2]=90-objTemp$anisPar$direction
        if (vario$ang1[2] < 0) vario$ang1[2] = vario$ang1[2] + 180				
				object$variogramModel=vario
#				vario$range=vario$range/objTemp$anisPar$ratio
  	} else {
		afv = autofitVariogram(object$formulaString,observations,verbose=(debug.level >=2),...)
    	object$variogramModel = afv$var_model
    }
  } else { 
  	afv = autofitVariogram(object$formulaString,observations,verbose=(debug.level >=2),...)
  	object$variogramModel = afv$var_model
  }
  if (debug.level >=2) print(object$variogramModel)
  object$sampleVariogram = afv$exp_var
  return(object)
}

spatialPredict.automap = function(object, nsim = 0, ...) {
# 
  params = getIntamapParams(object$params, ...)
  nmax = params$nmax
  nmin = params$nmin
  omax = params$omax
  beta = params$beta
  maxdist = params$maxdist
  debug.level = params$debug.level
  if (is.null(maxdist)) maxdist = Inf
  
    if (! "variogramModel" %in% names(object)) object = estimateParameters(object,...)
    
    nPred = nrow(coordinates(object$predictionLocations))
    if ("nclus" %in% names(params) && nsim == 0 && nPred >= 5000 ) 
      nclus = params$nclus else nclus = 1
    if (nclus > 1) {
      if (!suppressMessages(suppressWarnings(requireNamespace("doParallel"))))
  	    stop("nclus is > 1, but package doParallel is not available")    

    #  clus <- c(rep("localhost", nclus))
      cl <- makeCluster(nclus)
      registerDoParallel(cl, nclus)
#      clusterEvalQ(cl, library(gstat))
#      clusterEvalQ(cl, gstat::krige)
      formulaString = object$formulaString
      observations = object$observations
      predictionLocations = object$predictionLocations
      variogramModel = object$variogramModel
#      clusterExport(cl, list("formulaString", "observations", "predictionLocations",
#           "variogramModel", "nmax", "nsim", "debug.level"))
     # split prediction locations:
      splt = sample(1:nclus, nPred, replace = TRUE)
      splt = rep(1:nclus, each = ceiling(nPred/nclus), length.out = nPred)
      newdlst = lapply(as.list(1:nclus), function(w) predictionLocations[splt == w,])
      i = 1 # To avoid R CMD check complain about missing i
      pred <- foreach(i = 1:nclus, .combine = rbind) %dopar% {
        gstat::krige(formulaString, observations, 
           newdlst[[i]], variogramModel, nsim=nsim, nmax = nmax, nmin = nmin, omax = omax,  
           maxdist = maxdist, beta = beta, debug.level = debug.level)
      }
#      pred = do.call("rbind", parLapply(cl, newdlst, function(lst) 
#          krige(formulaString,observations, 
#           predictionLocations,variogramModel,nsim=nsim,nmax = nmax,debug.level = debug.level)))
      stopCluster(cl)
    } else {  
      pred = krige(object$formulaString, object$observations, 
           object$predictionLocations, object$variogramModel, nmax = nmax,
           nmin = nmin, omax = omax, maxdist = maxdist, beta = beta, debug.level = debug.level)
      if (nsim >0) {
        pred2 = krige(object$formulaString,object$observations, 
                      object$predictionLocations, object$variogramModel, nsim=nsim, nmax = nmax,
                      nmin = nmin, omax = omax, maxdist = maxdist, beta = beta, debug.level = debug.level)
        pred@data = cbind(pred2@data, pred@data)
      }
    }
    object$predictions = pred
    if ("MOK" %in% names(object$outputWhat) | "IWQSEL" %in% names(object$outputWhat))
      object$predictions = unbiasedKrige(object, debug.level = debug.level,...)$predictions
  object
}


estimateParameters.yamamoto = function(object,...) {
  estimateParameters.automap(object,...)
}




spatialPredict.yamamoto = function(object, nsim = 0, ...) {
# 
  params = getIntamapParams(object$params, ...)
  nmax = params$nmax
  nmin = params$nmin
  omax = params$omax
  beta = params$beta
  maxdist = params$maxdist
  debug.level = params$debug.level
  if (is.null(maxdist)) maxdist = Inf
  
  formulaString = object$formulaString

  if (!"variogramModel" %in% names(object)) {
    afv = autofitVariogram(object$formulaString,object$observations,object$predictionLocations)
  	object$variogramModel = afv$var_model
	object$sampleVariogram = afv$exp_var
  }
                     

  predictions = yamamotoKrige(formulaString,object$observations, 
            object$predictionLocations,object$variogramModel, nsim=nsim, nmax = nmax, maxdist = maxdist, ...)
  object$predictions = predictions
    if ("MOK" %in% names(object$outputWhat) | "IWQSEL" %in% names(object$outputWhat))
      object$predictions = unbiasedKrige(object,debug.level = debug.level,nsim = nsim, nmax = nmax, 
                                         nmin = nmin, omax = omax, maxdist = maxdist, beta = beta, ...)$predictions
  object
}

