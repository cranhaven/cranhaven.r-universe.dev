#'Make categorical forecast based on a multi-model forecast with potential for 
#'calibrate
#'
#'@author Bert Van Schaeybroeck, \email{bertvs@meteo.be}
#'@description This function converts a multi-model ensemble forecast into a 
#'categorical forecast by giving the probability for each category. Different 
#'methods are available to combine the different ensemble forecasting models 
#'into probabilistic categorical forecasts. 
#'
#'Motivation: Beyond the short range, the unpredictable component of weather 
#'predictions becomes substantial due to the chaotic nature of the earth system. 
#'Therefore, predictions can mostly be skillful when used in a probabilistic 
#'sense. In practice this is done using ensemble forecasts. It is then common to
#'convert the ensemble forecasts to occurence probabilities for different 
#'categories. These categories typically are taken as terciles from 
#'climatolgical distributions. For instance for temperature, there is a cold, 
#'normal and warm class. Commonly multiple ensemble forecasting systems are 
#'available but some models may be more competitive than others for the 
#'variable, region and user need under consideration. Therefore, when 
#'calculating the category probabilities, the ensemble members of the different 
#'forecasting system may be differently weighted. Such weighting is typically 
#'done by comparison of the ensemble forecasts with observations. 
#'
#'Description of the tool: The tool considers all forecasts (all members from 
#'all forecasting systems) and converts them into occurrence probabilities of 
#'different categories. The amount of categories can be changed and are taken as 
#'the climatological quantiles (e.g. terciles), extracted from the observational 
#'data. The methods that are available to combine the ensemble forecasting 
#'models into probabilistic categorical forecasts are: 1) ensemble pooling where 
#'all ensemble members of all ensemble systems are weighted equally, 
#' 2) model combination where each model system is weighted equally, and,
#' 3) model weighting. 
#'The model weighting method is described in Rajagopalan et al. (2002),
#'Robertson et al. 2004 and Van Schaeybroeck and Vannitsem (2019). More 
#'specifically, this method uses different weights for the occurence probability 
#'predicted by the available models and by a climatological model and optimizes 
#'the weights by minimizing the ignorance score. Finally, the function can also 
#'be used to categorize the observations in the categorical quantiles.
#' 
#'@param exp An object of class \code{s2dv_cube} as returned by \code{CST_Load} 
#'  function, containing the seasonal forecast experiment data in the element 
#'  named \code{$data}. The amount of forecasting models is equal to the size of 
#'  the \code{dataset} dimension of the data array. The amount of members per 
#'  model may be different. The  size of the \code{member} dimension of the data
#'  array is equal to the maximum of the ensemble members among the models. 
#'  Models with smaller ensemble sizes have residual indices of \code{member} 
#'  dimension in the data array filled with NA values.
#'@param obs An object of class \code{s2dv_cube} as returned by \code{CST_Load} 
#'  function, containing the observed data in the element named \code{$data}.
#'@param amt.cat Is the amount of categories. Equally-sized quantiles will be 
#'  calculated based on the amount of categories.
#'@param cat.method Method used to produce the categorical forecast, can be 
#'  either \code{pool}, \code{comb}, \code{mmw} or \code{obs}. The method pool 
#'  assumes equal weight for all ensemble members while the method comb assumes 
#'  equal weight for each model. The weighting method is descirbed in 
#'  Rajagopalan et al. (2002), Robertson et al. (2004) and Van Schaeybroeck and 
#'  Vannitsem (2019). Finally, the \code{obs} method classifies the observations 
#'  into the different categories and therefore contains only 0 and 1 values. 
#'@param eval.method Is the sampling method used, can be either 
#'  \code{"in-sample"} or \code{"leave-one-out"}. Default value is the 
#'  \code{"leave-one-out"} cross validation. 
#'@param ... other parameters to be passed on to the calibration procedure.
#'
#'@return An object of class \code{s2dv_cube} containing the categorical 
#'forecasts in the element called \code{$data}. The first two dimensions of the 
#'returned object are named dataset and member and are both of size one. An 
#'additional dimension named category is introduced and is of size amt.cat.
#'
#'@references Rajagopalan, B., Lall, U., & Zebiak, S. E. (2002). Categorical 
#'climate forecasts through regularization and optimal combination of multiple 
#'GCM ensembles. Monthly Weather Review, 130(7), 1792-1811.
#'@references Robertson, A. W., Lall, U., Zebiak, S. E., & Goddard, L. (2004). 
#'Improved combination of multiple atmospheric GCM ensembles for seasonal 
#'prediction. Monthly Weather Review, 132(12), 2732-2744.
#'@references Van Schaeybroeck, B., & Vannitsem, S. (2019). Postprocessing of 
#'Long-Range Forecasts. In Statistical Postprocessing of Ensemble Forecasts (pp. 267-290).
#'
#'@examples 
#'mod1 <- 1 : (2 * 2* 4 * 5 * 2 * 2)
#'dim(mod1) <- c(dataset = 2, member = 2, sdate = 4, ftime = 5, lat = 2, lon = 2)
#'mod1[2, 1, , , , ] <- NA
#'datasets <- c("MF", "UKMO")
#'obs1 <- 1 : (1 * 1 * 4 * 5 * 2 * 2)
#'dim(obs1) <- c(dataset = 1, member = 1, sdate = 4, ftime = 5, lat = 2, lon = 2)
#'lon <- seq(0, 30, 5)
#'lat <- seq(0, 25, 5)
#'coords <- list(lat = lat, lon = lon)
#'attrs <- list(Datasets = datasets)
#'exp <- list(data = mod1, coords = coords, attrs = attrs)
#'obs <- list(data = obs1, coords = coords)
#'attr(exp, 'class') <- 's2dv_cube'
#'attr(obs, 'class') <- 's2dv_cube'
#'a <- CST_CategoricalEnsCombination(exp = exp, obs = obs, amt.cat = 3, 
#'                                   cat.method = "mmw") 
#'@importFrom s2dv InsertDim
#'@import abind
#'@export
CST_CategoricalEnsCombination <- function(exp, obs, cat.method = "pool", 
                                          eval.method = "leave-one-out", 
                                          amt.cat = 3, 
                                          ...) {
  # Check 's2dv_cube'
  if (!inherits(exp, "s2dv_cube") || !inherits(exp, "s2dv_cube")) {
    stop("Parameter 'exp' and 'obs' must be of the class 's2dv_cube', ",
         "as output by CSTools::CST_Load.")
  }
  if (as.numeric(dim(obs$data)["member"]) != 1) {
    stop("The length of the dimension 'member' in the component 'data' ",
         "of the parameter 'obs' must be equal to 1.")
  }

  names.dim.tmp <- names(dim(exp$data))
  exp$data <- CategoricalEnsCombination(fc = exp$data, obs = obs$data, 
                                        cat.method = cat.method,
                                        eval.method = eval.method, 
                                        amt.cat = amt.cat, ...)

  names.dim.tmp[which(names.dim.tmp == "member")] <- "category"
  names(dim(exp$data)) <- names.dim.tmp

  exp$data <- InsertDim(exp$data, lendim = 1, posdim = 2, name = "member")
  exp$attrs$Datasets <- c(exp$attrs$Datasets, obs$attrs$Datasets)
  exp$attrs$source_files <- c(exp$attrs$source_files, obs$attrs$source_files)
  return(exp)
}

#'Make categorical forecast based on a multi-model forecast with potential for 
#'calibrate
#'
#'@author Bert Van Schaeybroeck, \email{bertvs@meteo.be}
#'@description This function converts a multi-model ensemble forecast into a 
#'categorical forecast by giving the probability for each category. Different 
#'methods are available to combine the different ensemble forecasting models 
#'into probabilistic categorical forecasts. 
#'
#' See details in ?CST_CategoricalEnsCombination
#'@param fc A multi-dimensional array with named dimensions containing the 
#'  seasonal forecast experiment data in the element named \code{$data}. The 
#'  amount of forecasting models is equal to the size of the \code{dataset} 
#'  dimension of the data array. The amount of members per model may be 
#'  different. The  size of the \code{member} dimension of the data array is 
#'  equal to the maximum of the ensemble members among the models. Models with 
#'  smaller ensemble sizes have residual indices of \code{member} dimension in 
#'  the data array filled with NA values.
#'@param obs A multidimensional array with named dimensions containing the 
#'  observed data in the element named \code{$data}.
#'@param amt.cat Is the amount of categories. Equally-sized quantiles will be 
#'  calculated based on the amount of categories.
#'@param cat.method Method used to produce the categorical forecast, can be 
#'  either \code{pool}, \code{comb}, \code{mmw} or \code{obs}. The method pool 
#'  assumes equal weight for all ensemble members while the method comb assumes 
#'  equal weight for each model. The weighting method is descirbed in 
#'  Rajagopalan et al. (2002), Robertson et al. (2004) and Van Schaeybroeck and 
#'  Vannitsem (2019). Finally, the \code{obs} method classifies the observations
#'  into the different categories and therefore contains only 0 and 1 values. 
#'@param eval.method Is the sampling method used, can be either 
#'  \code{"in-sample"} or \code{"leave-one-out"}. Default value is the 
#'  \code{"leave-one-out"} cross validation.
#'@param ... Other parameters to be passed on to the calibration procedure.
#'
#'@return An array containing the categorical forecasts in the element called 
#'\code{$data}. The first two dimensions of the returned object are named 
#'dataset and member and are both of size one. An additional dimension named 
#'category is introduced and is of size amt.cat.
#'
#'@references Rajagopalan, B., Lall, U., & Zebiak, S. E. (2002). Categorical 
#'climate forecasts through regularization and optimal combination of multiple 
#'GCM ensembles. Monthly Weather Review, 130(7), 1792-1811.
#'@references Robertson, A. W., Lall, U., Zebiak, S. E., & Goddard, L. (2004). 
#'Improved combination of multiple atmospheric GCM ensembles for seasonal 
#'prediction. Monthly Weather Review, 132(12), 2732-2744.
#'@references Van Schaeybroeck, B., & Vannitsem, S. (2019). Postprocessing of 
#'Long-Range Forecasts. In Statistical Postprocessing of Ensemble Forecasts (pp. 267-290).
#'
#'@importFrom s2dv InsertDim
#'@import abind
#'@export
CategoricalEnsCombination <- function (fc, obs, cat.method, eval.method, amt.cat, ...) {
  
  if (!all(c("member", "sdate") %in% names(dim(fc)))) {
    stop("Parameter 'exp' must have the dimensions 'member' and 'sdate'.")
  }
  if (!all(c("sdate") %in% names(dim(obs)))) {
    stop("Parameter 'obs' must have the dimension 'sdate'.")
  }
  if (any(is.na(fc)))  {
    warning("Parameter 'exp' contains NA values.")
  }
  if (any(is.na(obs)))  {
    warning("Parameter 'obs' contains NA values.")
  }
  fc.merged <- mergedatasets(fc = fc) 
  amt.sdate = dim(fc.merged)["sdate"]
  target.dims <- c("member", "sdate")
  return.feat <- list(amt.cat = amt.cat)
  return.feat$dim <- c(amt.cat, amt.sdate)
  return.feat$name <- c("category", "sdate")
  return.feat$dim.name <- list(category = paste0("cat_", seq(1, amt.cat))
    , dimnames(fc.merged)[["sdate"]])  
  cat_fc_out <- .apply.obs.fc(obs = obs,
    fc = fc.merged,
    target.dims = target.dims,
    FUN = .cat_fc, 
    return.feat = return.feat,
    cat.method = cat.method,
    eval.method = eval.method,
    amt.cat = amt.cat,
    ...)
  return(cat_fc_out)
}

mergedatasets <- function(fc) {
  dims.tmp <- dim(fc)
  dimnames.tmp <- dimnames(fc)
  names.dim.tmp <- names(dims.tmp)
  amt.mbr <- dims.tmp["member"][]
  amt.dataset <- dims.tmp["dataset"][]  
  member.dim <- which(names.dim.tmp == "member")
  dataset.dim <- which(names.dim.tmp == "dataset")  
  fc.tmp <- comb.dims(fc, c(dataset.dim, member.dim))  
  if(is.null(dimnames.tmp[[dataset.dim]])){
	  mod.ind.name <- rep(paste0("model_",seq(1, amt.dataset)), times = amt.mbr)
  } else{
    mod.ind.name <- rep(dimnames.tmp[[dataset.dim]], times = amt.mbr)
  }
  mod.ind <- which(apply(fc.tmp, c(1), function(x) {all(!is.na(x))}))
  amt.mbr.tot <- length(mod.ind)
  fc.tmp <- asub(fc.tmp, list(mod.ind),1)
  mod.ind.name <- mod.ind.name[mod.ind]
  dim(fc.tmp) <- c(1, dim(fc.tmp))
  names(dim(fc.tmp)) <- c("dataset", "member", names.dim.tmp[-c(1, 2)])
  dimnames(fc.tmp) <- c(list(dataset = c("dataset1"), member = mod.ind.name), dimnames.tmp[-c(member.dim, dataset.dim)])
  return(fc.tmp)
}

comb.dims <- function(arr.in, dims.to.combine){
	dims.orig <- dim(arr.in)
	tmp.dexes <- seq(1, length(dims.orig))
	new.dexes <- c(tmp.dexes[dims.to.combine], tmp.dexes[-dims.to.combine])
	new.dims <- c(prod(dims.orig[dims.to.combine]), dims.orig[tmp.dexes[-dims.to.combine]])
	arr.out <- aperm(arr.in, new.dexes)
	dim(arr.out) <- new.dims 
	return(arr.out)
}

.apply.obs.fc <- function(obs, fc, target.dims, FUN, return.feat, cat.method, eval.method, amt.cat, ...){
  dimnames.tmp <- dimnames(fc)
  fc.dims.tmp <- dim(fc)
  dims.out.tmp <- return.feat$dim
  
  obs.fc <- .combine.obs.fc(obs, fc)
  names.dim <- names(dim(obs.fc))
  amt.dims <- length(names.dim)
  margin.all <- seq(1, amt.dims)
  matched.dims <- match(target.dims, names.dim)
  margin.to.use <- margin.all[-matched.dims]
  arr.out <- apply(X = obs.fc,
    MARGIN = margin.to.use,
    FUN = FUN, 
    cat.method = cat.method,
    eval.method = eval.method,
    amt.cat = amt.cat,
    ...)
  dims.tmp <- dim(arr.out)
  names.dims.tmp <- names(dim(arr.out))
  if(prod(return.feat$dim) != dims.tmp[1]){
    stop("apply.obs.fc: returned dimensions not as expected: ", prod(return.feat$dim), " and ", dims.tmp[1])
  }
  dim(arr.out) <- c(dims.out.tmp, dims.tmp[-1])
  names(dim(arr.out)) <- c(return.feat$name, names.dims.tmp[c(-1)])
  names.dim[matched.dims] <- return.feat$name
  pos <- match(names.dim, names(dim(arr.out)))
  pos_inv <- match(names(dim(arr.out)), names.dim)
  arr.out <- aperm(arr.out, pos)
  for (i.item in seq(1,length(return.feat$name))){
    dimnames.tmp[[pos_inv[i.item]]] <- return.feat$dim.name[[i.item]]
  }
  dimnames(arr.out) <- dimnames.tmp
  return(arr.out)
}


.cat_fc <- function(obs.fc, amt.cat, cat.method, eval.method) {
	
  dims.tmp=dim(obs.fc)
  amt.mbr <- dims.tmp["member"][]-1
  amt.sdate <- dims.tmp["sdate"][]
  pos <- match(c("member","sdate"), names(dims.tmp))
  obs.fc <- aperm(obs.fc, pos)
  var.obs <- asub(obs.fc, list(1),1)
  var.fc <- asub(obs.fc, list(1+seq(1, amt.mbr)),1)
  dim(var.fc) <- c(amt.mbr, amt.sdate)
  dims.fc <- dim(var.fc)
  
  
  mdl.dimnames <- dimnames(obs.fc)[["member"]][-1]
  mdl.feat <- .get.mdl.features(mdl.dimnames)
  amt.mdl <- mdl.feat$amt.mdl
  amt.coeff <- amt.mdl + 1
  var.cat.fc <- array(NA, c(amt.cat, amt.sdate))
  
  eval.train.dexeses <- .make.eval.train.dexes(eval.method = eval.method, amt.points = amt.sdate)
  amt.resamples <- length(eval.train.dexeses)	
  
  for (i.sample in seq(1, amt.resamples)) {
    # defining training (tr) and evaluation (ev) subsets 
    eval.dexes <- eval.train.dexeses[[i.sample]]$eval.dexes
    train.dexes <- eval.train.dexeses[[i.sample]]$train.dexes
    
    fc.ev <- var.fc[ , eval.dexes, drop = FALSE]
    fc.tr <- var.fc[ , train.dexes, drop = FALSE]
    obs.tr <- var.obs[train.dexes , drop = FALSE]
    obs.ev <- var.obs[eval.dexes , drop = FALSE] 
    
    amt.sdate.tr <- dim(fc.tr)[2]
    amt.sdate.ev <- dim(fc.ev)[2]
    
    quant.to.use <- .calc.quantiles(obs.tr, amt.cat)
    cat.fc.ev <- .calc.cat(fc.ev, quant.to.use)
    cat.obs.ev <- .calc.cat(obs.ev, quant.to.use)
    
    if(cat.method == "mmw"){
    
      cat.fc.tr <- .calc.cat(fc.tr, quant.to.use)
      cat.obs.tr <- .calc.cat(obs.tr, quant.to.use)
      
      freq.per.mdl.at.obs <- .calc.freq.per.mdl.at.obs(cat.obs = cat.obs.tr, cat.fc = cat.fc.tr,
        mdl.feat = mdl.feat, amt.cat = amt.cat)
      freq.per.mdl.ev <- .calc.freq.per.mdl(cat.fc = cat.fc.ev, mdl.feat = mdl.feat, amt.cat = amt.cat)
      
      if(i.sample == 1){
        init.par <- c(rep(1 / amt.coeff, amt.coeff)) * 0.999
      }
      #calculate weights on training dataset
      constr.mtrx <- array(0, c(amt.coeff + 1, amt.coeff))
      for (i.coeff in seq(1, amt.coeff)){
        constr.mtrx[i.coeff, i.coeff] <- 1.
      }
      constr.mtrx[amt.coeff + 1, ] <- -1.
      constr.vec <- c(rep(0., amt.coeff), -1.)
      optim.tmp <- constrOptim(theta = init.par, f = .funct.optim, grad = .funct.optim.grad, 
                          ui = constr.mtrx, ci = constr.vec,
                          freq.per.mdl.at.obs = freq.per.mdl.at.obs) 
      init.par <- optim.tmp$par * (1 - abs(rnorm(amt.coeff, 0, 0.01)))
      var.cat.fc[ , eval.dexes] <- apply(suppressWarnings(InsertDim( 
        InsertDim(optim.tmp$par, lendim = amt.cat, posdim = 2),
        lendim =  amt.sdate.ev, posdim = 3)) *
        freq.per.mdl.ev[ , , , drop = FALSE], c(2,3), sum, na.rm = TRUE)
    } else if (cat.method == "comb") {
      freq.per.mdl.ev <- .calc.freq.per.mdl(cat.fc = cat.fc.ev, mdl.feat = mdl.feat, amt.cat = amt.cat)
      var.cat.fc[ , eval.dexes] <- apply(freq.per.mdl.ev[-amt.coeff, , , drop = FALSE], c(2, 3), mean, na.rm = TRUE)
    } else if (cat.method == "pool") {
      freq.per.mdl.ev <- .calc.freq.per.mdl(cat.fc = cat.fc.ev, mdl.feat = NULL, amt.cat = amt.cat)
      var.cat.fc[ , eval.dexes] <- freq.per.mdl.ev[1, , ]
    } else if (cat.method == "obs") {
      dim(cat.obs.ev) <- c(1,length(cat.obs.ev))
      freq.per.mdl.ev <- .calc.freq.per.mdl(cat.fc = cat.obs.ev, mdl.feat = NULL, amt.cat = amt.cat)
      var.cat.fc[ , eval.dexes] <- freq.per.mdl.ev[1, , ]
    }
  }
  names(dim(var.cat.fc)) <- c("member", "sdate")
  return(var.cat.fc)
}

.calc.percentiles <- function(data, amt.cat){
  if(amt.cat < 2){
    stop("amount of categories too low: ", amt.cat)
  }
  frac.to.use <- 1./amt.cat
  perc.out <- seq(from = frac.to.use, to = 1. - frac.to.use, by = frac.to.use)
  return(perc.out)
}

.calc.quantiles <- function(data, amt.cat){
  perc.to.use <- .calc.percentiles(data, amt.cat)
  
  quant.out <- quantile(data, perc.to.use, na.rm = T)
  if(any(duplicated(quant.out))){
	stop(paste0("The ",amt.cat," ( = amt.cat) different quantile categories are", 
	  " determined based on the observation data. However, ", length(data),
	  " datapoints are insufficient to determine the quantiles. Please", 
	  " reduce the amount of categories or extend observational dataset."))
  }
  return(quant.out)
}

.calc.cat <- function(data, quant){
  quant <- c(-Inf,quant,Inf)
  categ.all <- cut(as.vector(data), breaks = quant, labels = FALSE)
  
  if(!is.null(dim(data))){
    dim(categ.all) <- dim(data)
  }
  return(categ.all)
}

.get.mdl.features <- function(mdl.names){
  amt.mbr <- length(mdl.names)
  mdl.diff.names <- unique(mdl.names)
  amt.mdl <- length(mdl.diff.names)
  mdl.msk <- array(F,c(amt.mdl, amt.mbr))
  amt.mbr.per.mdl <- array(0, c(amt.mdl))
  if(amt.mdl == 1 & amt.mbr == 1) {
    mdl.msk = array(T, c(1, 1))
    amt.mbr.per.mdl <- array(1, c(1, 1))
  } else {
    mdl.msk <- t(sapply(mdl.diff.names, function(x){mdl.names == x}))
    amt.mbr.per.mdl <- apply(mdl.msk, c(1), sum,na.rm = TRUE)
  }
  return(list(amt.mbr = amt.mbr, amt.mdl = amt.mdl, 
    amt.mbr.per.mdl = amt.mbr.per.mdl, mdl.msk = mdl.msk))
}

.combine.obs.fc <- function(obs,fc){
  names.dim.tmp <- names(dim(obs))
  members.dim <- which(names.dim.tmp == "member")
  arr.out <- abind(obs, fc, along = members.dim)
  dimnames.tmp <- dimnames(arr.out)
  names(dim(arr.out)) <- names.dim.tmp
  dimnames(arr.out) <- dimnames.tmp
  names(dimnames(arr.out)) <- names.dim.tmp
  return(arr.out)
}

.funct.optim <- function(par, freq.per.mdl.at.obs){
  return(-mean(log(drop(par %*% freq.per.mdl.at.obs)), na.rm = TRUE))
}

.funct.optim.grad <- function(par, freq.per.mdl.at.obs){
  amt.model <- dim(freq.per.mdl.at.obs)[1]
  preprocess <- drop(par %*% freq.per.mdl.at.obs)
  if (is.null(dim(preprocess))) {
    dim(preprocess) <- c(dim = length(preprocess))
  }
  return(-apply(freq.per.mdl.at.obs/suppressWarnings(InsertDim(preprocess,
          lendim = as.numeric(amt.model), posdim = 1)), c(1), mean, na.rm = TRUE))
}

.calc.freq.per.mdl.at.obs <- function(cat.obs, cat.fc, amt.cat, mdl.feat){
  amt.mbr <- dim(cat.fc)[1]
  amt.sdate <- dim(cat.fc)[2]
  amt.mdl <- mdl.feat$amt.mdl
  mdl.msk.tmp <- mdl.feat$mdl.msk
  amt.coeff <- amt.mdl + 1
  msk.fc.obs <- (cat.fc == InsertDim(cat.obs, posdim = 1, lendim = amt.mbr))
  freq.per.mdl.at.obs <- array(NA, c(amt.coeff, amt.sdate))
  for (i.mdl in seq(1, amt.mdl)){
    freq.per.mdl.at.obs[i.mdl, ] <- apply(msk.fc.obs[mdl.msk.tmp[i.mdl, ], , drop = FALSE],
      c(2), mean, na.rm = TRUE)
  }
  freq.per.mdl.at.obs[amt.coeff, ] = 1 / amt.cat
  return(freq.per.mdl.at.obs)
}

.calc.freq.per.mdl <- function(cat.fc, amt.cat, mdl.feat){
  amt.mbr <- dim(cat.fc)[1]
  amt.sdate <- dim(cat.fc)[2]
  if(is.null(mdl.feat)){
    amt.mdl <- 1
    mdl.msk.tmp <- array(T, c(1, amt.mbr))
  } else {
    amt.mdl <- mdl.feat$amt.mdl
    mdl.msk.tmp <- mdl.feat$mdl.msk
  }
  amt.coeff <- 1 + amt.mdl
  freq.per.mdl <- array(NA, c(amt.coeff, amt.cat, amt.sdate))
  for (i.mdl in seq(1, amt.mdl)){
    ens.mdl.msk <- mdl.msk.tmp[i.mdl, , drop = FALSE]
    for (i.cat in seq(1, amt.cat)){
      freq.per.mdl[i.mdl, i.cat, ] <- apply(i.cat==cat.fc[ens.mdl.msk, , drop = F],
        c(2), mean, na.rm = TRUE)
    }
  }
  freq.per.mdl[amt.coeff, , ] =  1 / amt.cat
  return(freq.per.mdl)
  
}