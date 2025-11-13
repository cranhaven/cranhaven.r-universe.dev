catpredi.survival <-
function(formula, cat.var, cat.points = 1, data, method = c("addfor","genetic","backaddfor"), conc.index = c("cindex","cpe"), range = NULL, correct.index = FALSE, control = controlcatpredi.survival(), ...) {
	control <- do.call("controlcatpredi.survival", control)

	if(missing(formula)) {
		stop("Argument \"formula\" is missing, with no default")
	}
	if(is.character(formula))
		formula = as.formula(formula)
	if(missing(data)) {
		stop("Argument \"data\" is missing, with no default")
	}
	if(missing(cat.var)) {
		stop("Argument \"cat.var\" is missing, with no default")
	}
	var.names <- c(all.vars(formula), cat.var)
	if(!all(var.names %in% names(data))) {
		stop("Not all needed variables are supplied in \"data\"")
	}
	data.res <- na.omit(data[,var.names])
	unique.resp <- unique(data.res[,var.names[2]])
	if(length(unique.resp) != 2 || !is.numeric(unique.resp) || !all(unique.resp %in% c(0,1))) {
		stop("The event status indicator should be numeric and codified as 0 (censored) and 1 (event at time)")
	}
	## COMO PODEMOS ASEGURAR QUE SEA UN OBJETO DE TIPO SURV??	
	method <- match.arg(method)
	conc.index <- match.arg(conc.index)
	
	if(is.null(range)) {
		range <- range(data.res[,cat.var])
	}
	# Call the methods
	if(method == "addfor" & conc.index == "cindex") {
		res <- k.points.max.cind(formula = formula, cat.var = cat.var, data = data.res, range = range, k = cat.points, l.s.points = control$grid, min.p.cat = control$min.p.cat)
		cutpoints <- res[,1]
		Cindex = res[,2]
		# Correct the C-index  
		if(correct.index == TRUE) {
			Cindex.cor <- cindex.opt.corrected(formula = formula, cat.var =  cat.var , data = data.res , c.points = cutpoints , cindex = Cindex[length(cutpoints)] , B=control$B, b.method = control$b.method )   
	 	} else {
			Cindex.cor <- NULL
		}
	} else if(method == "addfor" & conc.index == "cpe") {  
		res <- k.points.max.cpe(formula = formula, cat.var = cat.var, data = data.res, range = range, k = cat.points, l.s.points = control$grid, min.p.cat = control$min.p.cat)
		cutpoints <- res[,1]
		CPE = res[,2]
		# Correct the CPE 
		if(correct.index == TRUE) {
			CPE.cor <- cpe.opt.corrected(formula = formula, cat.var =  cat.var , data = data.res , c.points = cutpoints , cpe = CPE[length(cutpoints)] , B=control$B , b.method = control$b.method) 
		} else {
			CPE.cor <- NULL
		} 
	} else if(method == "genetic" & conc.index == "cindex") { 
		Dim <- matrix(ncol = 2, nrow = cat.points)
		Dim[,1] = range[1]*1.0
		Dim[,2] = range[2]*1.0
		res <- rgenoud::genoud(calculate.cind, cat.points, max = TRUE, formula = formula, cat.var = cat.var, data.f = data.res, range = range, min.p.cat = control$min.p.cat, Domains = Dim, print.level = control$print.gen, ...)
		cutpoints <- res$par
		Cindex = res$value
		# Correct the C-index   
		if(correct.index == TRUE) {
			Cindex.cor <- cindex.opt.corrected(formula = formula, cat.var =  cat.var , data = data.res , c.points = cutpoints , cindex = Cindex , B=control$B, b.method = control$b.method) 	
		} else {
			Cindex.cor <- NULL
		}  
	} else if(method == "genetic" & conc.index == "cpe"){
		Dim <- matrix(ncol = 2, nrow = cat.points)
		Dim[,1] = range[1]*1.0
		Dim[,2] = range[2]*1.0
		res <- rgenoud::genoud(calculate.CPE, cat.points, max = TRUE, formula = formula, cat.var = cat.var, data.f = data.res, range = range, min.p.cat = control$min.p.cat, Domains = Dim, print.level = control$print.gen, ...)
		cutpoints <- res$par
		CPE = res$value
		# Correct the CPE   
		if(correct.index == TRUE) {
			CPE.cor <- cpe.opt.corrected(formula = formula, cat.var =  cat.var , data = data.res , c.points = cutpoints , cpe = CPE , B=control$B , b.method = control$b.method) 	
		} else {
			CPE.cor <- NULL
		}
	}  else if(method == "backaddfor" & conc.index == "cindex") { 
	  Dim <- matrix(ncol = 2, nrow = cat.points)
	  Dim[,1] = range[1]*1.0
	  Dim[,2] = range[2]*1.0
	  res <- backaddfor.cind(formula = formula, cat.var = cat.var, data = data.res, range = range, k = cat.points, l.s.points = control$grid, min.p.cat = control$min.p.cat, eps = control$eps, repmax = control$B)
	  # res <- rgenoud::genoud(calculate.cind, cat.points, max = TRUE, formula = formula, cat.var = cat.var, data.f = data.res, range = range, min.p.cat = control$min.p.cat, Domains = Dim, print.level = control$print.gen, ...)
	  cutpoints <- res$cuts
	  Cindex = res$cind 
	  # Correct the C-index   
	  if(correct.index == TRUE) {
	    Cindex.cor <- cindex.opt.corrected(formula = formula, cat.var =  cat.var , data = data.res , c.points = cutpoints , cindex = Cindex , B=control$B, b.method = control$b.method) 	
	  } else {
	    Cindex.cor <- NULL
	  }  
	} else { #if(method == "backaddfor" & conc.index == "cpe"){
	  Dim <- matrix(ncol = 2, nrow = cat.points)
	  Dim[,1] = range[1]*1.0
	  Dim[,2] = range[2]*1.0
	  res <- backaddfor(formula = formula, cat.var = cat.var, data = data.res, range = range, k = cat.points, l.s.points = control$grid, min.p.cat = control$min.p.cat, eps = control$eps, repmax = control$B, ...)
	  # res <- rgenoud::genoud(calculate.CPE, cat.points, max = TRUE, formula = formula, cat.var = cat.var, data.f = data.res, range = range, min.p.cat = control$min.p.cat, Domains = Dim, print.level = control$print.gen, ...)
	  cutpoints <- res$cuts
	  CPE = res$auc
	  # Correct the CPE   
	  if(correct.index == TRUE) {
	    CPE.cor <- cpe.opt.corrected(formula = formula, cat.var =  cat.var , data = data.res , c.points = cutpoints , cpe = CPE , B=control$B , b.method = control$b.method) 	
	  } else {
	    CPE.cor <- NULL
	  }
	}
	# Create the categorical covariate
	data[,paste0(cat.var,"_CatPredi")] <- cut(data[,cat.var], sort(unique(c(max(data[,cat.var]), min(data[,cat.var]), cutpoints))), include.lowest = TRUE, right = TRUE)
	results <- if(method == "addfor" & conc.index == "cindex" & correct.index == TRUE ) {
					list(cutpoints = cutpoints, Cindex = Cindex, Cindex.cor = Cindex.cor,  grid = control$grid)
				} else if(method == "addfor" & conc.index == "cpe" & correct.index == TRUE) {
					list(cutpoints = cutpoints, CPE = CPE, CPE.cor = CPE.cor,  grid = control$grid)
				} else if(method == "genetic" & conc.index == "cindex" & correct.index == TRUE) { 
					list(cutpoints = cutpoints, Cindex = Cindex, Cindex.cor = Cindex.cor)
				} else if(method == "genetic" & conc.index == "cpe" & correct.index == TRUE) {
					list(cutpoints = cutpoints, CPE = CPE, CPE.cor = CPE.cor)
				} else if(method == "backaddfor" & conc.index == "cindex" & correct.index == TRUE) { 
				  list(cutpoints = cutpoints, Cindex = Cindex, Cindex.cor = Cindex.cor)
				} else if(method == "backaddfor" & conc.index == "cpe" & correct.index == TRUE) {
				  list(cutpoints = cutpoints, CPE = CPE, CPE.cor = CPE.cor)
				}else if(method == "addfor" & conc.index == "cindex" & correct.index == FALSE ) {
					list(cutpoints = cutpoints, Cindex = Cindex, grid = control$grid)
				} else if(method == "addfor" & conc.index == "cpe" & correct.index == FALSE) {
					list(cutpoints = cutpoints, CPE = CPE, grid = control$grid)
				} else if(method == "genetic" & conc.index == "cindex" & correct.index == FALSE) { 
					list(cutpoints = cutpoints, Cindex = Cindex)
				} else if (method == "genetic" & conc.index == "cpe" & correct.index == FALSE){
					list(cutpoints = cutpoints, CPE = CPE)
				} else if(method == "backaddfor" & conc.index == "cindex" & correct.index == FALSE) { 
				  list(cutpoints = cutpoints, Cindex = Cindex)
				} else { #if (method == "backaddfor" & conc.index == "cpe" & correct.index == FALSE){
				  list(cutpoints = cutpoints, CPE = CPE)
				}		
	res <- list(call = match.call(), method = method, conc.index = conc.index, formula = formula, cat.var = cat.var, data = data, correct.index = correct.index, results = results, control =  control)
	class(res) <- "catpredi.survival"
	res
}
