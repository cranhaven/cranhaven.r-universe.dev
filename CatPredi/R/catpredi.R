catpredi <-
function(formula, cat.var, cat.points = 1, data, method = c("addfor","genetic","backaddfor"), range = NULL, correct.AUC = FALSE, control = controlcatpredi(), ...) {
	control <- do.call("controlcatpredi", control)

	if(missing(formula)) {
		stop("Argument \"formula\" is missing, with no default")
	}
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
	unique.resp <- unique(data.res[,var.names[1]])
	if(length(unique.resp) != 2 || !is.numeric(unique.resp) || !all(unique.resp %in% c(0,1))) {
		stop("The response variable should be numeric and codified as 0 (healthy) and 1 (diseased)")
	}
	method <- match.arg(method)
	if(is.null(range)) {
		range <- range(data.res[,cat.var])
	}
	# Call the methods
	if(method == "addfor") {
		res <- k.points.max.auc(formula = formula, cat.var = cat.var, data = data.res, range = range, k = cat.points, l.s.points = control$grid, min.p.cat = control$min.p.cat)
		cutpoints <- res[,1]
		AUC = res[,2]
		
		if(correct.AUC == TRUE) {
			AUC.cor <- auc.opt.corrected(formula = formula, cat.var = cat.var, data = data.res , c.points = cutpoints, AUC = AUC[length(cutpoints)], B=control$B , b.method = control$b.method)	
	 	} else {
			AUC.cor <- NULL
	 	}  
	} else if(method == "genetic"){
		Dim <- matrix(ncol = 2, nrow = cat.points)
		Dim[,1] = range[1]*1.0
		Dim[,2] = range[2]*1.0
		res <- rgenoud::genoud(calculate.AUC, cat.points, max = TRUE, formula = formula, cat.var = cat.var, data.f = data.res, range = range, min.p.cat = control$min.p.cat, Domains = Dim, print.level = control$print.gen, ...)
		cutpoints <- res$par
		AUC = res$value
		  
		if(correct.AUC == TRUE) {
			AUC.cor <- auc.opt.corrected(formula = formula, cat.var = cat.var, data = data.res , c.points = cutpoints, AUC = AUC, B=control$B , b.method = control$b.method)	
		} else {
			AUC.cor <- NULL
		}
	} else { # method == "backaddfor"
	  res <- backaddfor(formula = formula, cat.var = cat.var, data = data.res, range = range, k = cat.points, l.s.points = control$grid, min.p.cat = control$min.p.cat, eps = control$eps, repmax = control$B, ...)
	  cutpoints <- res$cuts
	  AUC = res$auc
	  
	  if(correct.AUC == TRUE) {
	    AUC.cor <- auc.opt.corrected(formula = formula, cat.var = cat.var, data = data.res , c.points = cutpoints, AUC = AUC, B=control$B , b.method = control$b.method)	
	  } else {
	    AUC.cor <- NULL
	  }  
	}  
	# Create the categorical covariate
	data[,paste0(cat.var,"_CatPredi")] <- cut(data[,cat.var], sort(unique(c(max(data[,cat.var], na.rm=TRUE), min(data[,cat.var], na.rm=TRUE), cutpoints))), include.lowest = TRUE, right = TRUE)
	results <- if(method == "addfor" & correct.AUC == TRUE) {
				list(cutpoints = cutpoints, AUC = AUC, AUC.cor = AUC.cor,  grid = control$grid)
			} else if(method == "genetic" & correct.AUC == TRUE) {
				list(cutpoints = cutpoints, AUC = AUC, AUC.cor = AUC.cor)
			} else if(method == "backaddfor" & correct.AUC == TRUE){
			  list(cutpoints = cutpoints, AUC = AUC, AUC.cor = AUC.cor,  grid = control$grid )
			} else if(method == "addfor" & correct.AUC == FALSE) {
				list(cutpoints = cutpoints, AUC = AUC, grid = control$grid)
			} else if(method == "genetic" & correct.AUC == FALSE){
			  list(cutpoints = cutpoints, AUC = AUC)
			} else{ # method == "backaddfor" & correct.AUC == FALSE 
			  list(cutpoints = cutpoints, AUC = AUC, grid = control$grid)
			}
		
	res <- list(call = match.call(), method = method, formula = formula, cat.var = cat.var, data = data, correct.AUC = correct.AUC, results = results , control = control)
	class(res) <- "catpredi"
	res
}
