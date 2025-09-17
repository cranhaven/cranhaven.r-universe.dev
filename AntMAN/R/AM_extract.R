#######################################################################################
###############
############### AntMAN Package
###############
###############
#######################################################################################


## INTERNAL
generate_column_names = function(target, r) {
	values = NULL;
	
	if (length(r) == 1) {
		
		values = append( values ,  sprintf ( "%s", target ) );
	
	} else {
	
		for (i in r) {
			values = append( values ,  sprintf ( "%s_%d", target , i) );
		}	
		
	}
	return (values);
}




## INTERNAL
extract_target = function(fit, target, iterations = NULL, debug = FALSE){
	
	result = NULL
	
	mainpath = strsplit(target, "_")[[1]]
	if (debug) message("path = ", paste(mainpath,collapse = " "), "\n");
	
	if (length(mainpath) == 0) {
		warning("ERROR Invalid variable name(too small)\n");
		return (NULL);
	}
	
	variable_name = mainpath[1];
	
	if (debug) message(" - variable_name = ", variable_name, "\n");
	
	if (!is.element(variable_name , names(fit))) {
		warning("ERROR Invalid variable name '",variable_name, "' has not been found)\n");
		return (NULL);
	}
	
	# level 1 - get variable
	variable = fit[[variable_name]];
	
	if (debug) message(" - variable taken, class = ",class(variable),", length = ",length(variable),"\n");
	
	explored_iterations = iterations
	if (is.null(explored_iterations)) {
		explored_iterations = c(1:length(variable));
	}
	
	for (iter in explored_iterations) {
		
		path = mainpath[-1];
		
		# level 2 - get iteration
		values = variable[[iter]];
		
		# level 3 - get named_index if list
		
		
		if (is.numeric(values) || is.integer(values)) {
			
			# path can only be a numerical index / we skip
			if (debug) message("   - This is already numerical, we don't need to go down\n")
			
		} else  if (is.list(values))  {
			
			# path is a list we can access an element
			if (debug) message("   - This is a list:", paste(names(values), collapse = " "), "\n")
			
			if (length(path) == 0) {
				warning("ERROR Invalid variable name(too small)\n");
				return (NULL);
			} 
			
			subitem = path[1];
			path = path[-1];
			
			if (!is.element(subitem , names(values))) {
				warning("ERROR: Cannot find subitem '",subitem,"' among [", paste(names(values), collapse=" "),"]\n")
				return (NULL);
			} else {
				if (debug) message("   - We access subitem: ", subitem, "\n")
				values = values[[subitem]];
			}
			
		} else {
			warning("ERROR: UNSUPPORTED TYPE " ,class(values) ," \n");
			return (NULL);
		}
		
		
		if (is.numeric(values) || is.integer(values)) {
			if (debug) message("   - lower level can be handle\n");
		} else {
			warning("ERROR: UNSUPPORTED TYPE " ,class(values) ," \n");
			return (NULL);
		}
		
		# now we have values a numerical value or array. 
		# we flatten it or we get what the index requires.
		if (length(path) == 0) {
			if (debug) message("   - We need to flatten\n");
			# we flatten 
		} else {
			# get the one element selected
			if (debug) message("   - We get the index\n");
			value_index = strtoi(path[1]);
			if (debug) message("   - value_index = " , value_index ,  "\n");
			values = values[value_index];
		}
		
		values = as.vector(unlist(values))
		
		if (is.null(result)) {
			result = data.frame(t(values))
			names(result) <- generate_column_names(target,c(1:length(values)))
		} else {
			
			## We add missing column on the new row
			if (ncol(result) > length(values)) {
				values = c( values , rep(NA, ncol(result) - length(values))); 
			}
			
			
			##  We add missing column on the original dataframe
			if ((ncol(result) < length(values))) {
				while (ncol(result) < length(values)) { ## TODO : Please find more efficient!
					result = cbind(result,c(NA))
				}
			
				names(result) <- generate_column_names(target,c(1:length(values)))
			}
		
			if (ncol(result) != length(values)) { ## SHOULD NEVER HAPPEND!
				warning("ERROR: NUMBER of COLUMN CHANGED FROM " ,ncol(result) , " to ", length(values)," \n");
				return (NULL);
			} else {
				result = rbind(result, values);
			}
		}
	}
	return (result)
}



#'  Extract values within a \code{\link{AM_mcmc_output}} object
#' 
#'  Given an \code{\link{AM_mcmc_output}} object, as well as the target variable names, 
#'  AM_extract will return a list of the variables of interest.
#'
#'  Due to the complexity of AntMAN outputs, \code{\link{AM_mcmc_output}} object can be difficult
#'  to handle. The AM_extract function eases access of particular variables within the
#'  \code{\link{AM_mcmc_output}} object. Variables of varying dimension are expected to result from the transdimensional moves. When considering such
#'  variables, the extracted list would correspond to an nx1 list, where n refers to the number of extracted iterations. Each of these nx1 entries consists
#'  of another list of dimension mx1, where m specifies the number of components inferred for that iteration.
#'  
#'@param object an \code{\link{AM_mcmc_output}} object.
#'@param targets List of variables to extract (ie. K, M, mu).
#'@param iterations Can specify particular iterations to extracts, NULL for all.
#'@param debug Activate log to. 
#'@return a list of variables specified in \code{targets}.
#'  
#'@export
AM_extract = function(object, targets, iterations = NULL, debug = FALSE){
	
	
	df = NULL;
	for (target in targets) {
		
		if (target == "CI") {
			## CI Extractor
			nrows = length(object$CI)
			ncols = length(object$CI[[1]])
			tmp = data.frame(t(array(as.numeric(unlist(object$CI)), dim=c(ncols,nrows))))
			names(tmp) <- generate_column_names(target,c(1:ncols));
			if (!is.null(iterations)) {
				tmp = tmp[iterations,];
			}
		}
		#TODO: working but not elegant
		if (target == "mu" || target == "sig2" || target == "Sig" || target == "theta" || target == "W"){
			tmp = as.matrix(object[[target]])
			if (!is.null(iterations)){
				tmp = as.matrix(tmp[iterations,])
			}
		}

		 else {
			## Generic extractor (SLOW)
			tmp = as.matrix(extract_target(object,target,iterations,debug));
		}
		
		
		if (is.null(tmp)) {
			warning("ERROR: Invalid extraction target: ",target,", please make sure this was part of the outputs list in AM_mcmc_parameters.\n", sep="");
			return (NULL);
		}
		
		if(is.null(df)) {
			nrows = nrow(tmp)
			df = list();
		} else {
			if (nrow(tmp) != nrows) {
				warning("ERROR: Invalid extraction size, previously found ",nrow(df),"while with target '",target,"' we have ", nrow(tmp),"\n", sep="");
				return (NULL);
			}
		}
		df[[target]] = tmp;
	}
	return (df);
	
	
	
}
