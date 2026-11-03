##
coreChecks <- function(betaTruth,
											 X,
											 N,
											 etaTerms,
											 expandX,
											 betaNames)
{
	##
	if(ncol(as.matrix(X)) == 1)
    return("* 'X' should have at least two columns")
	
	##
	if(sum(X[,1] != 1) > 0)
  	return("* 'X' requires the first column to represent the intercept")
  
	##
	if(expandX != "none")
	{
		##
		if(expandX == "all")
			colIndex <- 1:ncol(X)
		if(expandX != "all")
			colIndex <- c(1:ncol(X))[is.element(colnames(X), expandX)]
		##
		for(i in 2:ncol(X))
		{
			if(is.element(i, colIndex))
			{
				prob1 <- sum(ceiling(X[,i]) != floor(X[,i]))
				prob2 <- (min(X[,i]) != 0)
				prob3 <- (max(X[,i]) != (length(unique(X[,i])) - 1))
				prob4 <- (max(X[,i]) == 0)
				if((prob1 + prob2 + prob3 + prob4) > 0)
					return("* check that each variable is consistent with the {0,1,2,...} coding convention")
			}
		}
	}
	
  ##
  if(length(N) != nrow(X))
    return("* incompatible dimensions of 'X' and 'N'")
	
	##
	Xtemp <- X
	if(!is.null(etaTerms))
	{
		##
		cat("\nWARNING: Make sure that the elements of 'etaTerms' are in the same order as those of 'betaTruth'\n\n")
		##
		if(sum(!is.element(etaTerms, colnames(X))) > 0)
			return("* elements of 'etaTerms' are not in the design matrix 'X'")
		Xtemp <- X[, is.element(colnames(X), etaTerms)]
	}
	
  ##
  if(expandX == "all")  p <- sum(unlist(lapply(apply(Xtemp, 2, unique), FUN=length)) - 1) + 1
	if(expandX != "all") p <- ncol(Xtemp)
	if(length(betaTruth) != p)
		return("* invalid dimension of 'betaTruth'")
	  
	##
	if(!is.null(betaNames))
	{
		if(length(betaTruth) != length(betaNames))
			return("* 'betaTruth' and 'betaNames' are not of the same length")
		if(!is.character(betaNames))
			return("* elements of 'betaNames' are not character")
	}

	##
	return("")
}


##
ccChecks <- function(nCC,
										 threshold=NULL)
{
	##
	if(min(nCC) < 0 & length(nCC) == 1)
		return("* Case-control sample size 'nCC' is negative")
	if(min(nCC) < 0 & length(nCC) > 1)
		return("* At least one case-control sample size 'nCC' is negative")
	
	##
	if(!is.null(threshold))
	{
		if(length(threshold) != 2)
			return("* 'threshold' is not a pair of numbers")
	}
  
	##
	return("")
}


##
tpsChecks <- function(X,
										  strata,
											nII,
											cohort,
											NI,
											nII0=NULL,
											nII1=NULL,
											nCC=NULL,
											threshold=NULL)
{
  ##
  if(!is.list(strata))
  {
  	##
  	if(sum(!is.element(strata, 0:ncol(X))) > 0)
  		return("* 'strata' is invalid")
		##
  	if(max(strata) == 0)
  	{
  		if(is.null(nII))
  			return("* 'nII' is required when strata == 0")
			if(!is.null(nII0))
  			print("* Warning: argument 'nII0' is ignored when strata == 0")
			if(!is.null(nII1))
  			print("* Warning: argument 'nII1' is ignored when strata == 0")
  	}
	  ##
  	if(max(strata) > 0)
  	{
  		if(is.null(nII) & (is.null(nII0) | is.null(nII1)))
  			return("* Require valid phase II sample sizes: (i) 'nII' or (ii) 'nII0' and 'nII1'")
			if(is.element(0, strata))
  			print("* Warning: ignoring strata == 0")
  	}
  }

	##
  if(!is.null(nII))
  {
		if(min(nII) < 0 & length(nII) == 1)
			return("* Phase II sample size 'nII' is negative")
		if(min(nII) < 0 & length(nII) > 1)
			return("* At least one phase II sample size 'nII' is negative")
  }
  
	##
 	if(cohort == FALSE)
 	{
 		if(is.null(NI))
  		return("* 'NI' must be specified if phase I arises via case-control sampling")
	  if(!is.null(NI))
  	{
  		if(length(NI) != 2)
  			return("* 'NI' should be a pair of Phase I sample sizes for controls and cases")
	  	if(min(NI) < 0)
  			return("* Phase I case-control sample size 'NI' is not positive")
  	}
 	}

	##
  if(!is.null(nCC))
  {
		if(min(nCC) < 0 & length(nCC) == 1)
			return("* Case-control sample size 'nCC' is negative")
		if(min(nCC) < 0 & length(nCC) > 1)
			return("* At least one case-control sample size 'nCC' is negative")
  }

 	##
	if(!is.null(threshold))
	{
		if(length(threshold) != 2)
			return("* 'threshold' is not a pair of numbers")
	}
	
	##
	return("")
}


##
phaseIChecks <- function(X,
										     strata,
											   cohort,
											   NI,
											   nII0=NULL,
											   nII1=NULL)
{
  ##
  if(sum(!is.element(strata, 1:ncol(X))) > 0)
  	return("* 'strata' is invalid")

	##
 	if(cohort == FALSE)
 	{
 		if(is.null(NI))
  		return("* 'NI' must be specified if phase I arises via case-control sampling")
	  if(!is.null(NI))
  	{
  		if(length(NI) != 2)
  			return("* 'NI' should be a pair of Phase I sample sizes for controls and cases")
	  	if(min(NI) < 0)
  			return("* Phase I case-control sample size 'NI' is not positive")
  	}
 	}

	##
	return("")
}
