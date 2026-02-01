#-------------------------------------------------------------------------------#
# Package: Network-Based Genome-Wide Association Studies                        #
# Author: Pariya Behrouzi                                                       #
# Emails: <pariya.Behrouzi@gmail.com>                                           #
# Date: Nov 21th 2017                                                           #
#-------------------------------------------------------------------------------#
model.selection = function( result, criterion, lower.upper, ebic.gamma = 0.5,  ncores = 1, loglik_Y=TRUE, verbose = TRUE)
{
	#if(class(result) == "netgwas") 
	if( inherits(result, "netgwas") )  
	{
		p	 = ncol(result$path[[1]])
		nrho = length(result$rho)
		n	 = nrow(result$data)

	}else{
		stop("netgwas.object should belong to the netgwas class. \n ")
	} 
	loglik = result$loglik
	
	result$df <- sapply(1:nrho, function(x) sum(as.matrix(result$Theta[[x]])[upper.tri(result$Theta[[x]])] != 0 ))
	if (criterion == "ebic")
	{
		if(verbose)
		{
			cat("Calculating extended Bayesian information criterion (ebic) selection ...")
			flush.console()
		}
		result$ebic.scores	= - 2 * loglik + ( log(n) * result$df ) + ( 4 * ebic.gamma * log(p) * result$df )
		result$opt.index	= which.min(result$ebic.scores)
		result$opt.theta	= result$Theta[[result$opt.index]]
		colnames(result$opt.theta) = colnames(result$data)
		result$opt.adj		= abs(sign(as.matrix(result$opt.theta))) - diag(rep(1,p))
		colnames(result$opt.adj) = colnames(result$opt.theta)
		result$opt.rho		= result$rho[result$opt.index]
	}
	
	if (criterion == "aic")
	{
		if(verbose)
		{
			cat("Calculating Akaike information criterion (AIC) selection ...")
			flush.console()
		}
		 
		result$aic.scores	= ( - 2 * loglik ) + ( 2 * result$df )
		result$opt.index	= which.min(result$aic.scores)
		result$opt.theta	= result$Theta[[result$opt.index]]
		colnames(result$opt.theta) = colnames(result$data)
		result$opt.adj		= abs(sign(result$opt.theta)) - diag(rep(1,p))
		colnames(result$opt.adj) = colnames(result$opt.theta)
		result$opt.rho		= result$rho[result$opt.index]
	}

	if( sum(result$opt.adj ) < p - 2 )
	{
		ind <- unlist(lapply(1:length(result$rho), function(i) sum(result$adj[[i]])))
		result$opt.index	= which(ind >  ncol(result$data))[1] + 1 
		if( (is.na(result$opt.index)) || (result$opt.index > length(result$rho))) result$opt.index = length(result$rho)
		result$opt.theta	= result$Theta[[result$opt.index]]
		colnames(result$opt.theta) = colnames(result$data)
		result$opt.adj		= abs(sign(result$opt.theta)) - diag(rep(1,p))
		colnames(result$opt.adj) = colnames(result$opt.theta)
		result$opt.rho		= result$rho[result$opt.index]
	}
	
		if(verbose)
		{
			cat("done.\n")
			flush.console()
		}
	result <- list(opt.adj=result$opt.adj, opt.theta=result$opt.theta, opt.rho=result$opt.rho, opt.index=result$opt.index, theta=result$Theta, path= result$path, ES=result$ES, Z=result$Z, rho=result$rho, loglik=result$loglik, data=result$data )
	return(result)
}

