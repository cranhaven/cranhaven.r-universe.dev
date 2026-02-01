#-----------------------------------------------------------------------#
# Package: nutriNetwork                                                 #
# nutriNetwork(): networks reconstruction algorithm                     #
# Authors: Pariya Behrouzi                                              #
# maintainer: <pariya.Behrouzi@gmail.com>                               #
# Date: Feb 12 2019                                                     #
# Version: 0.1.0                                                        #
#-----------------------------------------------------------------------#

nutriNetwork = function(data, method = "gibbs", rho = NULL, n.rho = NULL, rho.ratio = NULL, ncores = 1, em.iter = 5, em.tol = .001, verbose = TRUE) 
{

	if(!is.matrix(data)) data <- as.matrix(data)
	if(ncores == "all") ncores <- detectCores() - 1
	if(is.null(em.iter)) em.iter = 5
	if(is.null(em.tol)) em.tol = 0.001
	
	n = nrow(data)
	p = ncol(data)
	result = list()
	
if( method == "gibbs" ||  method== "approx" ) 
{	
	if( method == "gibbs")
	{
		if((is.null(rho)) && (is.null(n.rho)) ) n.rho  = 6
		if(! is.null(rho)) n.rho  = length(rho)
		if(is.null(rho.ratio)) 
		{
			if( p <= 50) rho.ratio = 0.3 else rho.ratio = 0.35
		}	
		est = vector("list", n.rho)
		for(chain in 1 : n.rho ) 
		{
		if(verbose)
		{
			m <- paste(c("Network reconstruction is in progress ... :", floor(100 * chain/n.rho), "%"), collapse="")
			cat(m, "\r")
			flush.console()
		}
			if( chain == 1)
			{
				est[[chain]] = vector("list", n.rho)
				Theta = sparseMatrix(i = 1:ncol(data), j = 1:ncol(data), x = 1)
				est[[chain]] = Gibbs_method(data, rho=rho, n_rho=n.rho, rho_ratio=rho.ratio, Theta = Theta, ncores = ncores, chain = chain, max.elongation = em.iter, em.tol=em.tol)
			}else{
				est[[chain]] = vector("list", n.rho)
				Theta = est[[(chain - 1)]]$Theta
				Theta = as(Theta, "dgTMatrix") 
				Theta = as(Theta, "sparseMatrix")
				est[[chain]] = Gibbs_method(data, rho=rho, n_rho=n.rho, rho_ratio=rho.ratio, Theta= Theta, ncores = ncores, chain = chain, max.elongation = em.iter, em.tol=em.tol)
			}
		}
		rm(Theta)
		gc()
	}
	
	if(method == "approx")
	{

		if( !is.null(rho) ) n.rho = length(rho) 
		if( is.null(n.rho) ) n.rho = 6
		if(is.null(rho.ratio)) rho.ratio = 0.4
		
		ini = initialize(data, rho = rho, n_rho = n.rho, rho_ratio = rho.ratio, ncores=ncores )
		rho = ini$rho
		Z	= ini$Z
		ES	= ini$ES
		lower_upper = ini$lower_upper
		
		rm(ini)
		gc()
		
		est <- vector("list", n.rho)
		for(chain in 1 : n.rho) 
		{
		if(verbose)
			{
				m <- paste(c("Network reconstruction is in progress ... :", floor(100 * chain/n.rho), "%"), collapse="")
				cat(m, "\r")
				flush.console()
			}
			est[[chain]] <- vector("list", n.rho)
			est[[(chain)]] <- approx_method(data, Z, ES=ES, rho=rho, lower_upper=lower_upper, chain = chain, ncores = ncores, em_tol=em.tol, em_iter=em.iter)
		}

		rm(lower_upper)
		gc()
	}
	
	result$Theta  = vector("list", n.rho)
	result$path   = vector("list", n.rho)
	result$ES	  = vector("list", n.rho)
	result$Z	  = vector("list", n.rho)
	result$rho	  = vector()
	result$loglik = vector()
	result$data	  = data
	rm(data)
	
	for(chain in 1:n.rho)
	{
		result$Theta[[chain]]	= est[[chain]]$Theta
		if(!is.null(colnames(result$data))) colnames(result$Theta[[chain]]) = colnames(result$data)
		result$path[[chain]]	= abs(sign(result$Theta[[chain]])) - Diagonal(p)
		result$ES[[chain]]		= est[[chain]]$ES
		result$Z[[chain]]		= est[[chain]]$Z
		result$rho[chain]		= est[[chain]]$rho
		result$loglik[chain]	= est[[chain]]$loglik
	}
	rm(est)
	
	if(verbose) cat("Network reconstruction is done.  \n")
}else{
	if( method == "npn" ){
	
		if(verbose)
		{
			m <- paste("Network reconstruction is in progress ... \n")
			cat(m, "\r")
			flush.console()
		}
			
		if((is.null(rho)) && (is.null(n.rho)) ) n.rho  = 6
		if(! is.null(rho)) n.rho  = length(rho)
		if(is.null(rho.ratio)) 
		{
			if( p <= 50) rho.ratio = 0.25 else rho.ratio = 0.3
		}
		
		if( any(is.na(data)) ) npn.func = "shrinkage" else npn.func = "skeptic"
		tdata <- npn(data, npn.func= npn.func)
		est <- huge(tdata, lambda= rho, nlambda=n.rho, lambda.min.ratio=rho.ratio, method="glasso", verbose=FALSE)
			
		result$Theta  = est$icov
		result$path   = est$path
		result$rho	  = est$lambda
		result$loglik = n/2 * (est$loglik)
		result$data	  = est$data
		rm(est)
		if(verbose)
		{
			m <- paste("Network reconstruction is done. \n")
			cat(m, "\r")
			flush.console()
		}
	}
}	
	class(result) = "nutriNetwork" 
	return(result)
}
#-----------------------------------------------------#
#   		Plot for class "nutriNetwork"      	          #
#-----------------------------------------------------#
plot.nutriNetwork = function( x, n.memberships=NULL , ...)
{
	if(length(x$rho) == 1 ) par(mfrow = c(1, 2), pty = "s", omi=c(0.3,0.3,0.3,0.3), mai = c(0.3,0.3,0.3,0.3))
	if(length(x$rho) == 2 ) par(mfrow = c(2, 2), pty = "s", omi=c(0.3,0.3,0.3,0.3), mai = c(0.3,0.3,0.3,0.3))
	if(length(x$rho) >= 3 ) par(mfrow = c(2, ceiling(length(x$rho)/2)), pty = "s", omi=c(0.3,0.3,0.3,0.3), mai = c(0.3,0.3,0.3,0.3))
	
	for(chain in 1:length(x$rho))
	{
			if(length(x$rho) == 1 ) image(as.matrix(x$path[[chain]]), col = gray.colors(256), xlab= "", ylab="" ,main=paste( "rho ", x$rho[chain],  sep=""))
			if(length(x$rho) == 2 ) image(as.matrix(x$path[[chain]]), col = gray.colors(256), xlab= "", ylab="" ,main=paste( "rho ", x$rho[chain],  sep=""))
			
			adj = graph.adjacency(as.matrix(x$path[[chain]]), mode="undirected", diag=FALSE)
			if(is.null(n.memberships)) 
			{
					memberships = 1
					vertex.color = "red" 
			}else{
				LG = length(n.memberships)
				memberships = NULL
				i = 1
				while( i <= LG){
					grp <- rep(i, n.memberships[i])
					memberships = c(memberships, grp)
					i = i + 1
				}
				if(chain == 1){
					color = sample(terrain.colors(max(memberships) + 10), max(memberships))
					cl = color[memberships]
				}
				vertex.color = cl
			}
			adj$layout	= layout.fruchterman.reingold
			plot(adj, vertex.color = vertex.color, edge.color='gray40', vertex.size = 7, vertex.label = NA , vertex.label.dist = NULL)
	}
	if(length(memberships) > 1) legend("bottomright", paste("group", 1:length(n.memberships)), cex=0.7, col= color, pch=rep(20,10))		
}
#-----------------------------------------------------#
#   		Summary for class "nutriNetwork"        		      #
#-----------------------------------------------------#
print.nutriNetwork = function(x, ...){
	cat("Estimated a graph path for", length(x$rho), "penalty term(s)" , "\n")
	cat("Number of variables: p =", ncol(x$data), "\n")
	cat("Number of sample size: n =", nrow(x$data), "\n")
	sparsLevel <- sapply(1:length(x$rho), function(i) sum(x$Theta[[i]])/ncol(x$data)/(ncol(x$data)-1))
    cat("Sparsity level for each graph in the path:", sparsLevel,"\n")
	cat("To visualize the graph path consider plot() function \n")
	cat("To SELECT an optimal graph consider selectnet() function \n")
}