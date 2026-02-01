#-------------------------------------------------------------------------------#
# Package: Network-Based Genome-Wide Association Studies                        #
# netmap(): Reconstruct conditional dependence networks among markers on genome #
# Authors: Pariya Behrouzi, Ernst Wit                                           #
# maintainer: <pariya.Behrouzi@gmail.com>                                       #
# Date: Nov 21th 2017                                                           #
# Version: 0.0.1-1                                                              #
#-------------------------------------------------------------------------------#

netmap = function(data, method = "npn", cross= NULL, rho = NULL, n.rho = NULL, rho.ratio = NULL, min.m= NULL, use.comu= FALSE, ncores = "all", em.iter = 5, verbose = TRUE) 
{
  gcinfo(FALSE)
	if(is.null(cross)) stop("the argument cross should be fill in either as \"inbred\" or \"outbred\" \n")
	if(!is.matrix(data)) data <- as.matrix(data)
	if(ncores == "all") ncores <- detectCores() - 1
	if(is.null(em.iter)) em.iter = 5
	em.tol = 0.001
	
	result = list()
	data <- cleaning.dat(data)
	p = ncol(data)
	n = nrow(data)
	
if( method == "gibbs" ||  method== "approx" ) 
{
	if( method == "gibbs")
	{
		if((is.null(rho)) && (is.null(n.rho)) ) n.rho = 6
		if(! is.null(rho)) n.rho  = length(rho)
		if(is.null(rho.ratio)) 
		{
			if(p >= 2000) rho.ratio = 0.5 else rho.ratio = 0.45
		}
		est = vector("list", n.rho)
		for(chain in 1 : n.rho ) 
		{
			if(verbose)
			{
				m <- paste(c("Constructing linkage map is in progress:", floor(100 * chain/n.rho), "%"), collapse="")
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
		if(is.null(rho.ratio)) 
		{
			if(p >= 2000) rho.ratio = 0.7 else rho.ratio = 0.65
		}
		
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
				m <- paste(c("Constructing linkage map is in progress:", floor(100 * chain/n.rho), "%"), collapse="")
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
		result$rho[chain]		= est[[chain]]$rho
		result$loglik[chain]	= est[[chain]]$loglik
	}
	rm(est)
	class(result) = "netgwas"
}else{	
	if(method == "npn")
	{
		if(verbose)
		{
			m <- paste("Constructing linkage map is in progress ... \n")
			cat(m, "\r")
			flush.console()
		}
		if((is.null(rho)) && (is.null(n.rho)) ) n.rho = 6
		if(! is.null(rho)) n.rho  = length(rho)
		if(is.null(rho.ratio)) 
		{
			rho.ratio = 0.5
		}
		if( any(is.na(data)) ) npn.func = "shrinkage" else npn.func = "skeptic"
		tdata <- npn(data, npn.func= npn.func)
		if(is.null(rho))
		{
			cr = cor(tdata, method = "spearman") - diag(p)
			cr[is.na(cr)] <- 0
			rho_max = max(max(cr),-min(cr))
			if(( p <= 100) &&(rho_max >= .7)) rho_max = .8 
			if(( p > 100) && (rho_max >= .7)) rho_max = .7 
			rho_min = rho.ratio * rho_max
			rho = exp(seq(log(rho_max), log(rho_min), length = n.rho))
			rm(cr, rho_max, rho_min, rho.ratio)
		}
		
		est <- huge(tdata, lambda= rho, nlambda=n.rho, lambda.min.ratio=rho.ratio, method="glasso", verbose=FALSE)
			
		result$Theta  = est$icov
		result$path   = est$path
		result$rho	  = est$lambda
		result$loglik = n/2 * (est$loglik)
		result$data	  = data
		
		rm(data, est)		
		if(is.null(colnames(result$Theta[[1]]))) {for(i in 1:length(result$rho)) colnames(result$Theta[[i]]) <- colnames(result$data)}
		if(is.null(colnames(result$path[[1]]))) {for(i in 1:length(result$rho)) colnames(result$path[[i]]) <- colnames(result$data)}
		class(result) = "netgwas"
	}	
}	
	#Detecting LGs and ordering within each LG
	if(is.null(use.comu)) use.comu <- FALSE
	sel.net <- selectnet(result, criteria = "ebic", ebic.gamma = 0.5, ncores = ncores, verbose=FALSE)
	if(method == "npn") colnames(sel.net$opt.theta) <- colnames(result$data)
	map <- buildMap.internal(network= sel.net$opt.theta, cross= cross, num.iso.m= min.m, use.comu= use.comu)

	result$data <- result$data[ , c(as.character(map[,1]))]
	if(cross == "outbred") map <- data.frame(map) #added due to cross obj as.cross fun (can be removed later)
	results <- list( map= map, opt.index= sel.net$opt.index, cross.typ= cross, allres= result, man= FALSE)
	
	class(results$allres) = "netgwas"
	class(results) = "netgwasmap"
	rm(result)
	
	cat("Constructing linkage map is done.             \r\n")
	return(results)
}

#-----------------------------------------------------#
#   		Plot for class "netgwasmap"      	      #
#-----------------------------------------------------#
plot.netgwasmap = function(x, vis= NULL, layout= NULL, vertex.size= NULL, label.vertex = "none", label.size= NULL, vertex.color= NULL, edge.color = "gray29", sel.ID = NULL, ...)
{
    if(! inherits( x, "netgwasmap" )) stop("netgwas.object should belong to the netgwas class. \n ")
	if(is.null(vis)) vis <- "summary"
	if(is.null(label.vertex)) label.vertex <- "none"
	if(vis == "summary")
	{
		if(is.null( vertex.color))  vertex.color <- "red"
		if(is.null( label.size)) label.size <- 0.8
		if(is.null( vertex.size)) vertex.size <- 5
		if(label.vertex == "none") vertex.label <- NA

		opt.theta <- as.matrix(x$allres$Theta[[x$opt.index]]) 
		p <- ncol(opt.theta)
		path <-  as.matrix(abs(sign(opt.theta)) - diag(rep(1,p)))
		adj <- graph.adjacency(path, mode="undirected")
		adj$label.cex <- label.size
		if(label.vertex == "all") vertex.label <- colnames(opt.theta) #else{vertex.label <- NA}
		if(is.null(layout)) layout <- layout.fruchterman.reingold
    
		par(mfrow=c(1,1))
		split.screen( figs = c( 1, 2 ) )
		split.screen( figs = c( 1, 1 ), screen = 1 )
		split.screen( figs = c( 2, 1 ), screen = 2 )
		screen(1)
		plot(adj, layout=layout, edge.curved = F, vertex.label= vertex.label, vertex.color=vertex.color, edge.color="gray40", vertex.size=vertex.size, vertex.label.dist=0, vertex.label.color="darkblue", main="Network")
		screen(4)
		#image(path, col = gray.colors(256), xaxt="n", yaxt="n", main= "Conditional dependence relationships \nbefore ordering", cex.main=.8, cex.lab=.8, cex.axis=.8)
		image(path, col = c("white", "black"), xaxt="n", yaxt="n", main= "Before ordering markers", cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
		title(ylab = "markers", cex.lab = 1.5, line = .5)
		title(xlab = "markers", cex.lab = 1.5, line = .5)

		index <- as.character(x$map[ ,1])
		rownames(path) <- colnames(path)
		path.After <- path[c(index), c(index)] 
		screen(5)
		#image(path.After, col = gray.colors(256), xaxt="n", yaxt="n", ,main="Conditional dependence relationships \nafter ordering", cex.main=0.8, cex.lab=.8, cex.axis=.8)
		image(path.After, col = c("white", "black"), xaxt="n", yaxt="n", ,main="After ordering markers", cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
		title(ylab = "markers", cex.lab = 1.5, line = .5)
		title(xlab = "markers", cex.lab = 1.5, line = .5)
	}
	if(vis == "interactive")
	{
		 adj <- as.matrix(x$allres$path[[x$opt.index]])
		  
		  if(is.null(vertex.size)) vertex.size <- 5
		  if(is.null(vertex.color)) vertex.color <- "red"
		  if(is.null(edge.color)) edge.color <- "gray29"
		  if(is.null(sel.ID)) sel.ID <- NULL
		  if((label.vertex == "some") && (is.null(sel.ID )) ) stop("Please select some vertex label(s) or fix label.vertex to either none or all.")

		  p <- ncol(adj)
		  A <- graph.adjacency(adj, mode= "undirected")
		  if(is.null(layout)) layout <- layout_with_fr(A)
		  #if(is.null(layout)) layout <- layout_with_kk(A, maxiter = 50 * p )
		  V(A)$label.cex <- 1
	 
		  if(label.vertex == "none")
		{
			V(A)$label <- NA
			tkplot(A, layout=layout, vertex.color=vertex.color, edge.color=edge.color, vertex.size=vertex.size, vertex.label.dist=0)  
		}

		  if(label.vertex == "some") 
		{
			V(A)$label <- colnames(adj)
			tkplot(A, vertex.label=ifelse(V(A)$label %in% sel.ID, V(A)$label, NA ), layout=layout, vertex.color=vertex.color, edge.color=edge.color, vertex.size=vertex.size, vertex.label.dist=0)
		}
		 if(label.vertex == "all") 
		{	
			V(A)$label <- colnames(adj)
			tkplot(A, vertex.label=colnames(adj) , layout=layout, vertex.color=vertex.color, edge.color=edge.color, vertex.size=vertex.size, vertex.label.dist=0)  
		}	
	}
	
	if(vis == "unordered markers") 
	{
	  opt.theta <- x$allres$Theta[[x$opt.index]]
	  print(image(Matrix(opt.theta), xlab="markers", ylab="markers", main="Unordered markers", cex= 3, sub= ""))
	}

	if(vis == "ordered markers") 
	{
	  opt.theta <- x$allres$Theta[[x$opt.index]]
	  rownames(opt.theta)  <- colnames(opt.theta) 
	  
	  #orderedm <- Matrix( opt.theta[colnames(x$allres$data)  , colnames(x$allres$data) ] )
	  map <- as.character(x$map[ ,1])
	  orderedm <- Matrix( opt.theta[ map  , map  ] )
	 
	  print(image(orderedm,  xlab="markers", ylab="markers", main="Ordered markers", cex= 3, sub=""))
	}
}

#-----------------------------------------------------#
#   		Summary for class "netgwasmap"                #
#-----------------------------------------------------#
print.netgwasmap = function(x, ...){
	cat("Number of linkage groups: ", length(unique(sort(x$map[,2]))), "\n")
	cat("Number of markers per linkage group: ", table(x$map[, 2]), "\n")
	cat("Total number of markers in the linkage map:", length(x$map[,1]),".", " (", (ncol(x$allres$Theta[[1]]) - length(x$map[,1])), " markers removed from the input genotype data) \n")
	cat("Number of sample size: n =", nrow(x$allres$data), "\n")
	cat("Number of categories in dataset:", length(unique(sort(as.matrix(x$allres$data))))," (" , unique(sort(as.matrix(x$allres$data))), ")", "\n")
	cat("The estimated linkage map is inserted in <OUTPUT NAME>$map \n")
	cat("To visualize the network consider plot(<OUTPUT NAME>) \n")
	cat("----------------------- \n")
	cat("To visualize the other associated networks consider plot(<OUTPUT NAME>$allres) \n")
	if(x$man == FALSE) cat("To build a linkage map for your desired network consider buildMap() function \n")
}
