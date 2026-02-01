#-----------------------------------------------------------------------#
# Package: Network-Based Genome-Wide Association Studies                #
# simgeno(): Generate an ordinal data with genome-like graph structure  #
# Author: Pariya Behrouzi                                               #
# Emails: <pariya.Behrouzi@gmail.com>                                   #
# Date: Nov 21th 2017                                                   #
# Version: 0.0.1-1                                                      #
#-----------------------------------------------------------------------#

simgeno = function( p = 90, n = 200, k = NULL, g = NULL, adjacent = NULL, alpha = NULL, beta = NULL, con.dist = "Mnorm", d = NULL, vis = TRUE)
{

  if(is.null(k)) k = 3
  if(is.null(g))
  {
  	g = 1
  	if(p > 40)	g = ceiling(p/20)
  	if(p <= 40) g = 2
  }

	g.large = p %% g
	g.small = g - g.large
	n.small = floor(p/g)
	n.large = n.small+1
	g.list = c(rep(n.small,g.small),rep(n.large,g.large))
	g.ind = rep(c(1:g), g.list)
	rm(g.large, g.small, n.small, n.large)
	gc()
	
	A = matrix(0, p, p);	
	g.count = c(0, cumsum(g.list))
	if(is.null(adjacent)) adjacent = 1 
	for(i in 1:adjacent){
	for(j in 1: (length(g.count)-1)){
		diag(A[(g.count[j]+1):(g.count[j+1] - i), ((g.count[j]+1)+i):g.count[j +1 ]]) = 1
		diag(A[((g.count[j]+1)+i):g.count[j+1], (g.count[j]+1):(g.count[j+1] - 1) ]) = 1
	}
	}
	diag(A) <- 1
	image(Matrix(A))
	
	if(is.null(alpha)) alpha <- 0.01
	alpha = 1 - alpha
	for(i in 1:g){
		tmp = which(g.ind==i)
		tmp2 = matrix(runif(length(tmp)^2, 0, 0.5),length(tmp),length(tmp))
		tmp2 = tmp2 + t(tmp2)		 	
		A[tmp,tmp][tmp2 > alpha] = 1  
		rm(tmp,tmp2)
		gc()
	}
	image(Matrix(A))
	
	if(is.null(beta)) beta <- 0.02
	beta = 1 - beta
	tmp2 = matrix(runif(p^2, 0, 0.5), p, p)
	tmp2 = tmp2 + t(tmp2)
	A[tmp2 > beta] = 1
	rm(tmp2)
	image(Matrix(A))
	
	u= 0.1
	V <- matrix( 0, p, p )
	diag(A) = 0
	V[upper.tri(V)] <- runif(p*(p-1)/2, min=0.5, max=0.8)
	V <- V + t(V)
	theta = A * - V
	diag(theta) = abs(min(eigen(theta)$values)) + 0.1 + u
	sigma = cov2cor(solve(theta))
		
	if(is.null(con.dist)) con.dist = "Mnorm"	
	if(con.dist == "Mt")
	{
		if(is.null(d)) d = 3
		z = rmvt(n=n, sigma = sigma, df = d)
		sigmahat = cor(z)	
	}
  
	if(con.dist == "Mnorm")
	{
		z = rmvnorm(n=n, sigma=sigma)		
		sigmahat = cor(z)	
	}

	cutoffs   <- matrix( runif( k * p ), nrow = p, ncol = k )   
	marginals <- apply( cutoffs, 1, function(x) { qnorm( cumsum( x / sum(x) )[-length(x)] ) } )
  
	for ( j in 1:p )
	{
		breaks <- c( min( z[,j] ) - 1, marginals[,j], max(z[,j]) + 1 )  
		z[,j]  <- as.integer( cut( z[,j], breaks = breaks, right = FALSE ) )
	}	
	if ( vis == TRUE )
	{
		Figuers = par(mfrow = c(1, 2), pty = "s", omi=c(0.3,0.3,0.3,0.3), mai = c(0.3,0.3,0.3,0.3))
	 
		adj <- graph.adjacency( A, mode = "undirected", diag=FALSE )
		colnames(A)   <- paste("M", 1:p, sep="")
		if ( p < 50 ) label = colnames(A) else label = NA 
		if ( p < 50 ) size = 15 else size = 7
		color         <-  terrain.colors(g)
		ly <- layout_with_fr(adj)
		Figuers[1] = plot(adj, layout = ly , vertex.color = color[g.ind], vertex.size =size, vertex.label = label, vertex.label.color = 'black', vertex.label.dist=0, main = "Graph structure")
		Figuers[2] = image(A, col = gray.colors(2, start=1, end=0), xaxt="n", yaxt='n', main = "Adjacency Matrix")
		title(ylab = "markers", cex.lab = 1, line = .5)
		title(xlab = "markers", cex.lab = 1, line = .5)
		box()
		rm(Figuers)
		gc()
    }
	z <- z - 1
	
	colnames(z) <- colnames(A)
	simulation <- list( data=z, Theta=theta, adj=Matrix(A, sparse=TRUE), Sigma=sigma, n.groups=g, groups=g.ind, sparsity= sum(A)/(p*(p-1)))
  
	class(simulation) = "simgeno"
	return( simulation )
}


#-----------------------------------------------------#
#   		Plot for class "simgeno"        	      #
#-----------------------------------------------------#
plot.simgeno = function(x, layout = layout.fruchterman.reingold, ...){

   	par = par(mfrow = c(2, 2), pty = "s", omi=c(0.3,0.3,0.3,0.3), mai = c(0.3,0.3,0.3,0.3))
   	if ( ncol(x$data) < 50 ) size = 10 else size = 7
	if(is.null(layout)) layout = layout.fruchterman.reingold
	color =  terrain.colors(x$n.groups)
	g = graph.adjacency(x$adj, mode="undirected", diag=FALSE)
	plot(g, layout=layout, vertex.color=color[x$groups], vertex.size = size, vertex.label = NA,  vertex.label.dist=0, main = "Graph structure")
	
	
	image(as.matrix(x$adj), col = gray.colors(2, start=1, end=0), xaxt="n", yaxt="n", main = "Adjacency matrix")
	title(ylab = "markers", cex.lab = 1, line = .5)
	title(xlab = "markers", cex.lab = 1, line = .5)
	box()
	image(as.matrix(x$Theta), col = gray.colors(300), main = "Precision matrix")
	image(as.matrix(x$Sigma), col = gray.colors(300), main = "Covariance matrix")
	
	rm(g, layout, color)
	gc()
	
}

#-----------------------------------------------------#
#   		Summary for class "simgeno"        		  #
#-----------------------------------------------------#
print.simgeno = function(x, ...){
	cat("Simulated data generated by generate.ord()\n")
	cat("Number of variables: p =", ncol(x$data), "\n")
	cat("Sample size: n =", nrow(x$data), "\n")
	cat("Number of levels: k =", length(unique(sort((x$data)))), "\n")
    cat("Number of groups: g =", x$n.groups, "\n")
	cat("Graph type = genome-like", "\n")
    cat("Sparsity level:", sum(x$Theta)/ncol(x$data)/(ncol(x$data)-1),"\n")
}