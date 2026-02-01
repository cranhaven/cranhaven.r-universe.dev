#-------------------------------------------------------------------------------#
# Package: Network-Based Genome-Wide Association Studies                        #
# buildMap(): build a one-dimensional map from netgwasmap object    	      		#
# Author: Pariya Behrouzi                                                       #
# Emails: <pariya.Behrouzi@gmail.com>                                           #
# Date: Nov 21th 2017                                                           #
# Version: 0.0.1-1                                                              #
#-------------------------------------------------------------------------------#
buildMap.internal = function( network, cross, num.iso.m, use.comu )
{
	id <- colnames(network)
	p <- ncol(network)
	D <- diag(network)
	network[upper.tri(network)] <- 0
	network <- network + t(network)
	diag(network) <- D
	colnames(network) <- id
	
	# LG
	if(is.null(num.iso.m)) num.iso.m = floor(p/100)
	if( num.iso.m == 0 ) num.iso.m <- 1
	if(cross == "inbred")theta <- No.trans(as.matrix(network)) else theta <- as.matrix(network)
	memberships		<- community(theta, use.comu) 
	singl.elem <- which(lapply(memberships, function(Y) length(Y) ) <= num.iso.m)
	if(length(singl.elem) != 0) memberships <- memberships[- singl.elem ]
	rm(singl.elem)

	#ordering
	if(cross == "inbred")
	{
		cond.cor <- matrix(NA, ncol=p, nrow=p)
		for(i in 1:p)
		{
			for(j in 1:p)
			{
				cond.cor[i,j] <- - theta[i,j]/ sqrt(theta[i,i]* theta[j,j])
			}
		}
		sigma 	<- - solve(cond.cor)
		cr		<- cov2cor(sigma) 
		colnames(cr) <- colnames(theta)
		rownames(cr) <- colnames(theta)
		MDS <- lapply(1:length(memberships), function(Y) MDS.order(LG=Y, cr=cr, theta=theta, memberships=memberships ))
		map <- NULL
		k   <- 1
		while( k <= length(MDS)){
		  map <- rbind( map, MDS[[k]]$map )
		  k <- k + 1
		}
			        
		rm(MDS)     
	}
	
	if(cross == "outbred")
	{
		ord <- vector("list", length(memberships))
		th.map <- vector("list", length(memberships) )
		LG <- vector("list", length(memberships) )
		for(lg in 1: length(memberships))
			{
				index <- match( memberships[[lg]], id)
				th.LG <- network[index ,index]
				rownames(th.LG) <- colnames(th.LG)
				adj <- as.matrix(abs(sign(th.LG)) - diag(ncol(th.LG)))
				if(length(unique(adj[upper.tri(adj)])) == 1) adj[1,2] <- adj[2,1] <- 0  #full graph
			
				cuthill <- cuthill_mckee(adj)
				ord[[lg]] <- colnames(cuthill)
				ind <- match(colnames(cuthill), colnames(th.LG))
				#adj <- graphAM(adj)
				#cuthill <- cuthill.mckee.ordering(adj)
				#ord[[lg]] <- cuthill$`reverse cuthill.mckee.ordering`
				#ind <- match(cuthill$`reverse cuthill.mckee.ordering`, colnames(th.LG))
				th.map[[lg]]  <- th.LG[ind, ind]
				LG[[lg]] <- rep(lg, length(ind))
			}
		map <- cbind(markers= unlist(ord), LG=unlist(LG))
	}
	return(map)
}


buildMap = function(res, opt.index, min.m = NULL, use.comu = FALSE){
	
	#if(class(res) == "netgwasmap")
  if( inherits(res, "netgwasmap") )
	{
		if( is.null(opt.index)) stop("opt.index needs to be specified! \n")
		if( is.null(use.comu)) use.comu <- FALSE
		p <- ncol(res$allres$Theta[[opt.index]])
		if(is.null(min.m)) min.m = floor(p/100)
		if( min.m == 0 ) min.m <- 3
		cross <- res$cross.typ
		
		if(cross == "inbred") theta <- No.trans(as.matrix(res$allres$Theta[[opt.index]])) else theta <- as.matrix(res$allres$Theta[[opt.index]])
		D <- diag(theta)
		id <- colnames(theta)
		theta[upper.tri(theta)] <- 0
		theta <- theta + t(theta)
		diag(theta) <- D
		colnames(theta) <- id 
		memberships		<- community(theta, use.comu)
		singl.elem <- which(lapply(memberships, function(Y) length(Y) ) <= min.m)
		if(length(singl.elem) != 0) memberships <- memberships[- singl.elem ]
		rm(D, id)	
	
		if(cross == "inbred")
		{
			cond.cor <- matrix(NA, ncol=p, nrow=p)
			for(i in 1:p)
			{
				for(j in 1:p)
				{
					cond.cor[i,j] <- - theta[i,j]/ sqrt(theta[i,i]* theta[j,j])
				}
			}
			sigma 	<- - solve(cond.cor)
			cr		<- cov2cor(sigma) 
			colnames(cr) <- colnames(theta)
			rownames(cr) <- colnames(theta)
			MDS <- lapply(1:length(memberships), function(Y) MDS.order(LG=Y, cr=cr, theta=theta, memberships=memberships ))
			map <- NULL
			k   <- 1
			while( k <= length(MDS)){
			  map <- rbind( map, MDS[[k]]$map )
			  k <- k + 1
			}
		}
		if(cross == "outbred")
		{
			ord <- vector("list", length(memberships) )
			th.map <- vector("list", length(memberships))
			LG <- vector("list", length(memberships) )
			for(lg in 1: length(memberships))
			{
				index <- match( memberships[[lg]],colnames(theta))
				th.LG <- theta[index ,index]
				rownames(th.LG) <- colnames(th.LG)
				adj <- as.matrix(abs(sign(th.LG)) - diag(ncol(th.LG)))
				if(length(unique(adj[upper.tri(adj)])) == 1) adj[1,2] <- adj[2,1] <- 0
                cuthill <- cuthill_mckee(adj)
                ord[[lg]] <- colnames(cuthill)
				ind <- match(colnames(cuthill), colnames(th.LG))
        #adj <- graphAM(adj)
				#cuthill <- RBGL :: cuthill.mckee.ordering(adj)
				#ord[[lg]] <- cuthill$`reverse cuthill.mckee.ordering`
				#ind <- match(cuthill$`reverse cuthill.mckee.ordering`, colnames(th.LG))
				th.map[[lg]]  <- th.LG[ind, ind]
				LG[[lg]] <- rep(lg, length(ind))
			}
			map <- cbind(markers= unlist(ord), LG= unlist(LG))
		}
		results <- list(map= map, opt.index= opt.index, cross= cross, allres= res$allres, man= TRUE)
		class(results$allres) = "netgwas"
		class(results) = "netgwasmap"
		return(results)
	}else{
		stop("netgwasmap.object should belong to the netgwasmap class. \n ")
	}
}

#' Cuthill McKee (CM) algorithm
#' 
#' Transform sparse matrix into a band matrix
#' netprioR ::: cuthill_mckee()
#' @author Fabian Schmich
#' @import Matrix
#' @param x Input matrix
#' @return Band matrix
cuthill_mckee <- function(x) {
  degs <- data.frame(Idx=1:ncol(x), NonZero=apply(x, 1, function(x) length(which(x != 0))))
  R <- degs$Idx[which.min(degs$NonZero)]
  i <- 1
  for (i in 1:ncol(x)) {
    Ai <- setdiff(which(x[R[i],] != 0), R)
    if (length(Ai) > 0) {
      Ai <- Ai[order(degs$NonZero[Ai], decreasing = FALSE)]
      R <- append(R, Ai)
    } else {
      R <- append(R, degs$Idx[-R][which.min(degs$NonZero[-R])])
    }
    i <- i + 1
  }
  rR <- rev(R)
  return(x[rR, rR])
}


#### Check for invariant and markers#####
cleaning.dat = function(dat){
	dat <- as.matrix(dat, byrow= FALSE, ncol= ncol(dat), nrow= nrow(dat) )
	#remove fixed-value (invariant) markers
	fixd.val <-  which(apply(dat, 2, function(x) length(unique(sort(x)))) == 1)
	if(length(fixd.val) == 0){dat <- dat }else {dat <- dat[ ,- fixd.val ]}
	return(dat)
}

No.trans <- function(theta){
  dig <- diag(theta)
  theta[theta > 0 ] <-  0
  diag(theta) <- dig
  return(theta)
}

##### community detection
community = function(theta, use.comu){
	p		<- ncol(theta)
	path	<- abs(sign(theta)) - diag(rep(1,p))
	colnames(path) <- colnames(theta)
	adj <- graph.adjacency(path, mode="undirected")
	if(use.comu == TRUE)
	{
		fg <- cluster_fast_greedy(adj)
		memberships <- lapply(1:length(fg), function(x) fg[[x]]) 
	}else{
		cl  <- clusters(adj)
		grp <- groups(cl)
		memberships <- lapply(1:cl$no, function(x)  grp[[x]] ) 
	}
	return(memberships)
}


##### Ordering markers per LG 
MDS.order <- function( LG=3, cr, theta, memberships )
{
  index <- match(memberships[[LG]],colnames(cr))
  cr.LG <- cr[index ,index]
  theta.LG <- theta[index ,index]
  rownames(cr.LG) <- colnames(cr.LG)
  rownames(theta.LG) <- colnames(theta.LG)
  cr.LG[cr.LG <= 0] <- runif(1, 1e-08, 5e-08)
  dis		<- -log(cr.LG)
  dis.1	<- sammon(dis, k = 1, trace =FALSE)
  MDS.path <- order(dis.1$points)
  MDS.tour <- colnames(cr.LG )[MDS.path]
  chr <- rep(LG, length(MDS.tour))
  ordered <- data.frame(markers=MDS.tour, LG=chr )
  theta.LG <- theta.LG[MDS.path , MDS.path ]
  cr.LG <- cr.LG[MDS.path , MDS.path ]
  results <- list(map=ordered, cr.LG=cr.LG, theta.LG=theta.LG )
 
  return(results)
}

##### constructors graphAM as a subclasses of graph
graphAM <- function (adjMat = matrix(integer(), 0, 0), edgemode='undirected', values=NA)
{
    if (length (values) == 1L && is.na (values))
        new("graphAM", adjMat=adjMat, edgemode=edgemode)
    else
        new("graphAM", adjMat=adjMat, edgemode=edgemode,
                 values=values)
}
