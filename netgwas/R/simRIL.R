#-------------------------------------------------------------------------------#
# Package: Network-Based Genome-Wide Association Studies                        #
# simRIL(): generates genotype data from recombinant inbred lines population    #
# Author: Pariya Behrouzi                                                       #
# Emails: <pariya.Behrouzi@gmail.com>                                           #
# Date: Nov 21th 2017                                                           #
# Version: 0.0.1-1                                                              #
#-------------------------------------------------------------------------------#
simRIL <- function( d = 25, n = 200, g = 5, cM = 100, selfing=2 )
{
	Mar		<- NULL
	position<- NULL
	pos_chrs<- NULL 
	LG		<- NULL
	clusters<- NULL 
	orders	<- NULL 
	orders2 <- NULL 
	
	for ( j in 1: g )
	{
		LG 		 <-c(LG, rep(j, d ))
		clusters <-c(clusters, rep(j, d - 1))
		Mar		 <-c(Mar, 1:d )
		orders	 <-c(orders, 1:(d - 1) )
		orders2  <- c(orders2, 2:d )
		cM_dist  <- seq(0, cM, by = cM/(d - 1))
		pos_chrs <- c(pos_chrs, diff(cM_dist))
		position <- c(position, cM_dist)
	}
	rfr = 1/2* (1 - exp(-2 * pos_chrs / 100))
	
	F1 <- matrix(c(rep(0, d * g), rep(1, d*g) ) ,ncol=2, nrow= d * g) 
	sim_F1 <- vector("list", n); sim_F1 <- lapply(sim_F1, function(x) F1)	
	map <- data.frame(LG, paste("M", Mar, ".", LG, sep=""), position)
	colnames(map) <- c("chr", "marker", "cM")
	
	neighbor_mr <- data.frame(clusters, paste("M", orders, ".", clusters, sep=""), paste("M", orders2, ".", clusters, sep=""), pos_chrs, rfr)
	colnames(neighbor_mr) <- c("chr", "start", "end" , "pos_chrs", "recom")
	for (s in 1:selfing )
	{
		for (i in 1:length(sim_F1))
		{
			A <- NULL
			B <- NULL
			gam <- NULL
			haplo <- sim_F1[[i]]

						
			for (chr in 1:g)
			{
				neighbor_mr2 <- neighbor_mr[which(neighbor_mr[ ,"chr"] == chr), ]
				haplo2		 <- haplo[which(map[, "chr"] == chr), ]						
				A_new <- make_gamete( haplo2 = haplo2, neighbor_mr2 = neighbor_mr2, state = "A")	
				B_new <- make_gamete( haplo2 = haplo2, neighbor_mr2 = neighbor_mr2, state = "B")
				
				A <- c(A, A_new)	
				B <- c(B, B_new)		
			}
									
			sim_F1[i] <- list(cbind(A, B))
		}	
	}
		
	dat <- NULL
	for (i in 1:length(sim_F1))
	{
		haplo <- sim_F1[[i]] 
		dat <- cbind(dat, haplo[ ,1]+ haplo[ ,2] )
	}
	colnames(dat) <- paste("ind", 1:n, sep="")
	rownames(dat) <- map[ ,"marker"]	
	sim <- list(data= t(dat), map = map)	
	return(sim)
}	
	
make_gamete <- function( haplo2, neighbor_mr2, state = "A" )
{
	pr <- rbinom(1, 1, 0.5) + 1
	gameteA <- haplo2[ ,pr]
	gameteB <- haplo2[ ,setdiff(c(1,2), pr)]
	gam <- gameteA
	for( m in 1: nrow(neighbor_mr2))
	{	
		cr_over <- rbinom(1, 1, neighbor_mr2[m, "recom"]) 
		if (cr_over == 1 & state == "B"){gam[m+1] <- gameteA[m+1]; state2 <- "A"}
		if (cr_over == 0 & state == "B"){gam[m+1] <- gameteB[m+1]; state2 <- "B"}
		if (cr_over == 0 & state == "A"){gam[m+1] <- gameteA[m+1]; state2 <- "A"}
		if (cr_over == 1 & state == "A"){gam[m+1] <- gameteB[m+1]; state2 <- "B"}

		state <- state2
	}								
	return(gam)
}		