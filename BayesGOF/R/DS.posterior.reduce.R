DS.posterior.reduce <-
function(DS.GF.obj, exposure = NULL){
	#colnames(reduce.mat) <- c("PEB_MN", "DS_MN", "PEB_MD", "DS_MD")
	fam = DS.GF.obj$fam
	switch(fam,
		"Normal" = {
			reduce.mat <- t(apply(DS.GF.obj$obs.data, 1, 
								function(x) DS.micro.inf.nnu(DS.GF.obj, 
											y.0 = x[1], se.0 = x[2])$post.vec))
			},
		 "Binomial" = {
			reduce.mat <- t(apply(DS.GF.obj$obs.data, 1, 
								function(x) DS.micro.inf.bbu(DS.GF.obj, 
											y.0 = x[1], n.0 = x[2])$post.vec))
			},
		 "Poisson" = {
		 	tbl <- table(DS.GF.obj$obs.data)
			cnt.vec <- as.integer(unlist(dimnames(tbl)))
			cnt.vec <- c(min(cnt.vec):max(cnt.vec))
			reduce.mat <- matrix(0, ncol = 4, nrow = length(cnt.vec))
			if(is.null(exposure) == TRUE){
				for(i in 1:length(cnt.vec)){
					reduce.mat[i,] <- DS.micro.inf.pgu(DS.GF.obj,
									y.0 = cnt.vec[i])$post.vec
					}
				} else {
				for(i in 1:length(cnt.vec)){
					reduce.mat[i,] <- DS.micro.inf.pge(DS.GF.obj,
									y.0 = cnt.vec[i], e.0 = exposure[i])$post.vec
					}				
				}
			} 
		)	
	out <- data.frame(PEB_MN = reduce.mat[,1],
					  DS_MN = reduce.mat[,2],
					  PEB_MD = reduce.mat[,3],
					  DS_MD = reduce.mat[,4])
	return(out)		
}	
