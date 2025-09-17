#######################################################################################
###############
############### AntMAN Package
###############
###############
#######################################################################################


#'  Return the co-clustering matrix
#'  
#'
#'  Given an \code{\link{AM_mcmc_output}} object, this function returns the co-clustering matrix. 
#'
#'  The co-clustering matrix is produced by the simultaneous clustering of the rows and columns. Each entry denotes the (posterior) probability 
#' that items \eqn{i} and \eqn{j} are together.  This technique is also known as
#' bi-clustering and block clustering \insertCite{govaert2013co}{AntMAN}, and is useful for understanding the number of clusters in the dataset. 
#'  
#'@param fit an \code{\link{AM_mcmc_output}} object.
#'@return A numeric co-clustering matrix
#'@seealso \code{\link{AM_clustering}}
#'
#'@examples
#'\donttest{
#' fit = AM_demo_uvp_poi()$fit
#'ccm <- AM_coclustering(fit)
#'}
#'@export
AM_coclustering = function (fit) {
	
	CI =  as.matrix((AM_extract(fit, c("CI")))[["CI"]]);
	n_save <- dim(CI)[1]
	N  <- dim(CI)[2]
	
	c_out <- lapply(1:n_save, function(x){outer(CI[x,], CI[x,], "==")})
	pij = Reduce("+", c_out)/n_save

	#cast pij into matrix
	pij = matrix(unlist(pij), ncol=N, nrow=N, byrow=F)
	
	return(coclustering_probability = pij);
}


#'  Return the clustering matrix
#'  
#'
#'  Given an \code{\link{AM_mcmc_output}} object, this function returns the clustering matrix. 
#'
#' The clustering matrix is an M by n matrix. Each of the M rows represents a clustering of n items
#' using cluster labels. Items i and j are in the same cluster if fit\[m,i\] == fit\[m,j\] for the mth clustering.


#'@param fit an \code{\link{AM_mcmc_output}} object.
#'@return A numeric clustering matrix
#'@export
#'@seealso \code{\link{AM_coclustering}}
#'
#'@examples
#'\donttest{
#' fit = AM_demo_uvp_poi()$fit
#'ccm <- AM_clustering(fit)
#'}
AM_clustering = function (fit) {
	CI = as.matrix((AM_extract(fit, c("CI")))[["CI"]]);
	return(CI);
}

