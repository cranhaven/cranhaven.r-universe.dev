#######################################################################################
###############
############### AntMAN Package
###############
###############
#######################################################################################


# density_discrete_variables is internal
# 
density_discrete_variables <- function(Par, color=rgb(0.4, 0.8, 1, alpha=0.7), single_maxy=TRUE, title, ...){
	rows <- dim(Par)[2]
	fun <- function(xx){
		return(table(xx)/length(xx))
	}
	
	tables <- lapply(Par,fun)
	#cat("maxy is",maxy,"\n")
	if(single_maxy){
		maxy=rep(0,rows)
		for(r in 1:rows){
			maxy[r]=as.numeric(max(tables[[r]]))
		}
		#	print("ciao")
	}else{
		maxy <- rep(max(unlist(tables)),rows)
	}

	oldpar <- par(no.readonly = TRUE)   
  	on.exit(par(oldpar)) 
	
	par(mfrow=c(rows,1))
	for(r in 1:rows){
		plot(tables[[r]],lwd=8,col=color,ylim=c(0,maxy[r]),xlab=names(tables)[r],ylab="p.m.f.", main=title,...)
	}
	
	
	
}


#' Plot \code{\link{AM_mcmc_output}} scatterplot matrix
#' 
#' visualise a matrix of plots describing the MCMC results. This function is built upon GGally's plotting function ggpairs \insertCite{GGally}{AntMAN}.
#'  
#'@param x an \code{\link{AM_mcmc_output}} object, produced by calling \code{\link{AM_mcmc_fit}}.
#'@param tags A list of variables to consider for plotting. This function only produces meaningful plots for variables that have fixed dimension across the draws. If not specified, plots pertaining to M and K will be produced. 
#'@param title Title for the plot.
#'@return Same as ggpairs function, a ggmatrix object that if called, will print.
#'@importFrom graphics image
#'@importFrom GGally ggpairs
#'@importFrom grDevices gray.colors
#'@export
AM_plot_pairs=function(x,tags = NULL,title = "MCMC Results"){
	
	targets = tags
	if (is.null(targets)) {
		if (!is.null(x$M)) {targets = c(targets,"M")}
		if (!is.null(x$K)) {targets = c(targets,"K")}
	}
	message("Plotting pairs from ",paste(targets,collapse=","));
	
	if (length(targets) > 0) {
		df = data.frame(AM_extract(x,targets))
		ggpairs(df, title = title, upper = list(continuous = "points"),) + ggplot2::labs(title=title)
	}

}

#' Plot the density of variables from \code{\link{AM_mcmc_output}} object
#' 
#'
#' Given an \code{\link{AM_mcmc_output}} object, AM_plot_density plots the posterior density of the specified variables of interest. AM_plot_density makes use 
#' of bayesplot's plotting function mcmc_areas \insertCite{bayesplot}{AntMAN}.
#' 
#'  
#'@param x An \code{\link{AM_mcmc_output}} fit object, produced by calling \code{\link{AM_mcmc_fit}}.
#'@param tags A list of variables to consider. This function only produces meaningful plots for variables that have fixed dimension across the draws.
#'@param title Title for the plot.
#'@return a ggplot object visualising the posterior density of the specified variables.
#'@importFrom bayesplot mcmc_areas color_scheme_set
#'@export
AM_plot_density=function(x,tags = NULL,title = "MCMC Results"){
	targets = tags
	if (is.null(targets)) {
		stop('Please supplement arguments to tags. tags cannot be NULL.')
	}
	message("Plotting density from ",paste(targets,collapse=","));
	
	if (length(targets) > 0) {
		df = data.frame(AM_extract(x,targets))
		color_scheme_set("brightblue")
		mcmc_areas(df)+ ggplot2::labs(title=title)
	}
	
}


#' Plot the probability mass function of variables from \code{\link{AM_mcmc_output}} object
#' 
#'
#' Given an \code{\link{AM_mcmc_output}} object, AM_plot_pmf plots the posterior probability mass function of the specified variables.
#'  
#'@param x An \code{\link{AM_mcmc_output}} object, produced by calling \code{\link{AM_mcmc_fit}}.
#'@param tags A list of variables to consider. If not specified, the pmf of both M and K will be plotted.
#'@param title Title for the plot.
#'@return No return value. Called for side effects.
#'@importFrom grDevices rgb
#'@export
AM_plot_pmf=function(x,tags = NULL,title = "MCMC Results"){
	targets = tags
	if (is.null(targets)) {
		if (!is.null(x$M)) {targets = c(targets,"M")}
		if (!is.null(x$K)) {targets = c(targets,"K")}
	}
	message("Plotting pmf for ",paste(targets,collapse=","));
	
	if (length(targets) > 0) {
		df = data.frame(AM_extract(x,targets))
		density_discrete_variables(df, single_maxy=TRUE, title = title)
	}
	
}

#' Plot traces of variables from an \code{\link{AM_mcmc_output}} object 
#' 
#'
#' Given an \code{\link{AM_mcmc_output}} object, \code{\link{AM_plot_traces}} visualises the traceplots of the specified variables involved in the MCMC inference. 
#' AM_plot_traces is built upon bayesplot's mcmc_trace \insertCite{bayesplot}{AntMAN}.
#'  
#'@param x An \code{\link{AM_mcmc_output}} fit object, produced by calling \code{\link{AM_mcmc_fit}}.
#'@param tags A list of variables to consider. This function only produces meaningful plots for variables that have fixed dimension across the draws. If not specified, plots pertaining to M and K will be produced. 
#'@param title Title for the plot
#'@return No return value. Called for side effects.
#'@importFrom bayesplot mcmc_trace color_scheme_set
#'@export
AM_plot_traces=function(x,tags = NULL,title = "MCMC Results"){
	
	targets = tags
	if (is.null(targets)) {
		if (!is.null(x$M)) {targets = c(targets,"M")}
		if (!is.null(x$K)) {targets = c(targets,"K")}
	}
	message("Plotting traces from ",paste(targets,collapse=","));
	
	if (length(targets) > 0) {
		df = data.frame(AM_extract(x,targets))
		color_scheme_set("brightblue")
		mcmc_trace(df)+ ggplot2::labs(title=title)
	}
	
}

#' Plot posterior interval estimates obtained from MCMC draws
#' 
#' Given an object of class \code{\link{AM_mcmc_fit}}, AM_plot_values visualises the interval estimates of the specified variables involved in the MCMC inference. 
#' AM_plot_values is built upon bayesplot's mcmc_intervals \insertCite{bayesplot}{AntMAN}.  
#'
#'@param x An \code{\link{AM_mcmc_output}} fit object, produced by calling \code{\link{AM_mcmc_fit}}.
#'@param tags A list of variables to consider. This function only produces meaningful plots for variables that have fixed dimension across the draws. If not specified, plots pertaining to M and K will be produced. 
#'@param title Title for the plot.
#'@return No return value. Called for side effects.
#'@importFrom bayesplot mcmc_intervals color_scheme_set
#'@export
AM_plot_values=function(x,tags = NULL,title = "MCMC Results"){
	
	targets = tags
	if (is.null(targets)) {
		if (!is.null(x$M)) {targets = c(targets,"M")}
		if (!is.null(x$K)) {targets = c(targets,"K")}
	}
	message("Plotting values from ",paste(targets,collapse=","));
	
	if (length(targets) > 0) {
		df = data.frame(AM_extract(x,targets))
		color_scheme_set("brightblue")
		mcmc_intervals(df)+ ggplot2::labs(title=title)
	}
	
}


#'  Plot the Similarity Matrix
#'  
#'
#'  Given an \code{\link{AM_mcmc_output}} object, this function will produce an image of the Similarity Matrix.
#'  
#'@param x An \code{\link{AM_mcmc_output}} fit object, produced by calling \code{AM_mcmc_fit}.
#'@param loss Loss function to minimise. Specify either "VI" or "binder". If not specified, the default loss
#' to minimise is "binder".
#'@param ... All additional parameters wil lbe pass to the image command.
#'@return No return value. Called for side effects.
#'@importFrom graphics title
#'@export

AM_plot_similarity_matrix = function(x, loss, ...){

	CCM = AM_coclustering(x)
	CM = AM_clustering(x)

	if (loss == 'VI'){
		eam = CM
	}
	else{
		eam = CCM
	}

	hatc = AM_salso(eam, loss, 'maxZealousAttempts'=0, 'probSequentialAllocation'=1)

	par(mar=c(5,5,5,5))
	image(CCM[sort(hatc), sort(hatc)], axes=FALSE)
	axis(side=1, at=seq(0,1,length.out=ncol(CCM)), labels=sort(hatc), las=2)
	axis(side=2, at=seq(0,1,length.out=ncol(CCM)), labels=sort(hatc), las=2)
	title(main = "Ordered Similarity Matrix", cex.main=0.7)
}

# AM_plot_similarity_matrix=function(x, loss, ...){
	
# 	sorted = TRUE
# 	arguments <- list(...)
# 	if ("sorted" %in% names(arguments)) {
# 		sorted = arguments[[sorted]]
# 	}
	
	
# 	if (!is.null(x$CI)) {
		
# 		message("Plotting Similarity Matrix");

# 		pij = AM_clustering(x)
# 		clustering = AM_salso(pij, loss=loss)

# 		# binder_result = AM_binder(x)
# 		# clustering = binder_result[["Labels"]]
# 		# pij = AM_coclustering(x)
		
# 		if (sorted) {
# 			new_indexes = order(clustering, decreasing = TRUE)
# 			pij = pij[new_indexes,new_indexes];
# 		}
# 		image(pij,main="Similarity matrix") 
# 	} else {
# 		warning("CI has not been generated. Cannot plot the similarity matrix.")
# 	}
	
# }

#'  Plot the Autocorrelation function
#'  
#'  
#'
#' Given an \code{\link{AM_mcmc_output}} object, this function produces the autocorrelation function bars describing the MCMC results. AM_plot_chaincor makes use of bayesplot’s 
#' plotting function mcmc_acf_bar \insertCite{bayesplot}{AntMAN}.

#'  
#'@param x An \code{\link{AM_mcmc_output}} object, produced by calling \code{\link{AM_mcmc_fit}}.
#'@param tags A list of variables to consider. This function only produces meaningful plots for variables that have fixed dimension across the draws. If not specified, plots pertaining to M and K will be produced. 
#'This function is built upon bayesplot's \code{mcmc_acf_bar}.
#'@param lags An integer specifying the number of lags to plot. If no value is specified, the default number of lags shown is half the total number of iterations.
#'@param title Title for the plot.
#'  
#'@return A ggplot object.
#'@importFrom bayesplot mcmc_acf_bar
#'@export
AM_plot_chaincor=function(x, tags = NULL, lags = NULL, title = "MCMC Results"){
	
	targets = tags
	if (is.null(targets)) {
		if (!is.null(x$M)) {targets = c(targets,"M")}
		if (!is.null(x$K)) {targets = c(targets,"K")}
	#	if (!is.null(x$MNA)) {targets = c(targets,"MNA")}
	#	if (!is.null(x$H) && (length(x$H) > 0) && (length(x$H[[1]]) > 0)) {targets = c(targets,paste0("H_",names(x$H[[1]])))}
	#	if (!is.null(x$Q) && (length(x$Q) > 0) && (length(x$Q[[1]]) > 0)) {targets = c(targets,paste0("Q_",names(x$Q[[1]])))}
	}
	message("Plotting Autocorrelation from ",paste(targets,collapse=","))
	
	if (length(targets) > 0) {
		df = data.frame(AM_extract(x,targets))
		if (is.null(lags)) {
			lags = floor(dim(df)[1]/2)
		}
		color_scheme_set("brightblue")
		mcmc_acf_bar(df, lags = lags)+ ggplot2::labs(title=title)
	}
	

}

#'  Visualise the cluster frequency plot for the multivariate bernoulli model
#'  
#'
#'  Given an \code{\link{AM_mcmc_output}} object, and the data the model was fit on, this function will produce a cluster frequency plot for the multivariate bernoulli model.
#'  
#'@param fit An \code{\link{AM_mcmc_output}} fit object, produced by calling \code{AM_mcmc_fit}.
#'@param y A matrix containing the y observations which produced fit.
#'@param x_lim_param A vector with two elements describing the plot's x_axis scale, e.g. c(0.8, 7.2).
#'@param y_lim_param A vector with two elements describing the plot's y_axis scale, e.g. c(0, 1).
#'  
#'@return No return value. Called for side effects.
#'@importFrom grDevices n2mfrow
#'@importFrom graphics axis
#'@export
AM_plot_mvb_cluster_frequency = function(fit, y, x_lim_param= c(0.8, 7.2), y_lim_param = c(0,1)){


  eam = AM_clustering(fit)
  result = AM_salso(eam, "binder")
  hatc = result
  hatk = length(unique(hatc))
  ci = t(do.call(cbind,fit$CI)) +1
  
  # obtain dim of y
  y_dim = dim(y)[2]

  G = length(fit$K)
  n = length(hatc)
  
  thetapost <- array(0,dim=c(G,n,y_dim))
  for(g in 1:G){
    thetapost[g,,] =fit$theta[[g]][ci[g,],]
  }
  
  thetahat <- apply(thetapost,c(2,3),mean)
  
  
  # obtain col names (if any)
  col_names = colnames(y)

  oldpar <- par(no.readonly = TRUE)   
  on.exit(par(oldpar)) 
  
  par(mfrow=rev(n2mfrow(hatk)))
  
  for(j in 1:hatk){
    plot(1:y_dim,apply(thetahat[hatc==j,],2, mean),type="h",xaxt="n",
         xlim = x_lim_param, ylim= y_lim_param ,col=j, lwd=2, xlab = "",
         ylab = expression(hat(theta)), main=paste("Group", j))
    lines((1:y_dim)+0.1, apply(y[hatc==j,],2,mean), type="h",xaxt="n",
          xlim = x_lim_param, ylim = y_lim_param, col=j, lwd=2, ylab="",lty=2)
    axis(1, at=1:y_dim, labels = col_names)
  }
}
	
