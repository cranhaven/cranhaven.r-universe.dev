rPPD.ds <- 
function(DS.object, runs, pred.type = c("posterior","prior"), exposure = NULL){ #change exp.vec to exposure
	out <- list()
	pred.type = match.arg(pred.type)
	fam <- DS.object$fam
	out$obs.data <- DS.object$obs.data
	switch(fam,
		"Binomial" = {
			prior.fam <- "Beta"
			out$y.matrix <- matrix(0, nrow = runs, ncol = dim(DS.object$obs.data)[1])
			out$n.vec <- DS.object$obs.data[,2]
			if(pred.type == "posterior"){
				for(i in 1:runs){
					post.samps <- apply(DS.object$obs.data, 1, 
							function(x) DS.sampler.post(1,DS.object$g.par,
							DS.object$LP.par, y.0 = x[1], n.0 = x[2], 
							con.prior = prior.fam, LP.type = DS.object$LP.type))
					samps.df <- data.frame(theta = post.samps, 
								n = DS.object$obs.data[,2])
					out$y.matrix[i,]<- apply(samps.df, 1, 
								function(x) rbinom(1,size = x[2], prob = x[1]))
										}
				out$pred.type <- "posterior"
				class(out) <- "PostPD"
			} else {
				for(i in 1:runs){
					prior.samps <- DS.sampler(k = dim(DS.object$obs.data)[1], g.par = DS.object$g.par, 
										   LP.par = DS.object$LP.par, 
										   con.prior = prior.fam, LP.type = DS.object$LP.type)  
					samps.df <- data.frame(theta = prior.samps, 
											n = DS.object$obs.data[,2])
					out$y.matrix[i,]<- apply(samps.df, 1, 
										function(x) rbinom(1,size = x[2], prob = x[1]))
					}
				out$pred.type <- "prior"
			}
			out$first.set <- data.frame(y = out$y.matrix[1,], n = DS.object$obs.data[,2])
			out$bin <- 1
			return(out)
		},
		"Poisson" = {
			prior.fam <- "Gamma"
			out$y.matrix <- matrix(0, nrow = runs, ncol = length(DS.object$obs.data))
			if(is.null(exposure) == TRUE){
				if(pred.type == "posterior"){
					for(i in 1:runs){
						post.samps <- sapply(DS.object$obs.data, 
											function(x) DS.sampler.post(1,DS.object$g.par,
											DS.object$LP.par, y.0 = x, 
											con.prior = prior.fam, LP.type = DS.object$LP.type))
						out$y.matrix[i,]<- sapply(post.samps, 
											function(x) rpois(1,lambda = x))
						}
					out$pred.type <- "posterior"
					class(out) <- "PostPD"
				} else {
					for(i in 1:runs){
						prior.samps <- DS.sampler(k = length(DS.object$obs.data), g.par = DS.object$g.par, 
										   LP.par = DS.object$LP.par, con.prior = prior.fam, 
										   LP.type = DS.object$LP.type)  
						out$y.matrix[i,]<- sapply(prior.samps, 
											function(x) rpois(1,lambda = x))
						}
					out$pred.type <- "prior"
					}
				out$first.set <- out$y.matrix[1,]
				return(out)
			} else {
				if(pred.type == "posterior"){
					for(i in 1:runs){
					post.samps <- sapply(DS.object$obs.data, 
										function(x) DS.sampler.post(1,DS.object$g.par,
										DS.object$LP.par, y.0 = x, 
										con.prior = prior.fam, LP.type = DS.object$LP.type))
					samps.df <- data.frame(thet = post.samps, expv = exposure)
					out$y.matrix[i,]<- apply(samps.df, 1, 
										function(x) rpois(1,lambda = x[1]*x[2]))
						}
					out$pred.type <- "posterior"
					class(out) <- "PostPD"
				} else {
					for(i in 1:runs){
					prior.samps <- DS.sampler(k = length(DS.object$obs.data), g.par = DS.object$g.par, 
										   LP.par = DS.object$LP.par, con.prior = prior.fam, 
										   LP.type = DS.object$LP.type) 
					samps.df <- data.frame(thet = prior.samps, expv = exposure)
					out$y.matrix[i,]<- apply(samps.df, 1, 
										function(x) rpois(1,lambda = x[1]*x[2]))
					}
					out$pred.type <- "prior"
				}
				out$exposure <- exposure
				out$first.set <- out$y.matrix[1,]
				out$bin <- NULL
				return(out)
			}
			},
		"Normal" = {
			prior.fam <- "Normal"
			out$y.matrix <- matrix(0, nrow = runs, ncol = length(DS.object$obs.data[,1]))
			if(pred.type == "posterior"){
				for(i in 1:runs){
					post.samps <- apply(DS.object$obs.data, 1, 
									function(x) DS.sampler.post(1,DS.object$g.par,
										DS.object$LP.par, y.0 = x[1], n.0 = x[2], 
										con.prior = prior.fam, LP.type = DS.object$LP.type))
					samps.df <- data.frame(theta = post.samps, 
											se = DS.object$obs.data[,2])
					out$y.matrix[i,]<- apply(samps.df, 1, 
										function(x) rnorm(1,mean = x[1], sd = x[2]))
					}
					out$pred.type <- "posterior"
					class(out) <- "PostPD"
			} else {
				for(i in 1:runs){		
					prior.samps <- DS.sampler(k = dim(DS.object$obs.data)[1], g.par = DS.object$g.par, 
										   LP.par = DS.object$LP.par, 
										   con.prior = prior.fam, LP.type = DS.object$LP.type)  
					samps.df <- data.frame(theta = prior.samps, 
											se = DS.object$obs.data[,2])
					out$y.matrix[i,]<- apply(samps.df, 1, 
										function(x) rnorm(1,mean = x[1], sd = x[2]))
				}
				out$pred.type <- "prior"
			}
			out$first.set <- data.frame(y = out$y.matrix[1,], se = DS.object$obs.data[,2])
			out$se.vec <- DS.object$obs.data[,2]
			out$bin <- NULL
			return(out)
		}
		)
}