lca <- function(data, nclasses, fit=TRUE, family=NULL, verbose=FALSE, ...) {
	
	# univariate numeric data by default treated as gaussian
 	if(is.vector(data)) {
		nm <- deparse(substitute(data))
		form <- as.formula(paste(nm,"~1",sep=""))
		if(is.null(family)) family=gaussian()
		simple <- 0
		data <- data.frame(nm=data)
	} else {
		# univariate factor data treated as multinomial
 		if(is.factor(data)) {
			nm <- deparse(substitute(data))
			form <- as.formula(paste(nm,"~1",sep=""))
			if(is.null(family)) family=multinomial("identity")
			simple <- 1
			data <- data.frame(nm=data)
		} else {
			# multivariate data
			form <- list()
			fam <- list()
			nc <- ifelse(!is.null(dim(data)), dim(data)[2],1)
			simple <- numeric(nc)
			for(i in 1:nc) {
				form[[i]] <- as.formula(paste(names(data)[i],"~1",sep=""))
				fam[[i]] <- switch(class(data[,i]),
					"numeric"=gaussian(),
					"factor"=multinomial("identity"),
					stop("Provide family arguments of data other than 'numeric' or 
						'factor's"))
				simple[i] <- switch(class(data[,i]),
					"numeric"=0,
					"factor"=1)
			}
			if(is.null(family)) family=fam
			data <- data
		}
	}
		
	mod <- mix(response=form,data=data,nstates=nclasses,family=family, ...)
	attr(mod,"type") <- "lca"
	if(fit) res <- fit(mod, emcontrol=em.control(maxit=500), verbose=verbose)
	else res <- mod
	return(res)
}

hmm <- function(data, nstates, fit=TRUE, ntimes=NULL, family=NULL, verbose=FALSE, ...) {
	
	# univariate numeric data by default treated as gaussian
 	if(is.vector(data)) {
		nm <- deparse(substitute(data))
		form <- as.formula(paste(nm,"~1",sep=""))
		if(is.null(family)) family=gaussian()
		if(is.null(ntimes)) ntimes <- length(data)
		simple <- 0
		data <- NULL
	} else {
		# univariate factor data treated as multinomial
 		if(is.factor(data)) {
			nm <- deparse(substitute(data))
			form <- as.formula(paste(nm,"~1",sep=""))
			if(is.null(ntimes)) ntimes <- length(data)
			if(is.null(family)) family=multinomial("identity")
			simple <- 1
			data <- NULL
		} else {
			# multivariate data
			form <- list()
			fam <- list()
			nc <- ifelse(!is.null(dim(data)), dim(data)[2],1)
			simple <- numeric(nc)
			for(i in 1:nc) {
				form[[i]] <- as.formula(paste(names(data)[i],"~1",sep=""))
				fam[[i]] <- switch(class(data[,i]),
					"numeric"=gaussian(),
					"factor"=multinomial("identity"),
					stop("Provide family arguments of data other than 'numeric' or 
						'factor's"))
				simple[i] <- switch(class(data[,i]),
					"numeric"=0,
					"factor"=1)
			}
			if(is.null(family)) family=fam
			if(is.null(ntimes)) ntimes=nrow(data)
			data <- data
		}
	}
		
	mod <- depmix(response=form,data=data,nstates=nstates,ntimes=ntimes,family=family)
	attr(mod,"type") <- "hmm"
	if(fit) res <- fit(mod, emcontrol=em.control(maxit=500), verbose=verbose)
	else res <- mod
	return(res)
}


