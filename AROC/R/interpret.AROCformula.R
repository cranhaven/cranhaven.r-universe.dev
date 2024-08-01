interpret.AROCformula <-
function(formula, data) {
	env <- environment(formula) 
	if(inherits(formula, "character"))		  
		formula <- as.formula(formula)

	tf <- terms.formula(formula, specials = c("f"))
	if (attr(tf, "response") > 0) {
		marker <- as.character(attr(tf, "variables")[2])
	} else {
		stop("The formula should include the response variable (left hand side)")
	}

	terms <- attr(tf, "term.labels")
	#if(length(grep(":",terms)) != 0)  stop("Symbol '*' is not allowed")
	
	nt <- length(terms)	 
	ns <- attr(tf,"specials")$f - 1 # Marker is in the formula
	II <- list()
	h  <- list()
	K <- list()
	partial <- vector()
	k <- 0
	data.cov <- data[,names(data) %in% all.vars(formula)[-1], drop = FALSE]
	if(nt) {
		for (i in 1:nt) {
			if (i %in% ns) {
				k <- k+1				   
				st <- eval(parse(text = paste("AROC.",terms[i],sep="")))
				II[[k]] <- st$cov
                h[[k]] <- -1
                K[[k]] <- st$K
                partial[k] <- terms[i]														  
			} else {
				k <- k + 1
                II[[k]]<- c(-1, terms[i])
                h[[k]] <- 0 # parametric
                K[[k]] <- 0
                partial[k] <- terms[i]
			}
		}		   
	} else { # Only the intercept
        data.cov <- NULL
    }	   
	II <- if(length(II)) {
		matrix(unlist(II), nrow = 2)
	} else {
		matrix(0, nrow = 2)
	}	   
	res <- list(marker = marker, II = II, h = unlist(h), K = K, npartial = k, partial = partial, data.cov = data.cov)
	res
}
