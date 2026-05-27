gLP.basis <-
function(x, g.par, m, con.prior = c("Normal", "Beta", "Gamma"),ind = NULL){
#######################################
## INPUTS
##	x			set of values OR single value between (0,1)
##  g.par		Parameters for the beta parametric prior G
##  m			selected m value
##  ind			Extracts the jth column of matrix; ow entire matrix 
##  con.prior	Normal, Beta, or Gamma basis
##	
## OUTPUTS
##  TY      functional values for the first m legendre polynomials
##			evaluated over x
#######################################
	fam = match.arg(con.prior)
	switch(fam,
		"Normal" = {
			#LP.basis.norm(x, g.par, m, ind)
			u <- pnorm(x, g.par[1], sd = sqrt(g.par[2]))
			poly <-  slegendre.polynomials(m,normalized=TRUE)  
			TY <- matrix(NA,length(u),m)
			for(j in 1:m) TY[,j] <- predict(poly[[j+1]],u)
			if(is.numeric(ind) == FALSE){
			return(TY)
			}else{
			return(TY[,ind])
			}
		 },
		 "Beta" = {
			#LP.basis.beta(x, g.par, m, ind)
			u <- pbeta(x, g.par[1], g.par[2])
			poly <-  slegendre.polynomials(m,normalized=TRUE)  
			TY <- matrix(NA,length(u),m)
			for(j in 1:m) TY[,j] <- predict(poly[[j+1]],u)
			if(is.numeric(ind) == FALSE){
			return(TY)
			}else{
			return(TY[,ind])
			}	
		 },
		 "Gamma" = {
			#LP.basis.gamma(x, g.par, m, ind)
			u <- pgamma(x, shape = g.par[1], scale = g.par[2])
			poly <-  slegendre.polynomials(m,normalized=TRUE)  
			TY <- matrix(NA,length(u),m)
			for(j in 1:m) TY[,j] <- predict(poly[[j+1]],u)
			if(is.numeric(ind) == FALSE){
			return(TY)
			}else{
			return(TY[,ind])
			}
			 }
		)
	}