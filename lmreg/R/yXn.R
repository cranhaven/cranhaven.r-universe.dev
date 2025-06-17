yXn <- 
  function(response, group, subgroup) {
	  n <- length(response)
	  gr <- unique(group)
	  ngr <- length(gr)
	  Nsgr <- length(unique(group*n + subgroup))
	  X <- NULL
	  y <- NULL
	  nfillsgr <- 0
	  for (i in 1:ngr) {
		  sgr <- unique(subgroup[group==gr[i]])
		  nsgr <- length(sgr)
		  for (j in 1:nsgr) {
			  yij <- response[group==gr[i]&subgroup==sgr[j]]
			  y <- c(y,yij)
			  nij <- length(yij)
			  xij <- c(1, rep(0, (ngr+Nsgr)))
			  xij[1+i] <- 1
			  xij[1+ngr+nfillsgr+j] <- 1
			  X <- rbind(X, rep(1,nij)%*%t(xij))
	  	}
	  	nfillsgr <- nfillsgr + nsgr
  	}
  	return(list(y, X))
  }
