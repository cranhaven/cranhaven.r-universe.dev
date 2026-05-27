weight.fun.univ <-
function(u, alpha_0, beta_0, alpha.post.i, beta.post.i, 
		 family = c("Normal","Binomial", "Poisson")){
	fam = match.arg(family)
	switch(fam,
		"Normal" = {
			if(beta.post.i == 0){
				ratio = rep(0,length(u))
				return(ratio)
			} else {
				v1 <- dnorm(qnorm(u,mean = alpha_0,sd = sqrt(beta_0)), 
					  mean = alpha.post.i, sd = sqrt(beta.post.i))
				v2 <- dnorm(qnorm(u,mean = alpha_0,sd = sqrt(beta_0)), 
					  alpha_0, sd = sqrt(beta_0))
			ratio <- v1/v2
			return(ratio)
			}
		 },
		 "Binomial" = {
		 	if(alpha.post.i <= 0 | beta.post.i <=0){
				ratio = rep(0,length(u))
				return(ratio)
			} else {
				v1 <- dbeta(qbeta(u,shape1 = alpha_0, shape2 = beta_0), 
						shape1 = alpha.post.i, shape2 = beta.post.i)
				v2 <- dbeta(qbeta(u,shape1 = alpha_0, shape2 = beta_0), 
						shape1 = alpha_0, shape2 = beta_0)
				ratio <- v1/v2
				ratio[is.na(ratio)]<-0 #handles cases where first and last of both are 0
				return(ratio)
			}
		 },
		 "Poisson" = {
			if(alpha.post.i <= 0 | beta.post.i <=0){
				ratio = rep(0,length(u))
				return(ratio)
			} else {
				v1 <- dgamma(qgamma(u,shape = alpha_0, scale = beta_0), 
						shape = alpha.post.i, scale = beta.post.i)
				v2 <- dgamma(qgamma(u, shape = alpha_0, scale = beta_0), 
						shape = alpha_0, scale = beta_0)
				ratio <- v1/v2
				ratio[is.na(ratio)]<-0
				return(ratio)
			}
		 }
		)
	}