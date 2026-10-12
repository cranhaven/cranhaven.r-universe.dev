## Parameter functions used in the simulation of the regression model
generation.alpha.3strains <- function(x) {
	alpha=NULL
	alpha <- cbind(alpha,(cos(x[,2]) + (3/2))*100)
	alpha <- cbind(alpha,(sin(x[,1]) + (3/2))*100)
	alpha <- cbind(alpha,(sin(x[,2]) + (3/2))*100)
	return(alpha)
}

## Function generating draws from the Dirichlet distribution
## code taken from the R-package "gregmisc"
.rdirichlet=function (n, alpha){
    l <- length(alpha)
    x <- matrix(rgamma(l * n, alpha), ncol = l, byrow = TRUE)
    sm <- x %*% rep(1, l)
    return(x/as.vector(sm))
}

## Function providing the intensity of the risk of infection
.infection.potential=function(par,r,y){
  return(colSums(y*par[1]*exp(-r/par[2])))
}

## Epanechnikov smoothing kernel
.noyau <- function(u,kernel.type) { 
	if(kernel.type=="Linear"){
		out= (1 - u)*  (u >= 0 & u <= 1)  
	}
	if(kernel.type=="Quadratic"){
		out= (1 - u^2)*  (u >= 0 & u <= 1)  
	}
	if(kernel.type=="Power3"){
		out= (1 - u^3)*  (u >= 0 & u <= 1)  
	}
	if(kernel.type=="Power4"){
		out= (1 - u^4)*  (u >= 0 & u <= 1)  
	}
	return(out)
}

## Function computing the weights of the kernel smoothing 
.calcul.w <- function(jeu,xy,bw,kernel.type){ 
	dist=sqrt(outer(jeu[,1],xy[,1],"-")^2+outer(jeu[,2],xy[,2],"-")^2)
	return(.noyau(dist/bw,kernel.type))
} 

## Function estimating proportions of strains
.estimation.pS=function(jeu,xy,bw,kernel.type){
	w=.calcul.w(jeu,xy,bw,kernel.type)
	out=matrix(0,nrow = nrow(xy), ncol = ncol(jeu)-2)
	prop=jeu[,3:ncol(jeu)]/rowSums(jeu[,3:ncol(jeu)])
	for (i in 1:nrow(xy)) { 
		out[i,]=as.numeric(colSums(w[,i]%*%prop)/sum(w[,i]))
	}
	return(rbind(out))
}
	
	
