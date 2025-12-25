#' Simulating a longitudinal ordinal data with HSD correlation structures. 
#'
#' This function is used to simulate data for the cumulative probit mixed-effects model with HSD correlation structures. 
#'
#' @param Num.of.Obs the number of subjects will be simulated. 
#' @param Num.of.TimePoints the maximum number of time points among all subjects. 
#' @param Num.of.Cats the number of categories. 
#' @param Fixed.Effs a vector of regression coefficients. 
#' @param Random.Effs a list of covariance matrix and the degree of freedom, \cr
#' e.g., \code{list(Sigma = 0.5*diag(1), df=3)}. 
#' @param DesignMat a design matrix. 
#' @param Missing a list of the missing mechanism of observations, 0: data is complete, 1: missing complete at random, 2: missing at random related to responses , and 3: 2: missing at random related to covariates and the corresponding regression coefficients (weights) on the previous observed values either responses or covariates, e.g.,  \code{Missing = list(Missing.Mechanism = 3, RegCoefs = c(0.4, 1.2, -2.1))}. 
#' @param HSD.DesignMat.para the list of parameters in HSD correlation structure, \cr
#' e.g., \code{HSD.DesignMat.para = list(HSD.para = HSD.para, DesignMat = w)}. 
#'
#' @return a list containing the following components:
#' \describe{
#'   \item{sim.data}{The simulated response variables \eqn{y}, covariates \eqn{x}'s, and subject identifcation \samp{id}.}
#'   \item{beta.true}{The given values of fixed effects.}
#'   \item{classes}{The intervals of classes.}
#'   \item{HSD.para}{The given values of parameters in HSD model.}
#' }
#' @examples
#' \dontrun{
#' library(BayesRGMM)
#' rm(list=ls(all=TRUE))
#' 
#' Fixed.Effs = c(-0.1, 0.1, -0.1)  
#' P = length(Fixed.Effs) 
#' q = 1 #number of random effects
#' T = 7 #time points
#' N = 100 #number of subjects
#' Num.of.Cats = 3 #number of categories 
#' num.of.iter = 1000 #number of iterations
#' 
#' HSD.para = c(-0.9, -0.6) #the parameters in HSD model
#' a = length(HSD.para)
#' w = array(runif(T*T*a), c(T, T, a)) #design matrix in HSD model
#'  
#' for(time.diff in 1:a)
#' w[, , time.diff] = 1*(as.matrix(dist(1:T, 1:T, method="manhattan")) 
#' ==time.diff)
#' 
#' 
#' x = array(0, c(T, P, N))
#' for(i in 1:N){
#'     x[, 1, i] = 1:T
#'     x[, 2, i] = rbinom(1, 1, 0.5)
#'     x[, 3, i] = x[, 1, i]*x[, 2, i]
#' }
#' 
#' DesignMat = x
#' 
#' #MAR
#' CPREM.sim.data = SimulatedDataGenerator.CumulativeProbit(
#'  Num.of.Obs = N, Num.of.TimePoints = T, Num.of.Cats = Num.of.Cats, 
#'  Fixed.Effs = Fixed.Effs, Random.Effs = list(Sigma = 0.5*diag(1), df=3), 
#'  DesignMat = DesignMat, Missing = list(Missing.Mechanism = 2, 
#'  MissingRegCoefs=c(-0.7, -0.2, -0.1)), 
#'  HSD.DesignMat.para = list(HSD.para = HSD.para, DesignMat = w))
#' 
#' 
#' print(table(CPREM.sim.data$sim.data$y))
#' print(CPREM.sim.data$classes)
#' }



SimulatedDataGenerator.CumulativeProbit = function(Num.of.Obs, Num.of.TimePoints, Num.of.Cats, Fixed.Effs, Random.Effs, 
	DesignMat, Missing, HSD.DesignMat.para)
{
	P = Num.of.Fixed.Effs = length(Fixed.Effs)
	q = Num.of.Random.Effs = dim(Random.Effs$Sigma)[1]
	T = Num.of.TimePoints
	N = Num.of.Obs

	MissingMechanism = Missing$Missing.Mechanism #0: complete, 1: MCAR, 2: MAR with Y, 3:MAR with X
	if(MissingMechanism == 1)
		MissingRegCoefs = Missing$Probs # probs or weights
	
	if(MissingMechanism == 2)
		MissingRegCoefs = Missing$RegCoefs
	
	beta.true = matrix(Fixed.Effs, ncol=1) #matrix(rnorm(P, 0, 1), ncol=1)#matrix(c(-1.2,-0.3, 0.8, -0.4), ncol=1)

	x = array(0, c(T, P, N))
	z = array(0, c(T, q, N))
	y.star = matrix(0, T, N)

	df = Random.Effs$df

	nu = rgamma(N, df/2, df/2) #rep(1, N) #

	 
	b = NULL
	Sigma = Random.Effs$Sigma#*diag(q)#0.5*diag(q)
	for(i in 1:N)
		b = cbind(b, t(rmvnorm(1, rep(0, q), Sigma /nu[i])))
#=====================================================================================================#

	HSD.para = HSD.DesignMat.para$HSD.para
	a = length(HSD.para)

	w = HSD.DesignMat.para$DesignMat #array(runif(T*T*a), c(T, T, a))

	Ri=CorrMat.HSD(w, HSD.para)

	x = DesignMat

	if(dim(x)[2] != length(beta.true))
		stop("Check the size of design matrix and regressors!")

	for(i in 1:N){
		#x[,, i] = t(rmvnorm(P, rep(0, T), AR1.cor(T, Cor.in.DesignMat)))
		#x[, 1, ] = 1 
		#x[, 2, i] = rbinom(1, 1, 0.5)
		z[, 1, ] = 1
		y.star[, i] = x[,, i] %*% beta.true + z[, , i]%*%b[, i, drop=F] + t(rmvnorm(1, rep(0, T), Ri))
	}

    cut.points = quantile(y.star, seq(0, 1, length=Num.of.Cats+1))
    #seq(range(y.star)[1], range(y.star)[2], length = Num.of.Cats+1)
    #quantile(yy, seq(0, 1, length=4))
	y = matrix(as.numeric(cut(y.star, cut.points, include.lowest=TRUE)), dim(y.star)) 

    #alpha.ini = c(-Inf, tail(head(cut.points, -1), -1), Inf)
    #alpha.ini = c(-Inf, seq(-5, 5, length = Num.of.Cats-1), Inf)

#=====================================================================================================#
	sim.data = as.data.frame(cbind(c(y), adply(x, 3)))
	colnames(sim.data) = c("y", "id", paste0("x", 1:P))
	sim.data$id = as.numeric(sim.data$id)
#=====================================================================================================#

	TimePointsAvailable = rep(T, N)
	if(MissingMechanism==1)
		TimePointsAvailable =  sample(1:T, N, replace = TRUE, prob = MissingRegCoefs)#c(423, 208, 263, 1416))# rep(T, N)
	if(MissingMechanism==2){
		TimePointsAvailable.MAR = TimePointsAvailable
		post.time = length(MissingRegCoefs)
		for(i in 1:N){
			for(t1 in 1:TimePointsAvailable[i]){
		  		if(t1>post.time){
		    		eta = sum(MissingRegCoefs*y[(t1-1):(t1-post.time), i])#MissingRegCoefs[1] + MissingRegCoefs[2]*y[t1-1,i]
		    		pdrop = exp(eta)/(1+exp(eta))
		    		#cat("MissingRegCoefs = ", MissingRegCoefs, "\teta = ", eta, "\tpdrop=", pdrop, "\n")
		    		if(runif(1)<pdrop)
		    			TimePointsAvailable.MAR[i]<-t1-1
		  		}
			}
		}
		TimePointsAvailable = TimePointsAvailable.MAR
	}


	#print(TimePointsAvailable)
	for(i in 1:N)
		sim.data[sim.data$id==i, ][(1:T)>TimePointsAvailable[i], names(sim.data)!="id"] = NA

	simdata = list(sim.data = sim.data, beta.true = beta.true, classes = cut.points, HSD.para = HSD.para)

	invisible(simdata)
}




