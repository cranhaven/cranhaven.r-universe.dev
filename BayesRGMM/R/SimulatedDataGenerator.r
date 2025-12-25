#' Generate simulated data with either ARMA or MCD correlation structures. 
#'
#' This function is used to generate simulated data for simulation studies with ARMA and MCD correlation structures. 
#'
#' @param Num.of.Obs the number of subjects will be simulated. 
#' @param Num.of.TimePoints the maximum number of time points among all subjects. 
#' @param Fixed.Effs a vector of regression coefficients. 
#' @param Random.Effs a list of covariance matrix and the degree of freedom, \cr 
#' e.g., \code{list(Sigma = 0.5*diag(1), df=3)}. 
#' @param Cor.in.DesignMat the correlation of covariates in the design matrix. 
#' @param Missing a list of the missing mechanism of observations, 0: data is complete, 1: missing complete at random, 2: missing at random related to responses , and 3: 2: missing at random related to covariates and the corresponding regression coefficients (weights) on the previous observed values either responses or covariates, e.g.,  \code{Missing = list(Missing.Mechanism = 3, RegCoefs = c(0.4, 1.2, -2.1))}. 
#' @param Cor.Str the model for correlation structure, ``ARMA'' or ``HSD''.
#' @param HSD.DesignMat.para if \code{Cor.Str="HSD"}, you need to specify the list of parameters in HSD correlation structure, \cr
#' e.g., \code{HSD.DesignMat.para = list(HSD.para = HSD.para, DesignMat = w)}. 
#' @param ARMA.para if \code{Cor.Str="ARMA"}, you need to specify the list of parameters in AMRA correlation structure, e.g., \code{ARMA.para = list(AR.para=0.1, MA.para=0.2)}. 
#'
#' @return a list containing the following components:
#' \describe{
#'   \item{sim.data}{The simulated response variables \eqn{y}, covariates \eqn{x}'s, and subject identifcation \samp{id}.}
#'   \item{beta.true}{The given values of fixed effects .}
#'   \item{ARMA.para}{The given values of parameters in ARMA model.}
#'   \item{HSD.para}{The given values of parameters in ARMA model.}
#' }
#' @examples
#' \dontrun{
#' library(BayesRGMM)
#' rm(list=ls(all=TRUE))
#' Fixed.Effs = c(-0.2, -0.3, 0.8, -0.4) 
#' P = length(Fixed.Effs) 
#' q = 1 #number of random effects
#' T = 5 #time points
#' N = 100 #number of subjects
#' num.of.iter = 100 #number of iterations

#' HSD.para = c(-0.5,  -0.3) #the parameters in HSD model
#' a = length(HSD.para)
#' w = array(runif(T*T*a), c(T, T, a)) #design matrix in HSD model
#' 
#' for(time.diff in 1:a)
#' 	w[, , time.diff] = 1*(as.matrix(dist(1:T, 1:T, method="manhattan")) 
#'  ==time.diff)
#' 
#' #Generate a data with HSD model
#' HSD.sim.data = SimulatedDataGenerator(Num.of.Obs = N, Num.of.TimePoints = T, 
#'	Fixed.Effs = Fixed.Effs, Random.Effs = list(Sigma = 0.5*diag(1), df=3), 
#'	Cor.in.DesignMat = 0., Missing = list(Missing.Mechanism = 2, 
#'  RegCoefs = c(-1.5, 1.2)), Cor.Str = "HSD", 
#'  HSD.DesignMat.para = list(HSD.para = HSD.para, DesignMat = w))
#' 
#' #the proportion of 1's
#' ones = sum(HSD.sim.data$sim.data$y==1, na.rm=T)
#' num.of.obs = sum(!is.na(HSD.sim.data$sim.data$y))
#' print(ones/num.of.obs) 
#' 
#' #the missing rate in the simulated data 
#' print(sum(is.na(HSD.sim.data$sim.data$y)))
#' 
#'
#' #===========================================================================#
#' #Generate a data with ARMA model
#' ARMA.sim.data = SimulatedDataGenerator(Num.of.Obs = N, Num.of.TimePoints = T, 
#' 	Fixed.Effs = Fixed.Effs, Random.Effs = list(Sigma = 0.5*diag(1), df=3), 
#' 	Cor.in.DesignMat = 0., list(Missing.Mechanism = 2, RegCoefs = c(-1.5, 1.2)), 
#' 	Cor.Str = "ARMA", ARMA.para=list(AR.para = 0.8))
#' }



SimulatedDataGenerator = function(Num.of.Obs, Num.of.TimePoints, Fixed.Effs, Random.Effs, 
	Cor.in.DesignMat, Missing, Cor.Str, HSD.DesignMat.para, ARMA.para)
{
	P = Num.of.Fixed.Effs = length(Fixed.Effs)
	q = Num.of.Random.Effs = dim(Random.Effs$Sigma)[1]
	T = Num.of.TimePoints
	N = Num.of.Obs
	MissingMechanism = Missing$Missing.Mechanism #0: complete, 1: MCAR, 2: MAR with Y, 3:MAR with X
	if(MissingMechanism == 1)
		MissingRegCoefs = Missing$Probs
	
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
	if(Cor.Str == "HSD"){
		#delta = HSD.para #c(-0.5, -0.3)
		HSD.para = HSD.DesignMat.para$HSD.para
		a = length(HSD.para)

		w = HSD.DesignMat.para$DesignMat #array(runif(T*T*a), c(T, T, a))

		#for(time.diff in 1:a)
		#	w[, , time.diff] = 1*(as.matrix(dist(1:T, 1:T, method="manhattan")) ==time.diff)


		Ri=CorrMat.HSD(w, HSD.para)

		for(i in 1:N){
			x[,, i] = t(rmvnorm(P, rep(0, T), AR1.cor(T, Cor.in.DesignMat)))
			x[, 1, ] = 1 
			x[, 2, i] = rbinom(1, 1, 0.5)
			z[, 1, ] = 1
			y.star[, i] = x[,, i] %*% beta.true + z[, , i]%*%b[, i, drop=F] + t(rmvnorm(1, rep(0, T), Ri))
		}

		#prob = pnorm(y.star)

		y = 1*(y.star>0) #apply(prob, 1:2, rbinom, n=1, size=1)

	}
#=====================================================================================================#
	if(Cor.Str == "ARMA"){
		e = matrix(0, T, N)
		AR.para = ARMA.para$AR.para
		MA.para = ARMA.para$MA.para
		AR.order = length(AR.para)
		MA.order = length(MA.para)
		
		

		if(AR.order>0)
			phi.true = matrix(rep(AR.para, N), AR.order, N)
		if(MA.order>0)
			psi.true = matrix(rep(MA.para, N), MA.order, N)


		ARMAorder = c(AR.order, MA.order)

		for(i in 1:N){
			x[,, i] = t(rmvnorm(P, rep(0, T), AR1.cor(T, Cor.in.DesignMat)))
			x[, 1, ] = 1 
			x[, 2, i] = rbinom(1, 1, 0.5)
			z[, 1, ] = 1
			for(t in 1:T){
				if(t<=max(ARMAorder)){
					#cat("max ARMA>0\n")
					e[t, i] = rnorm(1)
					y.star[t, i] = x[t,, i] %*% beta.true + z[t, , i]%*%b[, i, drop=F] + e[t, i]
				}
				else if( (AR.order == 0) & (MA.order == 0) ){
					e[t, i] = rnorm(1)
					y.star[t, i] = x[t,, i] %*% beta.true + z[t, , i]%*%b[, i, drop=F]+ e[t, i]
					              
				}
				else if( (AR.order > 0) & (MA.order == 0) ){
					#cat("AR>0\n")
					e[t, i] = rnorm(1)
					y.star[t, i] = x[t,, i] %*% beta.true + z[t, , i]%*%b[, i, drop=F]+
					               sum(phi.true[, i]*(y.star[(t-1):(t-AR.order), i]-x[(t-1):(t-AR.order),, i] %*% beta.true)) + 
					               + e[t, i]
				}
				else if( (AR.order == 0) & (MA.order > 0) ){
					#cat("MA>0\n")
					e[t, i] = rnorm(1)
					y.star[t, i] = x[t,, i] %*% beta.true + z[t, , i]%*%b[, i, drop=F]+
					               + sum(psi.true[, i]*e[(t-1):(t-MA.order), i]) + e[t, i]
					#cat("i = ", i, "\tt = ", t, "\n")
					#cat("psi.true[, i] = ", psi.true[, i], "\t", "e[(t-1):(t-MA.order), i] = ", e[(t-1):(t-MA.order), i], "\n")
					#cat("sum(psi.true[, i]*e[(t-1):(t-MA.order), i]) = ", sum(psi.true[, i]*e[(t-1):(t-MA.order), i]), "\n")
				}
				else if( (AR.order > 0) & (MA.order > 0) ){
					#cat("ARMA>0\n")
					e[t, i] = rnorm(1)
					y.star[t, i] = x[t,, i] %*% beta.true + z[t, , i]%*%b[, i, drop=F]+
					               sum(phi.true[, i]*(y.star[(t-1):(t-AR.order), i]-x[(t-1):(t-AR.order),, i] %*% beta.true)) + 
					               sum(psi.true[, i]*e[(t-1):(t-MA.order), i]) + e[t, i]
				}

				
			}
		}
		y = 1*(y.star>0)
	}

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

	if(0){
	if(MissingMechanism==3){
			TimePointsAvailable.MAR = TimePointsAvailable
			for(i in 1:N){
				for(t1 in 1:TimePointsAvailable[i]){
			  		if(t1>1){
			    		eta <- MissingRegCoefs[1]*x[t1-1,2, i]+MissingRegCoefs[2]*x[t1-1,3, i]+MissingRegCoefs[3]*x[t1-1,4, i]
			    		pdrop <-exp(eta)/(1+exp(eta))
			    		print(pdrop)
			    		if(runif(1)<pdrop)
			    			TimePointsAvailable.MAR[i]<-t1-1
			  		}
				}
			}
		TimePointsAvailable = TimePointsAvailable.MAR
	}
	}

	#print(TimePointsAvailable)
	for(i in 1:N)
		sim.data[sim.data$id==i, ][(1:T)>TimePointsAvailable[i], names(sim.data)!="id"] = NA

	if(Cor.Str == "HSD")
		simdata = list(sim.data = sim.data, beta.true = beta.true, HSD.para = HSD.para)
	if(Cor.Str == "ARMA")
		simdata = list(sim.data = sim.data, beta.true = beta.true, ARMA.para = ARMA.para)

	invisible(simdata)
}




