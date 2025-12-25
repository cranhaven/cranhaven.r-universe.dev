#' Perform MCMC algorithm to generate the posterior samples
#'
#' This function is used to generate the posterior samples using MCMC algorithm from the probit model 
#' with either the hypersphere decomposition or ARMA models applied to model the correlation structure 
#' in the serial dependence 
#' of repeated responses.
#'
#' @param fixed a two-sided linear formula object to describe fixed-effects with the response on the left of 
#' a \samp{~} operator and the terms separated by \samp{+} or \samp{*} operators, on the right. 
#' The specification \code{first*second} indicates the cross of \code{first} and \code{second}. 
#' This is the same as \code{first + second + first:second}.
#' @param data  an optional data frame containing the variables named in \samp{fixed} and \samp{random}. 
#' It requires an ``integer'' variable named by \samp{id} to denote the identifications of subjects. 
#' @param random a one-sided linear formula object to describe random-effects with the terms separated by 
#' \samp{+} or \samp{*} operators on the right of a \samp{~} operator.
#' @param Robustness logical. If 'TRUE' the distribution of random effects is assumed to be \cr
#' t-distribution; otherwise normal distribution. 
#' @param na.action a function that indicates what should happen when the data contain NA’s. 
#' The default action (\samp{na.omit}, inherited from the \samp{factory fresh} value of \cr
#' \samp{getOption("na.action")}) strips any observations with any missing values in any variables.
#' @param subset an optional expression indicating the subset of the rows of \samp{data} that should be used in the fit. 
#' This can be a logical vector, or a numeric vector indicating which observation numbers are to be included, 
#' or a character vector of the row names to be included.  All observations are included by default.
#' @param HS.model a specification of the correlation structure in HSD model: 
#' \itemize{
#'   \item \code{HS.model = ~0} denotes independence, that is, \eqn{R_i} is an identity matrix, 
#'   \item \code{HS.model = ~IndTime+}\eqn{\cdots}\code{+IndTimer} denotes AR(r) correlation structure, 
#'   \item \code{HS.model = ~DiffTime1+}\eqn{\cdots}\code{+DiffTimer} denotes correlation structure related to \eqn{r}th order 
#' of time difference. 
#' }
#' @param arma.order a specification of the order in an ARMA model: the two integer components (p, q) are the AR order and the MA order.
#' @param hyper.params specify the values in hyperparameters in priors. 
#' @param num.of.iter an integer to specify the total number of iterations; default is 20000.   
#' @param Interactive logical. If 'TRUE' when the program is being run interactively for progress bar and 'FALSE' otherwise.   
#'
#' @return a list of posterior samples, parameters estimates, AIC, BIC, CIC, DIC, MPL, RJR, predicted values, 
#' and the acceptance rates in MH are returned.
#'
#' @note Only a model either HSD (\samp{HS.model}) or ARMA (\samp{arma.order}) model should be specified in the function. 
#' We'll provide the reference for details of the model and the algorithm for performing 
#' model estimation whenever the manuscript is accepted. 
#'
#' @author Kuo-Jung Lee <kuojunglee@ncku.edu.tw> 
#' @references{
#'   \insertRef{Lee:etal:2021}{BayesRGMM} 
#'   
#'   \insertRef{Lee:etal:2020}{BayesRGMM}
#'
#'} 
#'
#' @examples
#' \dontrun{
#' library(BayesRGMM)
#' rm(list=ls(all=TRUE))
#' Fixed.Effs = c(-0.2, -0.3, 0.8, -0.4) #c(-0.2,-0.8, 1.0, -1.2)
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
#'  HSD.sim.data = SimulatedDataGenerator(
#'  Num.of.Obs = N, Num.of.TimePoints = T, Fixed.Effs = Fixed.Effs, 
#'  Random.Effs = list(Sigma = 0.5*diag(1), df=3), 
#'	Cor.in.DesignMat = 0., Missing = list(Missing.Mechanism = 2, 
#'  RegCoefs = c(-1.5, 1.2)), Cor.Str = "HSD", 
#'  HSD.DesignMat.para = list(HSD.para = HSD.para, DesignMat = w))
#' 
#' hyper.params = list(
#'         sigma2.beta = 1,
#'         sigma2.delta = 1,
#'         v.gamma = 5,
#'         InvWishart.df = 5,
#'         InvWishart.Lambda = diag(q) )
#' 
#' HSD.output = BayesRobustProbit(
#' fixed = as.formula(paste("y~-1+", paste0("x", 1:P, collapse="+"))), 
#' data=HSD.sim.data$sim.data, random = ~ 1, Robustness=TRUE, 
#' HS.model = ~IndTime1+IndTime2, subset = NULL, na.action='na.exclude', 
#' hyper.params = hyper.params, num.of.iter = num.of.iter, 
#'  Interactive=0)
#' } 

BayesRobustProbit = function(fixed, data, random, Robustness=TRUE, subset=NULL, na.action='na.exclude', arma.order=NULL, 
	                         HS.model=NULL, hyper.params = NULL, num.of.iter=20000, Interactive=FALSE)
{ 

	
	#cat("\nCall:\n", printCall(match.call()), "\n\n", sep = "")

	if(length(arma.order)==0 && length(HS.model)==0)
		stop("Please specify at least one model for the correlation structure!!")
	if(length(arma.order)!=0 && length(HS.model)!=0)
		stop("Please specify only one model for the correlation structure!!")

	if( length(unique(na.omit(data[, names(data)==all.vars(fixed)[1]])))>2 ){
		output = do.call("BayesCumulativeProbitHSD", list(fixed=fixed, data=data, random=random, Robustness=Robustness, 
				subset=subset, na.action=na.action, HS.model=HS.model, hyper.params = hyper.params, 
				num.of.iter=num.of.iter, Interactive = Interactive))
	}
	else{
		if(length(arma.order)>0)
			output = do.call("BayesProbitARMA", list(fixed=fixed, data=data, random=random, Robustness=Robustness, 
				subset=subset, na.action=na.action, arma.order=arma.order, hyper.params = hyper.params, 
				num.of.iter=num.of.iter, Interactive = Interactive))
		if(length(HS.model)>0)
			output = do.call("BayesProbitHSD", list(fixed=fixed, data=data, random=random, Robustness=Robustness, 
				subset=subset, na.action=na.action, HS.model=HS.model, hyper.params = hyper.params, 
				num.of.iter=num.of.iter, Interactive = Interactive))
			#output = BayesProbitHSD(fixed, data, random, HS.model, subset=NULL, na.action, num.of.iter)
	}

	output$call$data = deparse(substitute(data))
	#print(output$call$data)
	output
}