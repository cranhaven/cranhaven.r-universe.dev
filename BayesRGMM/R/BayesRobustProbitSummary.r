#' To summarizes model estimation outcomes
#'
#' It provides basic posterior summary statistics such as the posterior point and 
#' confidence interval estimates of parameters and the values of  information criterion statistics for model comparison. 
#'
#' @param object output from the function \code{BayesRobustProbit}. 
#' @param digits rounds the values in its first argument to the specified
#'     number of significant digits.
#' @return a list of posterior summary statistics and corresponding model information 
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
#'  == time.diff)
#' 
#' #Generate a data with HSD model
#' HSD.sim.data = SimulatedDataGenerator(Num.of.Obs = N, Num.of.TimePoints = T, 
#'	Fixed.Effs = Fixed.Effs, Random.Effs = list(Sigma = 0.5*diag(1), df=3), 
#'	Cor.in.DesignMat = 0., Missing = list(Missing.Mechanism = 2, 
#'  RegCoefs = c(-1.5, 1.2)), Cor.Str = "HSD", 
#'	HSD.DesignMat.para = list(HSD.para = HSD.para, DesignMat = w))
#' 
#' hyper.params = list(
#'         sigma2.beta = 1,
#'         sigma2.delta = 1,
#'         v.gamma = 5,
#'         InvWishart.df = 5,
#'         InvWishart.Lambda = diag(q) )
#'
#' HSD.output = BayesRobustProbit(
#'  fixed = as.formula(paste("y~-1+", paste0("x", 1:P, collapse="+"))), 
#' 	data=HSD.sim.data$sim.data, random = ~ 1, Robustness=TRUE, 
#'  HS.model = ~IndTime1+IndTime2, subset = NULL, na.action='na.exclude', 
#'  hyper.params = hyper.params, num.of.iter = num.of.iter, Interactive =0)
#' 
#' BayesRobustProbitSummary(HSD.output)
#' }


BayesRobustProbitSummary = function(object, digits = max(1L, getOption("digits") - 4L))
{
	
	if(object$call[1]!="BayesProbitHSD()" & object$call[1]!="BayesProbitARMA()" & object$call[1]!="BayesCumulativeProbitHSD()")
		stop("Please input the correct object!!")
	if(object$call[1] == "BayesProbitHSD()")
		output = BayesProbitHSD.Summary(object, digits = digits)
	if(object$call[1] == "BayesProbitARMA()")
		output = BayesProbitARMA.Summary(object, digits = digits)
	if(object$call[1] == "BayesCumulativeProbitHSD()")
		output = BayesProbitHSD.Summary(object, digits = digits)
	output
}