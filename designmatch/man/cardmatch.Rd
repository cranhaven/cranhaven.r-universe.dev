\name{cardmatch}

\alias{cardmatch}

\title{Optimal cardinality matching in observational studies}

\description{
	Function for optimal cardinality matching in observational studies.  \code{cardmatch} finds the largest matched sample that is balanced and representative by design.  The formulation of \code{cardmatch} has been simplified to handle larger data than \code{bmatch} or \code{nmatch}.  Similar to \code{bmatch} or \code{nmatch}, the performance of \code{cardmatch} is greatly enhanced by using the \code{solver} options \code{cplex} or \code{gurobi}.
}

\usage{
	cardmatch(t_ind, mom = NULL, fine = NULL, solver = NULL)
}

\arguments{

\item{t_ind}{treatment indicator: a vector of zeros and ones indicating treatment (1 = treated; 0 = control).  Please note that the data needs to be sorted in decreasing order according to this treatment indicator.}

\item{mom}{moment balance parameters: a list with three arguments, 

\code{mom = list(covs = mom_covs, tols = mom_tols, targets = mom_targets)}.

\code{mom_covs} is a matrix where each column is a covariate whose mean is to be balanced.  \code{mom_tols} is a vector of tolerances for the maximum difference in means for the covariates in \code{mom_covs}.  \code{mom_targets} is a vector of target moments (e.g., means) of a distribution to be approximated by matched sampling.  If \code{mom_targets} is specified, then \code{cardmatch} will select units from each treatment group so that each matched group differs at most by \code{mom_tols} units from the respective moments in \code{mom_targets}.  As a result, the matched groups will differ at most \code{mom_tols * 2} from each other.  Under certain assumptions, \code{mom_targets} can be used for constructing a representative matched sample.  The lengths of \code{mom_tols} and \code{mom_target} have to be equal to the number of columns of \code{mom_covs}.  Note that the columns of \code{mom_covs} can be transformations of the original covariates to balance higher order single-dimensional moments like variances and skewness, and multidimensional moments such as correlations (Zubizarreta 2012).}

\item{fine}{Fine balance parameters: a list with one argument, 

\code{fine = list(covs = fine_covs)}, 

where \code{fine_covs} is a matrix where each column is a nominal covariate for fine balance.  Fine balance enforces exact distributional balance on nominal covariates, but without constraining treated and control units to be matched within each category of each nominal covariate as in exact matching.  See chapter 10 of Rosenbaum (2010) for details.}

\item{solver}{
Optimization solver parameters: a list with four objects, 

\code{solver = list(name = name, t_max = t_max, approximate = 1, round_cplex = 0,}\cr
\code{    trace_cplex = 0)}.

\code{solver} is a string that determines the optimization solver to be used.  The options are: \code{cplex}, \code{glpk}, \code{gurobi}, \code{highs}, and \code{symphony}.  The default solver is \code{highs} with \code{approximate = 1}. For an exact solution, we strongly recommend using \code{cplex} or \code{gurobi} as they are much faster than the other solvers, but they do require a license (free for academics, but not for people outside universities).  Between \code{cplex} and \code{gurobi}, note that installing the R interface for \code{gurobi} is much simpler.

\code{t_max} is a scalar with the maximum time limit for finding the matches.  This option is specific to \code{cplex}, \code{gurobi}, and \code{highs}.  If the optimal matches are not found within this time limit, a partial, suboptimal solution is given. 
	
\code{approximate} is a scalar that determines the method of solution.  If \code{approximate = 1} (the default), an approximate solution is found via a relaxation of the original integer program.  This method of solution is faster than \code{approximate = 0}, but some balancing constraints may be violated to some extent.  This option works only with \code{n_controls = 1}, i.e. pair matching.

\code{round_cplex} is binary specific to \code{cplex}.  \code{round_cplex = 1} ensures that the solution found is integral by rounding and all the constraints are exactly statisfied; \code{round_cplex = 0} (the default) encodes there is no rounding which may return slightly infeasible integer solutions.  
	
\code{trace} is a binary specific to \code{cplex} and \code{gurobi}. \code{trace = 1} turns the optimizer output on. The default is \code{trace = 0}.
}
}

\value{
	A list containing the optimal solution, with the following objects:
	
	\item{obj_total}{value of the objective function at the optimum;}
	
	\item{obj_dist_mat}{value of the total sum of distances term of the objective function at the optimum;}
	
	\item{t_id}{indexes of the matched treated units at the optimum;}
	
	\item{c_id}{indexes of the matched controls at the optimum;}
	
	\item{group_id}{matched pairs or groups at the optimum;}
	
	\item{time}{time elapsed to find the optimal solution.}
}

\references{
	Bennett, M., Vielma, J. P., and Zubizarreta, J. R. (2017), "Building a Representative Matched Sample with Treatment Doses," working paper.	

	Visconti, G., and Zubizarreta, J. R. (2017), "Finding the Largest Matched Sample that is Balanced by Design: A Case Study of the Effect of an Earthquake on Electoral Outcomes," working paper.	

	Zubizarreta, J. R. (2012), "Using Mixed Integer Programming for Matching in an Observational Study of Kidney Failure after Surgery," \emph{Journal of the American Statistical Association}, 107, 1360-1371.
		
	Zubizarreta, J. R., Paredes, R. D., and Rosenbaum, P. R. (2014), "Matching for Balance, Pairing for Heterogeneity in an Observational Study of the Effectiveness of For-profit and Not-for-profit High Schools in Chile," \emph{Annals of Applied Statistics}, 8, 204-231.
}

\author{
	Jose R. Zubizarreta <zubizarreta@hcp.med.harvard.edu>, Cinar Kilcioglu <ckilcioglu16@gsb.columbia.edu>, Juan P. Vielma <jvielma@mit.edu>.
}

\seealso{
	\pkg{sensitivitymv}, \pkg{sensitivitymw}.
}

\examples{
	
# Load, sort, and attach data
data(lalonde)
lalonde = lalonde[order(lalonde$treatment, decreasing = TRUE), ]
attach(lalonde)

################################# 
# Step 1: use cardinality matching to find the largest sample of matched pairs for which 
# all the covariates are finely balanced.
#################################

# Discretize covariates
quantiles = function(covar, n_q) {
	p_q = seq(0, 1, 1/n_q)
	val_q = quantile(covar, probs = p_q, na.rm = TRUE)
	covar_out = rep(NA, length(covar))
	for (i in 1:n_q) {
		if (i==1) {covar_out[covar<val_q[i+1]] = i}
		if (i>1 & i<n_q) {covar_out[covar>=val_q[i] & covar<val_q[i+1]] = i}
		if (i==n_q) {covar_out[covar>=val_q[i] & covar<=val_q[i+1]] = i}}
	covar_out
}
age_5 = quantiles(age, 5)
education_5 = quantiles(education, 5)
re74_5 = quantiles(re74, 5)
re75_5 = quantiles(re75, 5)

# Treatment indicator; note that the data needs to be sorted in decreasing order
# according to this treatment indicator
t_ind = treatment
t_ind 

# Fine balance
fine_covs = cbind(black, hispanic, married, nodegree, age_5, education_5, re74_5, re75_5)
fine = list(covs = fine_covs)

# Solver options
t_max = 60*5
solver = "highs"
approximate = 0
solver = list(name = solver, t_max = t_max, approximate = approximate,
round_cplex = 0, trace = 0)

# Match                   
out_1 = cardmatch(t_ind, fine = fine, solver = solver)

# Indices of the treated units and matched controls
t_id_1 = out_1$t_id  
c_id_1 = out_1$c_id	

# Mean balance
covs = cbind(age, education, black, hispanic, married, nodegree, re74, re75)
meantab(covs, t_ind, t_id_1, c_id_1)

# Fine balance (note here we are getting an approximate solution)
for (i in 1:ncol(fine_covs)) {		
	print(finetab(fine_covs[, i], t_id_1, c_id_1))
}

################################# 
# Step 2: use optimal matching (minimum distance matching) to find the (re)pairing of
# treated and control that minimizes the total sum of covariate distances between matched 
# pairs.  For this, use the function 'distmatch' which is a wrapper for 'bmatch'.  
#################################

# New treatment indicator
t_ind_2 = t_ind[c(t_id_1, c_id_1)]
table(t_ind_2)

# To build the distance matrix, the idea is to use strong predictors of the outcome
dist_mat_2 = abs(outer(re74[t_id_1], re74[c_id_1], "-"))
dim(dist_mat_2)

# Match
out_2 = distmatch(t_ind_2, dist_mat_2, solver)

# Indices of the treated units and matched controls
t_id_2 = t_id_1[out_2$t_id]  
c_id_2 = c_id_1[out_2$c_id-length(out_2$c_id)]	

# Covariate balance is preserved...
meantab(covs, t_ind, t_id_2, c_id_2)
for (i in 1:ncol(fine_covs)) {		
	print(finetab(fine_covs[, i], t_id_2, c_id_2))
}

# ... but covariate distances are reduced
distances_step_1 = sum(diag(dist_mat_2)) 
distances_step_2 = sum(diag(dist_mat_2[out_2$t_id, out_2$c_id-length(out_2$c_id)])) 
distances_step_1
distances_step_2

# The mean difference in outcomes is the same...
mean(re78[t_id_1]-re78[c_id_1])
mean(re78[t_id_2]-re78[c_id_2])

# ... but their standard deviation is reduced
sd(re78[t_id_1]-re78[c_id_1])
sd(re78[t_id_2]-re78[c_id_2])
	
}

\keyword{Causal inference}
\keyword{Instrumental variable}
\keyword{Matching}
\keyword{Regression discontinuity}
\keyword{Observational study}
