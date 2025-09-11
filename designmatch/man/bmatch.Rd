\name{bmatch}

\alias{bmatch}

\title{Optimal bipartite matching in observational studies}

\description{
	Function for optimal bipartite matching in observational studies that directly balances the observed covariates.  \code{bmatch} allows the user to enforce different forms of covariate balance in the matched samples such as moment balance (e.g., of means, variances and correlations), distributional balance (e.g., fine balance, near-fine balance, strength-\emph{k} balancing) and exact matching.  In observational studies where an instrumental variable is available, \code{bmatch} can be used to handle weak instruments and strengthen them by means of the \code{far} options (see Yang et al. 2014 for an example). \code{bmatch} can also be used in discontinuity designs by matching units in a neighborhood of the discontinuity (see Keele et al. 2015 for details).  In any of these settings, \code{bmatch} either minimizes the total sum of covariate distances between matched units, maximizes the total number of matched units, or optimizes a combination of the two, subject to matching, covariate balancing, and representativeness constraints (see the examples below).
}

\usage{
	bmatch(t_ind, dist_mat = NULL, subset_weight = NULL, n_controls = 1,
	       total_groups = NULL, mom = NULL, ks = NULL, exact = NULL, 
	       near_exact = NULL, fine = NULL, near_fine = NULL, near = NULL, 
	       far = NULL, solver = NULL)
}

\arguments{

\item{t_ind}{treatment indicator: a vector of zeros and ones indicating treatment (1 = treated; 0 = control).  Please note that the data needs to be sorted in decreasing order according to this treatment indicator.}

\item{dist_mat}{distance matrix: a matrix of positive distances between treated units (rows) and controls (columns).  If \code{dist_mat = NULL} and \code{subset_weight = 1}, then bmatch will solve the cardinality matching problem in Zubizarreta et al. (2014).}

\item{subset_weight}{subset matching weight: a scalar that regulates the trade-off between the total sum of distances between matched pairs and the total number of matched pairs.  The larger \code{subset_weight}, the more importance will be given to the the total number of matched pairs relative to the total sum of distances between matched pairs.  See Rosenbaum (2012) and Zubizarreta et al. (2013) for a discussion of this parameter. 

	If \code{subset_weight = NULL}, then \code{bmatch} will match all the treated observations, provided there exists a feasible solution.

	If \code{subset_weight = 1} and \code{dist_mat = NULL}, then \code{bmatch} will solve the cardinality matching problem in Zubizarreta et al. (2014).}

\item{n_controls}{number of controls: a scalar defining the number of controls to be matched with a fixed rate to each treated unit.  The default is \code{n_controls = 1}, i.e. pair matching.}

\item{total_groups}{total number of matched pairs: a scalar specifying the number of matched pairs to be obtained.  If \code{total_groups = NULL}, then no specific number of matched pairs is required before matching.}

\item{mom}{moment balance parameters: a list with three arguments, 

\code{mom = list(covs = mom_covs, tols = mom_tols, targets = mom_targets)}.

\code{mom_covs} is a matrix where each column is a covariate whose mean is to be balanced.  \code{mom_tols} is a vector of tolerances for the maximum difference in means for the covariates in \code{mom_covs}.  \code{mom_targets} is a vector of target moments (e.g., means) of a distribution to be approximated by matched sampling.  \code{mom_targets} is optional, but if \code{mom_covs} is specified then \code{mom_tols} needs to be specified too.  If \code{mom_targets} is \code{NULL}, then \code{bmatch} will match treated and control units so that covariates in \code{mom_covs} differ at most by \code{mom_tols}.  If \code{mom_targets} is specified, then \code{bmatch} will match treated and control units so that each matched group differs at most by \code{mom_tols} units from the respective moments in \code{mom_targets}.  As a result, the matched groups will differ at most \code{mom_tols * 2} from each other.  Under certain assumptions, \code{mom_targets} can be used for constructing a representative matched sample.  The lengths of \code{mom_tols} and \code{mom_target} have to be equal to the number of columns of \code{mom_covs}.  Note that the columns of \code{mom_covs} can be transformations of the original covariates to balance higher order single-dimensional moments like variances and skewness, and multidimensional moments such as correlations (Zubizarreta 2012).}

\item{ks}{Kolmogorov-Smirnov balance parameters: a list with three objects, 

\code{ks = list(covs = ks_covs, n_grid = ks_n_grid, tols = ks_tols)}.  

\code{ks_covs} is a matrix where each column is a covariate whose difference in a coarsened version of the Kolmogorov-Smirnov statistic between treated units and matched controls is to be constrained.  \code{ks_n_grid} is  scalar defining the number of equispaced grid points for calculating the Kolmogorov-Smirnov statistic between the distributions of the treated units and matched controls for the covariates in \code{ks_covs}.  The defaul is \code{ks_n_grid = 10} for the deciles of the empirical cumulative distribution functions of the treated units for the covariate in question.  \code{ks_tols} is a vector of tolerances for the maximum differences in means for the covariates defined in \code{ks_covs}.  Note that the length of \code{ks_tols} has to be equal to the number of columns of \code{ks_covs}.}

\item{exact}{Exact matching parameters: a list with one argument, 

\code{exact = list(covs = exact_covs)}, 

where \code{exact_covs} is a matrix where each column is a nominal covariate for exact matching.}

\item{near_exact}{Near-exact matching parameters: a list with two objects, 

\code{near_exact = list(covs = near_exact_covs, devs = near_exact_devs)}. 

\code{near_exact_covs} are the near-exact matching covariates; specifically, a matrix where each column is a nominal covariate for near-exact matching.  \code{near_exact_devs} are the maximum deviations from near-exact matching: a vector of scalars defining the maximum deviation allowed from exact matching for the covariates defined in \code{near_exact_covs}.  Note that the length of \code{near_exact_devs} has to be equal to the number of columns of \code{near_exact_covs}.  For detailed expositions of near-exact matching, see section 9.2 of Rosenbaum (2010) and Zubizarreta et al. (2011).}

\item{fine}{Fine balance parameters: a list with one argument, 

\code{fine = list(covs = fine_covs)}, 

where \code{fine_covs} is a matrix where each column is a nominal covariate for fine balance.  Fine balance enforces exact distributional balance on nominal covariates, but without constraining treated and control units to be matched within each category of each nominal covariate as in exact matching.  See chapter 10 of Rosenbaum (2010) for details.}

\item{near_fine}{Near-fine balance parameters: a list with two objects, 

\code{near_fine = list(covs = near_fine_covs, devs = near_fine_devs)}.

\code{near_fine_covs} is a matrix where each column is a nominal covariate for near-fine matching. \code{near_fine_devs} is a vector of scalars defining the maximum deviation allowed from fine balance for the covariates in \code{near_fine_covs}.  Note that the length of \code{near_fine_devs} has to be equal to the number of columns of \code{near_fine_covs}.  See Yang et al. (2012) for a description of near-fine balance.}

\item{near}{Near matching parameters: a list with three objects, 

\code{near = list(covs = near_covs, pairs = near_pairs, groups = near_groups)}.

\code{near_covs} is a matrix where each column is a variable for near matching.  \code{near_pairs} is a vector determining the maximum distance between individual matched pairs for each variable in \code{near_covs}.  \code{near_groups} is a vector determining the maximum average distance between matched groups (in aggregate) for each covariate in \code{near_covs}.  If \code{near_covs} is specified, then either \code{near_pairs}, \code{near_covs} or both must be specified as well, and the length of \code{near_pairs} and/or \code{near_groups} has to be equal to the number of columns of \code{near_covs}.  The \code{near} options can be useful for matching within a window of a discontinuity (see Keele et al. 2015 for details).
}

\item{far}{Far matching parameters: a list with three objects, 

\code{far = list(covs = far_covs, pairs = far_pairs, groups = far_groups)}.  

\code{far_covs} is a matrix where each column is a variable (a covariate or an instrumental variable) for far matching.  \code{far_pairs} is a vector determining the minimum distance between units in a matched pair for each variable in \code{far_covs}, and \code{far_groups} is a vector defining the minimum average (aggregate) distance between matched groups for each variable in \code{far_covs}.  If \code{far_covs} is specified, then either \code{far_pairs}, \code{far_covs}, or both, must be specified, and the length of \code{far_pairs} and/or \code{far_groups} has to be equal to the number of columns of \code{far_covs}.  See Yang et al. (2014) for strengthening an instrumental variable via far matching in a bipartite setting.}

\item{solver}{
Optimization solver parameters: a list with four objects, 

\code{solver = list(name = name, t_max = t_max, approximate = 1, round_cplex = 0,}\cr
\code{    trace_cplex = 0)}.

\code{solver} is a string that determines the optimization solver to be used.  The options are: \code{cplex}, \code{glpk}, \code{gurobi}, \code{highs}, and \code{symphony}.  The default solver is \code{highs} with \code{approximate = 1}, so that by default an approximate solution is found (see \code{approximate} below).  For an exact solution, we strongly recommend using \code{cplex} or \code{gurobi} as they are much faster than the other solvers, but they do require a license (free for academics, but not for people outside universities).  Between \code{cplex} and \code{gurobi}, note that installing the R interface for \code{gurobi} is much simpler.

\code{t_max} is a scalar with the maximum time limit for finding the matches.  This option is specific to \code{cplex} and \code{gurobi}.  If the optimal matches are not found within this time limit, a partial, suboptimal solution is given.  
	
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
	Keele, L., Titiunik, R., and Zubizarreta, J. R., (2015), "Enhancing a Geographic Regression Discontinuity Design Through Matching to Estimate the Effect of Ballot Initiatives on Voter Turnout," \emph{Journal of the Royal Statistical Society: Series A}, 178, 223-239.

	Rosenbaum, P. R. (2010), \emph{Design of Observational Studies}, Springer.

	Rosenbaum, P. R. (2012), "Optimal Matching of an Optimally Chosen Subset in Observa-
tional studies," \emph{Journal of Computational and Graphical Statistics}, 21, 57-71.

	Yang, D., Small, D., Silber, J. H., and Rosenbaum, P. R. (2012), "Optimal Matching With Minimal Deviation From Fine Balance in a Study of Obesity and Surgical Outcomes," \emph{Biometrics}, 68, 628-36. 

	Yang. F., Zubizarreta, J. R., Small, D. S., Lorch, S. A., and Rosenbaum, P. R. (2014), "Dissonant Conclusions When Testing the Validity of an Instrumental Variable," \emph{The American Statistician}, 68, 253-263.

	Zubizarreta, J. R., Reinke, C. E., Kelz, R. R., Silber, J. H., and Rosenbaum, P. R. (2011), "Matching for Several Sparse Nominal Variables in a Case-Control Study of Readmission Following Surgery," \emph{The American Statistician}, 65, 229-238.
		
	Zubizarreta, J. R. (2012), "Using Mixed Integer Programming for Matching in an Observational Study of Kidney Failure after Surgery," \emph{Journal of the American Statistical Association}, 107, 1360-1371.
	
	Zubizarreta, J. R., Paredes, R. D., and Rosenbaum, P. R. (2014), "Matching for Balance, Pairing for Heterogeneity in an Observational Study of the Effectiveness of For-profit and Not-for-profit High Schools in Chile," \emph{Annals of Applied Statistics}, 8, 204-231.
}

\author{
	Jose R. Zubizarreta <zubizarreta@hcp.med.harvard.edu>, Cinar Kilcioglu <ckilcioglu16@gsb.columbia.edu>.
}

\seealso{
	\pkg{sensitivitymv}, \pkg{sensitivitymw}.
}

\examples{

## Uncomment the following examples
## Load, sort, and attach data
#data(lalonde)
#lalonde = lalonde[order(lalonde$treatment, decreasing = TRUE), ]
#attach(lalonde)

################################# 
## Example 1: cardinality matching
################################# 

## Cardinality matching finds the largest matched sample of pairs that meets balance 
## requirements.  Here the balance requirements are mean balance, fine balance and 
## exact matching for different covariates. The solver used is glpk with the 
## approximate option.

## Treatment indicator; note that the data needs to be sorted in decreasing order
## according to this treatment indicator
#t_ind = treatment
#t_ind 

## Distance matrix
#dist_mat = NULL 

## Subset matching weight
#subset_weight = 1

## Moment balance: constrain differences in means to be at most .05 standard deviations apart
#mom_covs = cbind(age, education, black, hispanic, married, nodegree, re74, re75)
#mom_tols = round(absstddif(mom_covs, t_ind, .05), 2)
#mom = list(covs = mom_covs, tols = mom_tols)

## Fine balance
#fine_covs = cbind(black, hispanic, married, nodegree)
#fine = list(covs = fine_covs)

## Exact matching
#exact_covs = cbind(black)
#exact = list(covs = exact_covs)

## Solver options
#t_max = 60*5
#solver = "glpk"
#approximate = 1
#solver = list(name = solver, t_max = t_max, approximate = approximate,
#round_cplex = 0, trace = 0)

## Match                   
#out = bmatch(t_ind = t_ind, dist_mat = dist_mat, subset_weight = subset_weight, 
#mom = mom, fine = fine, exact = exact, solver = solver)              
              
## Indices of the treated units and matched controls
#t_id = out$t_id  
#c_id = out$c_id	

## Time
#out$time/60

## Matched group identifier (who is matched to whom)
#out$group_id

## Assess mean balance
#meantab(mom_covs, t_ind, t_id, c_id)

## Assess fine balance (note here we are getting an approximate solution)
#for (i in 1:ncol(fine_covs)) {		
#  print(finetab(fine_covs[, i], t_id, c_id))
#}

## Assess exact matching balance
#table(exact_covs[t_id]==exact_covs[c_id])

################################## 
## Example 2: minimum distance matching
################################## 

## The goal here is to minimize the total of distances between matched pairs.  In 
## this example there are no covariate balance requirements.  Again, the solver 
## used is glpk with the approximate option

## Treatment indicator
#t_ind = treatment

## Matrix of covariates
#X_mat = cbind(age, education, black, hispanic, married, nodegree, re74, re75)

## Distance matrix
#dist_mat = distmat(t_ind, X_mat)

## Subset matching weight
#subset_weight = NULL

## Total pairs to be matched
#total_groups = sum(t_ind)

## Solver options
#t_max = 60*5
#solver = "glpk"
#approximate = 1
#solver = list(name = solver, t_max = t_max, approximate = approximate, 
#round_cplex = 0, trace_cplex = 0)

## Match                   
#out = bmatch(t_ind = t_ind, dist_mat = dist_mat, total_groups = total_groups, 
#solver = solver)              
              
## Indices of the treated units and matched controls
#t_id = out$t_id  
#c_id = out$c_id	

## Total of distances between matched pairs
#out$obj_total

## Assess mean balance
#meantab(X_mat, t_ind, t_id, c_id)

################################## 
## Example 3: optimal subset matching
################################## 

## Optimal subset matching pursues two competing goals at 
## the same time: to minimize the total sum of covariate distances  
## while matching as many observations as possible.  The trade-off 
## between these two goals is regulated by the parameter subset_weight 
## (see Rosenbaum 2012 and Zubizarreta et al. 2013 for a discussion).
## Here the balance requirements are mean balance, near-fine balance 
## and near-exact matching for different covariates.
## Again, the solver used is glpk with the approximate option.

## Treatment indicator
#t_ind = treatment

## Matrix of covariates
#X_mat = cbind(age, education, black, hispanic, married, nodegree, re74, re75)

## Distance matrix
#dist_mat = distmat(t_ind, X_mat)

## Subset matching weight
#subset_weight = median(dist_mat)

## Moment balance: constrain differences in means to be at most .05 standard deviations apart
#mom_covs = cbind(age, education, black, hispanic, married, nodegree, re74, re75)
#mom_tols = round(absstddif(mom_covs, t_ind, .05), 2)
#mom = list(covs = mom_covs, tols = mom_tols)

## Near-fine balance
#near_fine_covs = cbind(married, nodegree)
#near_fine_devs = rep(5, 2)
#near_fine = list(covs = near_fine_covs, devs = near_fine_devs)

## Near-exact matching
#near_exact_covs = cbind(black, hispanic)
#near_exact_devs = rep(5, 2)
#near_exact = list(covs = near_exact_covs, devs = near_exact_devs)

## Solver options
#t_max = 60*5
#solver = "glpk"
#approximate = 1
#solver = list(name = solver, t_max = t_max, approximate = approximate, 
#round_cplex = 0, trace_cplex = 0)

## Match                   
#out = bmatch(t_ind = t_ind, dist_mat = dist_mat, subset_weight = subset_weight, 
#mom = mom, near_fine = near_fine, near_exact = near_exact, solver = solver)              
              
## Indices of the treated units and matched controls
#t_id = out$t_id  
#c_id = out$c_id	

## Time
#out$time/60

## Matched group identifier (who is matched to whom)
#out$group_id

## Assess mean balance (note here we are getting an approximate solution)
#meantab(X_mat, t_ind, t_id, c_id)

## Assess fine balance
#for (i in 1:ncol(near_fine_covs)) {		
#	print(finetab(near_fine_covs[, i], t_id, c_id))
#}

## Assess exact matching balance
#for (i in 1:ncol(near_exact_covs)) {	
#	print(table(near_exact_covs[t_id, i]==near_exact_covs[c_id, i]))
#}
	
}

\keyword{Causal inference}
\keyword{Instrumental variable}
\keyword{Matching}
\keyword{Regression discontinuity}
\keyword{Observational study}
