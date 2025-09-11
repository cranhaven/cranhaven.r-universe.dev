\name{nmatch}

\alias{nmatch}

\title{
	Optimal nonbipartite matching in randomized experiments and observational studies
}

\description{
	Function for optimal nonbipartite matching in randomized experiments and observational studies that directly balances the observed covariates.  \code{nmatch} allows the user to enforce different forms of covariate balance in the matched samples, such as moment balance (e.g., of means, variances, and correlations), distributional balance (e.g., fine balance, near-fine balance, strength-\emph{k} balancing), and exact matching.  Among others, \code{nmatch} can be used in the design of randomized experiments for matching before randomization (Greevy et al. 2004, Zou and Zubizarreta 2016), and in observational studies for matching with doses and strengthening an instrumental variable (Baiocchi et al. 2010, Lu et al. 2011).
}

\usage{
	nmatch(dist_mat, subset_weight = NULL, total_pairs = NULL, mom = NULL,
	       exact = NULL, near_exact = NULL, fine = NULL, near_fine = NULL,
	       near = NULL, far = NULL, solver = NULL)
}

\arguments{

\item{dist_mat}{distance matrix: a matrix of positive distances between units.}

\item{subset_weight}{subset matching weight: a scalar that regulates the trade-off between the total sum of distances between matched pairs and the total number of matched pairs.  The larger \code{subset_weight}, the more importance will be given to the the total number of matched pairs relative to the total sum of distances between matched pairs.  See Rosenbaum (2012) and Zubizarreta et al. (2013) for a discussion of this parameter.  If \code{subset_weight = NULL}, then \code{nmatch} will match all the available units, provided it exists a feasible solution exists.}

\item{total_pairs}{total number of matched pairs: a scalar specifying the number of matched pairs to be obtained.  If \code{total_pairs = NULL} then no specific number of matched pairs is required before matching.}

\item{mom}{moment balance parameters: a list with three arguments, 

\code{mom = list(covs = mom_covs, tols = mom_tols, targets = mom_targets)}.

\code{mom_covs} is a matrix where each column is a covariate whose mean is to be balanced.  \code{mom_tols} is a vector of tolerances for the maximum difference in means for the covariates in \code{mom_covs}.  \code{mom_targets} is a vector of target moments (e.g., means) of a distribution to be approximated by matching.  \code{mom_targets} is optional, but if \code{mom_covs} is specified then \code{mom_tols} needs to be specified too.  If \code{mom_targets} is \code{NULL}, then \code{nmatch} will match treated and control units so that covariates in \code{mom_covs} differ at most by \code{mom_tols}.  If \code{mom_targets} is specified, then \code{nmatch} will match treated and control units so that each matched group differs at most by \code{mom_tols} units from the respective moments in \code{mom_targets}.  As a result, the matched groups will differ at most \code{mom_tols * 2} from each other.  Under certain assumptions, \code{mom_targets} can be used for constructing a representative matched sample.  The lengths of \code{mom_tols} and \code{mom_target} have to be equal to the number of columns of \code{mom_covs}.  Note that the columns of \code{mom_covs} can be transformations of the original covariates to balance higher order single-dimensional moments like variances and skewness, and multidimensional moments such as correlations (Zubizarreta 2012).}

\item{exact}{Exact matching parameters: a list with one argument, 

\code{exact = list(covs = exact_covs)}, 

where \code{exact_covs} is a matrix where each column is a nominal covariate for exact matching.}

\item{near_exact}{Near-exact matching parameters: a list with two arguments, 

\code{near_exact = list(covs = near_exact_covs, devs = near_exact_devs)}. 

\code{near_exact_covs} are the near-exact matching covariates; specifically, a matrix where each column is a nominal covariate for near-exact matching.  \code{near_exact_devs} are the maximum deviations from near-exact matching: a vector of scalars defining the maximum deviation allowed from exact matching for the covariates defined in \code{near_exact_covs}.  Note that the length of \code{near_exact_devs} has to be equal to the number of columns of \code{near_exact_covs}.  For detailed expositions of near-exact matching in the context of bipartite matching, see section 9.2 of Rosenbaum (2010) and Zubizarreta et al. (2011).}

\item{fine}{Fine balance parameters: a list with one argument, 

\code{fine = list(covs = fine_covs)}, 

where \code{fine_covs} is a matrix where each column is a nominal covariate for fine balance.  Fine balance enforces exact distributional balance on nominal covariates, but without constraining treated and control units to be matched within each category of each nominal covariate as in exact matching.  See chapter 10 of Rosenbaum (2010) for details.}

\item{near_fine}{Near-fine balance parameters: a list with two arguments, 

\code{near_fine = list(covs = near_fine_covs, devs = near_fine_devs)}.

\code{near_fine_covs} is a matrix where each column is a nominal covariate for near-fine matching.  \code{near_fine_devs} is a vector of scalars defining the maximum deviation allowed from fine balance for the covariates in \code{near_fine_covs}.  Note that the length of \code{near_fine_devs} has to be equal to the number of columns of \code{near_fine_covs}.  See Yang et al. (2012) for a description of near-fine balance.}

\item{near}{Near matching parameters: a list with three arguments, 

\code{near = list(covs = near_covs, pairs = near_pairs, groups = near_groups)}.

\code{near_covs} is a matrix where each column is a variable for near matching.  \code{near_pairs} is a vector determining the maximum distance between individual matched pairs for each variable in \code{near_covs}.  \code{near_groups} is a vector defining the maximum average distance (in aggregate) between matched groups for each covariate in \code{near_covs}.  If \code{near_covs} is specified, then either \code{near_pairs}, \code{near_covs}, or both must be specified as well, and the length of \code{near_pairs} and/or \code{near_groups} has to be equal to the number of columns of \code{near_covs}.}

\item{far}{Far matching parameters: a list with three arguments, 

\code{far = list(covs = far_covs, pairs = far_pairs, groups = far_groups)}.

\code{far_covs} is a matrix where each column is a variable (a covariate or an instrumental variable) for far matching.  \code{far_pairs} is a vector determining the minimum distance between units in a matched pair for each variable in \code{far_covs}, and \code{far_groups} is a vector defining the minimum average (aggregate) distance between matched groups for each variable in \code{far_covs}.  If \code{far_covs} is specified, then either \code{far_pairs}, \code{far_covs}, or both, must be specified, and the length of \code{far_pairs} and/or \code{far_groups} has to be equal to the number of columns of \code{far_covs}.  See Zubizarreta et al. (2013) for strengthening an instrumental variable with integer programming.}

\item{solver}{Optimization solver parameters: a list with four objects, 

\code{solver = list(name = name, t_max = t_max, approximate = 1, round_cplex = 0,}\cr
\code{    trace_cplex = 0)}.
	  
\code{solver} is a string that determines the optimization solver to be used.  The options are: \code{cplex}, \code{glpk}, \code{gurobi}, \code{highs}, and \code{symphony}.  The default solver is \code{highs} with \code{approximate = 1}, so that by default an approximate solution is found (see \code{approximate} below).  For an exact solution, we strongly recommend using \code{cplex} or \code{gurobi} as they are much faster than the other solvers, but they do require a license (free for academics, but not for people outside universities).  Between \code{cplex} and \code{gurobi}, note that the installation of the \code{gurobi} interface for R is much simpler.

\code{t_max} is a scalar with the maximum time limit for finding the matches.  This option is specific to \code{cplex} and \code{gurobi}.  If the optimal matches are not found within this time limit, a partial, suboptimal solution is given.  
	
\code{approximate} is a scalar that determines the method of solution.  If \code{approximate = 1} (the default), an approximate solution is found via a relaxation of the original integer program.  This method of solution is faster than \code{approximate = 0}, but some balancing constraints may be violated to some extent.

\code{round_cplex} is binary specific to \code{cplex}.  \code{round_cplex = 1} ensures that the solution found is integral by rounding and all the constraints are exactly statisfied; \code{round_cplex = 0} (the default) encodes there is no rounding which may return slightly infeasible integer solutions.  
	
\code{trace} is a binary specific to \code{cplex} and \code{gurobi}.  \code{trace = 1} turns the optimizer output on.  The default is \code{trace = 0}.
}
}



\value{
	A list containing the optimal solution, with the following objects:
	
	\item{obj_total}{value of the objective function at the optimum;}
	
	\item{obj_dist_mat}{value of the total sum of distances term of the objective function at the optimum;}
	
	\item{id_1}{indexes of the matched units in group 1 at the optimum;}
	
	\item{id_2}{indexes of the matched units in group 2 at the optimum;}
	
	\item{group_id}{matched pairs at the optimum;}
	
	\item{time}{time elapsed to find the optimal solution.}
}

\references{
	Baiocchi, M., Small, D., Lorch, S. and Rosenbaum, P. R. (2010), "Building a Stronger Instrument in an Observational Study of Perinatal Care for Premature Infants," \emph{Journal of the American Statistical Association}, 105, 1285-1296.
	
	Greevy, R., Lu, B., Silber, J. H., and Rosenbaum, P. R. (2004), "Optimal Multivariate
Matching Before Randomization," \emph{Biostatistics}, 5, 263-275.

	Lu, B., Greevy, R., Xu, X., and Beck C. (2011), "Optimal Nonbipartite Matching and its Statistical Applications," \emph{The American Statistician}, 65, 21-30.

	Rosenbaum, P. R. (2010), \emph{Design of Observational Studies}, Springer.

	Rosenbaum, P. R. (2012), "Optimal Matching of an Optimally Chosen Subset in Observa-
tional studies," \emph{Journal of Computational and Graphical Statistics}, 21, 57-71.

	Yang. F., Zubizarreta, J. R., Small, D. S., Lorch, S. A., and Rosenbaum, P. R. (2014), "Dissonant Conclusions When Testing the Validity of an Instrumental Variable," \emph{The American Statistician}, 68, 253-263.

	Zou, J., and Zubizarreta, J. R. (2016), "Covariate Balanced Restricted Randomization: Optimal Designs, Exact Tests, and Asymptotic Results," working paper.
	
	Zubizarreta, J. R., Reinke, C. E., Kelz, R. R., Silber, J. H., and Rosenbaum, P. R. (2011), "Matching for Several Sparse Nominal Variables in a Case-Control Study of Readmission Following Surgery," \emph{The American Statistician}, 65, 229-238.
		
	Zubizarreta, J. R. (2012), "Using Mixed Integer Programming for Matching in an Observational Study of Kidney Failure after Surgery," \emph{Journal of the American Statistical Association}, 107, 1360-1371.
}

\author{
	Jose R. Zubizarreta <zubizarreta@hcp.med.harvard.edu>, Cinar Kilcioglu <ckilcioglu16@gsb.columbia.edu>.
}

%\seealso{
%
%}

\examples{    

## Uncomment the following example
## Load and attach data
#data(lalonde)
#attach(lalonde)

################################# 
## Example: optimal subset matching
################################# 

## Optimal subset matching pursues two competing goals at 
## the same time: to minimize the total of distances while 
## matching as many observations as possible.  The trade-off 
## between these two is regulated by the parameter subset_weight 
## (see Rosenbaum 2012 and Zubizarreta et al. 2013 for a discussion).
## Here the balance requirements are mean and fine balance for 
## different covariates.  We require 50 pairs to be matched.
## Again, the solver used is HiGHS with the approximate option.

## Matrix of covariates
#X_mat = cbind(age, education, black, hispanic, married, nodegree, re74, re75)

## Distance matrix
#dist_mat_covs = round(dist(X_mat, diag = TRUE, upper = TRUE), 1)
#dist_mat = as.matrix(dist_mat_covs)

## Subset matching weight
#subset_weight = 1

## Total pairs to be matched
#total_pairs = 50

## Moment balance: constrain differences in means to be at most .1 standard deviations apart
#mom_covs = cbind(age, education)
#mom_tols = apply(mom_covs, 2, sd)*.1
#mom = list(covs = mom_covs, tols = mom_tols)

## Solver options
#t_max = 60*5
#solver = "highs"
#approximate = 1
#solver = list(name = solver, t_max = t_max, approximate = approximate, round_cplex = 0, 
#trace_cplex = 0)

## Match                  
#out = nmatch(dist_mat = dist_mat, subset_weight = subset_weight, total_pairs = total_pairs, 
#mom = mom, solver = solver)              
              
## Indices of the treated units and matched controls
#id_1 = out$id_1  
#id_2 = out$id_2	

## Assess mean balance
#a = apply(mom_covs[id_1, ], 2, mean)
#b = apply(mom_covs[id_2, ], 2, mean)
#tab = round(cbind(a, b, a-b, mom_tols), 2)
#colnames(tab) = c("Mean 1", "Mean 2", "Diffs", "Tols")
#tab

## Assess fine balance (note here we are getting an approximate solution)
#for (i in 1:ncol(fine_covs)) {		
#	print(finetab(fine_covs[, i], id_1, id_2))
#}
}

\keyword{Causal inference}
\keyword{Matching}
\keyword{Randomized experiment}

