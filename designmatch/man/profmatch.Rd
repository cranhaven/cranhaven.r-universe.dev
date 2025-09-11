\name{profmatch}

\alias{profmatch}

\title{Optimal profile matching}

\description{
	Function for optimal profile matching to construct matched samples that are balanced toward a user-specified covariate profile.  This covariate profile can represent a specific population or a target individual, facilitating the generalization and personalization of causal inferences (Cohn and Zubizarreta 2022).  For each treatment group reservoir, \code{profmatch} finds the largest sample that is balanced relative to the profile.  The formulation of \code{profmatch} has been simplified to handle larger data than \code{bmatch} or \code{nmatch}.  Similar to \code{bmatch} or \code{nmatch}, the performance of \code{profmatch} is greatly enhanced by using the \code{solver} options \code{cplex} or \code{gurobi}.
}

\usage{
	profmatch(t_ind, mom, solver = NULL)
}

\arguments{

\item{t_ind}{treatment indicator: a vector indicating treatment group of each observation.}

\item{mom}{moment balance parameters: a list with three arguments, 

\code{mom = list(targets = mom_targets, covs = mom_covs, tols = mom_tols)}.


\code{mom_targets} is the profile, i.e. a vector of target moments (e.g., means) of a distribution or a vector of characteristics of an individual toward which to balance the treatment groups. \code{mom_covs} is a matrix where each column is a covariate whose mean is to be balanced toward \code{mom_targets}.  \code{mom_tols} is a vector of tolerances for the maximum difference in means between the covariates in \code{mom_covs} and the elements of \code{mom_targets}.  \code{profmatch} will select units from each treatment group so that each matched group differs at most by \code{mom_tols} from the respective elements of \code{mom_targets}.  As a result, the matched groups will differ at most \code{mom_tols * 2} from each other.  Under certain assumptions, \code{mom_targets} can be used for constructing a representative matched sample.  The lengths of \code{mom_tols} and \code{mom_target} have to be equal to the number of columns of \code{mom_covs}.  Note that the columns of \code{mom_covs} can be transformations of the original covariates to balance higher order single-dimensional moments like variances and skewness, and multidimensional moments such as correlations (Zubizarreta 2012).}

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
	
	\item{obj_totals}{values of the objective functions at the optima (one value for each treatment group matching problem);}
	
	\item{ids}{indices of the matched units at the optima;}
	
	\item{times}{time elapsed to find the optimal solutions (one value for each treatment group matching problem).}
}

\references{

	Zubizarreta, J. R. (2012), "Using Mixed Integer Programming for Matching in an Observational Study of Kidney Failure after Surgery," \emph{Journal of the American Statistical Association}, 107, 1360-1371.
	
	Cohn, E. R. and Zubizarreta, J. R. (2022) "Profile Matching for the Generalization and Personalization of Causal Inferences," \emph{Epidemiology}
}

\author{
	Eric R. Cohn <ericcohn@g.harvard.edu>, Jose R. Zubizarreta <zubizarreta@hcp.med.harvard.edu>, Cinar Kilcioglu <ckilcioglu16@gsb.columbia.edu>, Juan P. Vielma <jvielma@mit.edu>.
}

\seealso{
	\pkg{sensitivitymv}, \pkg{sensitivitymw}.
}

\examples{
	
### Load, sort, and attach data
#data(lalonde)
#lalonde = lalonde[order(lalonde$treatment, decreasing = TRUE), ]
#attach(lalonde)

### Specify covariates
#covs = c("age", "education", "black", "hispanic", "married", "nodegree", "re74", "re75")

### Vector of treatment group indicators
#t_ind = lalonde$treatment

### Covariate matrix
#mom_covs = as.matrix(lalonde[, covs])

### Tolerances will be 0.05 * each covariate's standard deviation
#mom_sds = apply(lalonde[, covs], 2, sd)
#mom_tols = 0.05 * mom_sds

### Target moments will be the overall means in the sample
#mom_targets = colMeans(lalonde[, covs])

### Solver options
#t_max = 60*30
#solver = "gurobi"
#approximate = 0
#solver = list(name = solver, t_max = t_max, approximate = approximate, round_cplex = 0, trace = 0)

#mom = list(covs = mom_covs, tols = mom_tols, targets = mom_targets)
#pmatch_out = profmatch(t_ind, mom, solver)

### Selecting the matched units
#lalonde.matched = lalonde[pmatch_out$id,]

### Comparing TASMDs before and after matching
#TASMD.0.2 = abs(colMeans(lalonde.matched[which(lalonde.matched$treatment == 0), covs]) 
#                    - mom_targets) / mom_sds
#TASMD.1.2 = abs(colMeans(lalonde.matched[which(lalonde.matched$treatment == 1), covs]) 
#                    - mom_targets) / mom_sds

#TASMD.0.1 = abs(colMeans(lalonde[which(lalonde$treatment == 0), covs]) - mom_targets) / mom_sds
#TASMD.1.1 = abs(colMeans(lalonde[which(lalonde$treatment == 1), covs]) - mom_targets) / mom_sds

### For each treatment group, ASAMDs are reduced after matching (i.e., balance is achieved)
#cbind(TASMD.0.1, TASMD.0.2)
#cbind(TASMD.1.1, TASMD.1.2)

}

\keyword{Causal inference}
\keyword{Instrumental variable}
\keyword{Matching}
\keyword{Regression discontinuity}
\keyword{Observational study}
