\name{designmatch-package}
\alias{designmatch-package}
\alias{designmatch}
\docType{package}
\title{
Optimal Matched Design of Randomized Experiments and Observational Studies
}
\description{
\code{designmatch} includes two functions for the construction of matched samples that are balanced and representative by design.  These two functions are \code{bmatch} and \code{nmatch} for bipartite and nonbipartite matching, respectively.  Both functions include options for directly balancing means, higher order moments, and distributions of the observed covariates.  In both \code{bmatch} and \code{nmatch}, an integer programming (IP) problem is solved.  This IP problem either minimizes the total sum of covariate distances between matched units, maximizes the total number of matched units, or optimizes a combination of the two, subject to matching and covariate balancing constraints.  In order to solve these problems, four different optimization solvers can be used: CPLEX, GLPK, Gurobi, HiGHS, and Symphony.  By default, both \code{bmatch} and \code{nmatch} solve a relaxation of these integer programs using HiGHS, which runs quickly but may violate to some extent some of the balancing constraints.  If the user wants to solve for an exact solution of the program, we strongly recommend using either CPLEX or Gurobi, which are much faster but require a license (free for academic users) and special installation (see the installation instructions).  Between the two, Gurobi is considerably easier to install.  Among others, \code{designmatch} can be used for matching in treatment-control as well as case-control observational studies; observational studies with instrumental variables and discontinuity designs; and for the design of randomized experiments, for example for matching before randomization.  The package also includes functions for assessing covariate balance in the matched samples.
}
\details{
\tabular{ll}{
Package: \tab designmatch\cr
Type: \tab Package\cr
Version: \tab 0.5.4\cr
Date: \tab 2023-08-29\cr
License: \tab GPL-2 | GPL-3\cr
}
}
\author{
	Jose R. Zubizarreta <zubizarreta@hcp.med.harvard.edu>, Cinar Kilcioglu <ckilcioglu16@gsb.columbia.edu>.

Maintainer: Jose R. Zubizarreta <zubizarreta@hcp.med.harvard.edu>, Cinar Kilcioglu <ckilcioglu16@gsb.columbia.edu>.
}
\references{
	Greevy, R., Lu, B., Silber, J. H., and Rosenbaum, P. R. (2004), "Optimal Multivariate
Matching Before Randomization," \emph{Biostatistics}, 5, 263-275.

    Hsu. J., Zubizarreta, J. R., Small, D. S., and Rosenbaum, P. R. (2015), "Strong Control of the Family-Wise Error Rate in Observational Studies that Discover Effect Modification by Exploratory Methods," \emph{Biometrika}, 102, 767-782.

	Keele, L., Titiunik, R., and Zubizarreta, J. R., (2015), "Enhancing a Geographic Regression Discontinuity Design Through Matching to Estimate the Effect of Ballot Initiatives on Voter Turnout," \emph{Journal of the Royal Statistical Society: Series A}, 178, 223-239.

	Kilcioglu, C., and Zubizarreta, J. R., (2016), "Maximizing the Information Content of a Balanced Matched Sample in a Study of the Economic Performance of Green Buildings," working paper.

	Lu, B., Greevy, R., Xu, X., and Beck C. (2011), "Optimal Nonbipartite Matching and its Statistical Applications," \emph{The American Statistician}, 65, 21-30.

	Rosenbaum, P. R. (2010), \emph{Design of Observational Studies}, Springer.

	Rosenbaum, P. R. (2012), "Optimal Matching of an Optimally Chosen Subset in Observa-
tional studies," \emph{Journal of Computational and Graphical Statistics}, 21, 57-71.

	Yang, D., Small, D., Silber, J. H., and Rosenbaum, P. R. (2012), "Optimal Matching With Minimal Deviation From Fine Balance in a Study of Obesity and Surgical Outcomes," \emph{Biometrics}, 68, 628-636. 

	Yang. F., Zubizarreta, J. R., Small, D. S., Lorch, S. A., and Rosenbaum, P. R. (2014), "Dissonant Conclusions When Testing the Validity of an Instrumental Variable," \emph{The American Statistician}, 68, 253-263.

	Zou, J., and Zubizarreta, J. R., (2015) "Covariate Balanced Restricted Randomization: Optimal Designs, Exact Tests, and Asymptotic Results," working paper.

	Zubizarreta, J. R., Reinke, C. E., Kelz, R. R., Silber, J. H., and Rosenbaum, P. R. (2011), "Matching for Several Sparse Nominal Variables in a Case-Control Study of Readmission Following Surgery," \emph{The American Statistician}, 65, 229-238.
		
	Zubizarreta, J. R. (2012), "Using Mixed Integer Programming for Matching in an Observational Study of Kidney Failure after Surgery," \emph{Journal of the American Statistical Association}, 107, 1360-1371.
	
	Zubizarreta, J. R., Paredes, R. D., and Rosenbaum, P. R. (2014), "Matching for Balance, Pairing for Heterogeneity in an Observational Study of the Effectiveness of For-profit and Not-for-profit High Schools in Chile," \emph{Annals of Applied Statistics}, 8, 204-231.
}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation ~~
%~~ directory ~~
%\keyword{ package }
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
%\examples{
%~~ simple examples of the most important functions ~~
%}
