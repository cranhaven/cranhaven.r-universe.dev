\name{optextras-package}
\alias{optextras}
\docType{package}
\title{
Tools to Support Optimization Possibly with Bounds and Masks
}
\description{
Provides tools that work with extensions of the optim() function to unify
and streamline optimization capabilities in R for smooth, possibly box
constrained functions of several or many parameters

There are three test functions, fnchk, grchk, and hesschk, to allow the user
function to be tested for validity and correctness. However, no set of tests is 
exhaustive, and extensions and improvements are welcome. The package 
\code{numDeriv} is used for generation of numerical approximations to 
derivatives.
}
\details{
\tabular{ll}{
Package: \tab optextras\cr
Version: \tab 2012-6.18\cr
Date: \tab 2012-06-18\cr
License: \tab GPL-2\cr
Lazyload: \tab Yes\cr
Depends: \tab numDeriv \cr
Suggests: \tab BB, ucminf, Rcgmin, Rvmmin, minqa, setRNG, dfoptim\cr
Repository: \tab R-Forge\cr
Repository/R-Forge/Project: \tab optimizer\cr
}

Index:
\preformatted{
axsearch     Perform an axial search optimality check
bmchk        Check bounds and masks for parameter constraints
bmstep       Compute the maximum step along a search direction.
fnchk        Test validity of user function
gHgen        Compute gradient and Hessian as a given 
             set of parameters
gHgenb       Compute gradient and Hessian as a given 
             set of parameters appying bounds and masks
grback       Backward numerical gradient approximation
grcentral    Central numerical gradient approximation
grchk        Check that gradient function evaluation 
             matches numerical gradient
grfwd        Forward numerical gradient approximation
grnd         Gradient approximation using \code{numDeriv}
hesschk      Check that Hessian function evaluation 
             matches numerical approximation
kktchk       Check the Karush-Kuhn-Tucker optimality conditions
optsp        An environment to hold some globally useful items
             used by optimization programs
scalechk     Check scale of initial parameters and bounds
}
}
\author{
John C Nash <nashjc@uottawa.ca> and Ravi Varadhan <RVaradhan@jhmi.edu>

Maintainer: John C Nash <nashjc@uottawa.ca>
}
\references{
  Nash, John C. and Varadhan, Ravi (2011) Unifying Optimization Algorithms 
          to Aid Software System Users: optimx for R, Journal of Statistical
	  Software, publication pending.
}
\keyword{ package }
\keyword{ optimization }
\seealso{
optim
}

