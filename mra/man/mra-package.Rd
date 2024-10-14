\name{mra-package}
\alias{mra-package}
\alias{mra}
\docType{package}
\title{
MRA - Mark Recapture Analysis
}
\description{
\bold{Description} - 
This package contains analysis functions, and associated routines, to conduct 
analyses of mark-recapture (capture-recapture) data using individual, 
time, and individual-time varying covariates. In general, these routines 
relate vectors of capture histories to vectors of covariates using 
a regression approach (Amstrup et al. 2005, Ch 9).  All capture, survival, 
transition, etc. parameters are functions of individual and time 
specific covariates, and the estimated parameters 
are coefficients in logistic-linear equations. 

\bold{Relationship to MARK} - 
For the most part, these routines perform a subset of the analyses available in 
program MARK or via the MARK front-end package, RMark.  
The most significant difference between this package
and MARK is parameterization.  The parameterization used here
does not utilize triangular 
"parameter information matrices" (PIMs) as MARK (and RMark) does.  
Because of this, the "design" matrix utilized by 
this package is not parallel to the "design" matrix of program MARK.  For those new 
to mark-recapture analysis, this parameterization difference will be inconsequential.  
The approach taken here provides equivalent modeling flexibility, yet is 
easier to grasp and visualize, in our opinion. 
For those already familiar with the PIMs used 
by program MARK, it is helpful to view the "PIMs" of this package as 
rectangular matrices of the real parameters. I.e., the "PIMs" of this package are 
rectangular matrices where cell (i,j) contains the real parameter (capture or survival) 
for individual i at capture occasion j. 

Analyses available here that are \emph{not} included in program MARK include:
\itemize{
\item Estimation of population size from open population CJS models via 
the Horvitz-Thompson estimator. 
\item Residuals, goodness of fit tests, and associated plots for assessing
model fit in open CJS models.
}

\bold{Future Research} - 
The author of MRA welcome interest in and routines that 
perform the 
following analyzes:
\itemize{
\item Continuous time models.  Especially those that allow inclusion of covariates.
\item Band recovery models.
\item Baysian models.
\item Joint live-dead recovery models.
\item MCMC methods or routines that can be applied to exiting models. 
\item Plotting methods for exiting models.
\item Model selection methods for existing models. 
\item Simulation methods and routines.
}


}

\details{
\tabular{ll}{
Package: \tab mra\cr
Type: \tab Package\cr
License: \tab GNU General Public License\cr
}
}



\author{
Trent McDonald

Maintainer: Trent McDonald <tmcdonald@west-inc.com>
}

\references{
Amstrup, S.C., T.L. McDonald, and B.F.J. Manly. 2005. \emph{Handbook of 
Capture-Recapture Analysis}, Princeton: Princeton University Press.
}

\keyword{ package }
