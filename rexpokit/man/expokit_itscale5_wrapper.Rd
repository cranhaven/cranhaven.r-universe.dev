\name{expokit_itscale5_wrapper}
\alias{expokit_itscale5_wrapper}
\title{wrapper function for FORTRAN itscale5, for FD's maxent}
\usage{
  expokit_itscale5_wrapper(SXT, ngroups, ntraits, const, prior, 
    prob, entropy, niter, tol, denom)
}
\arguments{
\item{SXT}{is a Groups (rows) X Traits (columns) matrix}
\item{ngroups}{is an integer (nb: NJM's interpretation)}
\item{ntraits}{is an integer (nb: NJM's interpretation)}
\item{const}{is a vector of the constraint values (means, variances)}
\item{prior}{is the prior distribution}
\item{prob}{is the return vector of the maximum entropy}
\item{entropy}{is the maximum entropy probabilities}
\item{niter}{is the number of iterations required}
\item{tol}{is the convergence tolerance value; tolerance is mean square difference}
\item{denom}{are final moments}
}

\description{This function wraps the .C call to EXPOKIT for the itscale5 function.}

\details{The itscale5 function is in the "itscale5.f" FORTRAN file.  itscale5 is used by
the FD::maxent function.\cr

The maxent function is used by BioGeoBEARS, merely to provide a simple method 
of putting flat or skewed probability distributions on the ordered categorical variable
"size of smaller daughter range").\cr

As the package FD has a number of other dependencies, some of which cause problems on 
some machines, I am just including maxent and itscale5 here, in order to avoid 
"dependency hell".\cr

I am putting it in rexpokit rather than BioGeoBEARS, to make rexpokit the only
package using FORTRAN code (which has a list of its own issues).}

\value{\code{res} A list of outputs}

\seealso{\code{\link{maxent}}}

\author{Nicholas J. Matzke \email{nickmatzke.ncse@gmail.com}}

\examples{# See maxent() function
test=1}
