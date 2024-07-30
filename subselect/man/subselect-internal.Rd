\name{subselect-internal}
\title{Internal subselect Objects}

\alias{initialization}
\alias{validannimp}
\alias{validation}
\alias{validgenetic}
\alias{validmat}
\alias{validnovcrit}
\alias{prepRetOtp}

\description{
  Internal subselect functions that validate input to the search functions 
(if their names start with valid). 
}
\usage{

initialization(mat, criterion, r)

validannimp(kmin, kmax, nsol, exclude, nexclude, include, ninclude, initialsol, implog)

validation(mat, kmin, kmax, exclude, include, criterion, pcindices, tolval, tolsym, 
algorithm)

validgenetic(kmin, kmax, popsize, mutprob, exclude, nexclude, include, ninclude, 
initialpop)

validmat(mat, p, tolval, tolsym, allowsingular, algorithm)

validnovcrit(mat, criterion, H, r, p, tolval, tolsym)

prepRetOtp(output, kmin, kmax, nsol, Waldval, algorithm ,Numprb, Optimal, tl)

}
\details{
  These functions are not to be called by the user.
}
\keyword{internal}
