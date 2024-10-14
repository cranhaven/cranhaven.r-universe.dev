\name{tvar}
\alias{tvar}

\title{ Expand Time-varying covariates in models }
\description{
  	Expands a vector of time-varying values into a 2-d matrix 
    for use in MRA model formulas.
}
\usage{
tvar(x, nan=attr(x,"nan"), drop.levels=attr(x,"drop.levels"))
}
\arguments{
  \item{x}{ The vector of time varying values to expand.  This can be 
    a factor (see \code{as.factor}). It is assumed that \code{length(x)} = 
    number of sampling occasions.  If not, an error will occur in whatever routine 
    called this function (e.g., \code{F.3d.model.matrix}). }
  \item{nan}{ Number of individuals. Default is to use the 'nan' attribute 
    of \code{x}.  If \code{nan} is not specified or is not an attribute of \code{x}, 
    an error is thrown. }
  \item{drop.levels}{ A vector of integers specifying which levels of a factor 
    to drop.  Only applicable if \code{x} is a factor.  By default, the 
    the 'drop.levels' attribute of \code{x} is used.  If \code{x} does not 
    have a 'drop.levels' attribute, the first level of the factor is dropped.  
    \code{drop.levels=length(levels(x))}
    does the SAS thing and drops the last level of a factor.  Specifying multiple 
    levels to drop is acceptable.
    E.g., \code{drop.levels=c(1,2,7)} drops 
    the 1st, 2nd, and 7th levels of the factor.  First level 
    of a factor is first element of \code{levels(x)}.  Second level 
    of a factor is second element of \code{levels(x)}, and so on.  Setting  
    \code{drop.levels} to 0, a negative number, or a number greater than the number 
    of levels will not drop any levels (this is so-called cell mean coding). 
    Keep in mind presence of the intercept.}
}


\value{
    A 2-d matrix of size \code{nan} x \code{length(x)} suitable for passing to the 
    Fortran DLL of MRA for estimation.  Values within columns are constant, values 
    across columns vary according to \code{x}. If \code{x} is a factor, this matrix 
    contains 0-1 indicator functions necessary to fit the factor. 

    If \code{x} is a factor, attributes of the returned matrix are 
    "levels" = levels of the factor and "contr" = contrasts used in the coding (always
    \code{contr.treatment}).  For other contrast coding of factors, make your own 
    2-d matrix with a call to the appropriate function (like \code{contr.poly}).     

}
\author{ Trent McDonald, WEST-INC, tmcdonald@west-inc.com }
\seealso{ \code{\link{F.cjs.estim}}, \code{\link{tvar}} }
\examples{

nan <- 30
ns <- 5
time.occ <- as.factor(paste("T",1:ns, sep=""))
attr(time.occ,"nan") <- nan
attr(time.occ,"drop.levels") <- ns

time.mat <- tvar(time.occ) # Last occasion is the reference, the SAS and MARK default.

time.mat <- tvar(as.factor(1:ns),nan,ns) #equivalent to above.

#   Look at 3-d matrix produced when called with factors
dim(time.mat) <- c(nan,ns,length(levels(time.occ))-1)
print(time.mat)  # each page is the 2-d matrix used in the fit.
print(time.mat[1,,])

#  compare above to 
tvar( 1:ns, nan )


}

\keyword{ models }

