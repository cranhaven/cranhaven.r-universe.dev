\name{ivar}
\alias{ivar}

\title{ Expand Individual-varying covariates in models }
\description{
  	Expands a vector of individual-varying values into a 2-d matrix 
    of the appropriate size for use in MRA model formulas.
}
\usage{
ivar(x, ns=attr(x,"ns"), drop.levels=attr(x,"drop.levels"))
}
\arguments{
  \item{x}{ The vector of individual varying values to expand.  This can be 
    a factor (see \code{as.factor}). It is assumed that \code{length(x)} = 
    number of individuals.  If not, an error will occur in whatever routine 
    called this function (e.g., \code{F.3d.model.matrix}). }
  \item{ns}{ Number of sampling occasions. Default is to use the 'ns' attribute 
    of \code{x}.  If \code{ns} is not specified or is not an attribute of \code{x}, 
    an error is thrown. }
  \item{drop.levels}{ A vector of integers specifying which levels of a factor 
    to drop.  Only applicable if \code{x} is a factor.  By default, the 
    the 'drop.levels' attribute of \code{x} is used.  If \code{x} does not 
    have a 'drop.levels' attribute, the first level of the factor is dropped. 
    \code{drop.levels=length(levels(x))}
    does the SAS thing and drops the last level of a factor.  Specifying multiple 
    levels to drop is acceptable. E.g., \code{drop.levels=c(1,2,7)} drops 
    the 1st, 2nd, and 7th levels of the factor, whatever they are.  First level 
    of a factor is first element of \code{levels(x)}.  Second level 
    of a factor is second element of \code{levels(x)}, and so on.  Setting  
    \code{drop.levels} to 0, a negative number, or a number greater than the number 
    of levels will not drop any levels (this is so-called cell mean coding). 
    Keep in mind presence of the intercept.}
}


\value{
    A 2-d matrix of size \code{length(x)} x \code{ns} suitable for passing to the 
    Fortran DLL of MRA for estimation.  Values within rows are constant, values 
    across rows vary according to \code{x}. If \code{x} is a factor, this matrix 
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
age <- as.factor(sample( c("J","S1","S2","Adult"), size=nan, replace=TRUE ))
attr(age,"ns") <- ns

# Note that levels get reordered (by R default, alphabetically)
attr(age,"drop.levels") <- (1:length(levels(age)))[ levels(age) == "J" ]

age.mat <- ivar(age)  # level J is the reference
age.mat <- ivar(age, drop=4) # level S2 is the reference

# Look at 3-D matrix produced when called with a factor.
dim(age.mat) <- c(nan,ns,length(levels(age))-1)
print(age.mat)  # each page is the 2-d matrix used in the fit.
print(age.mat[1,,])

age.mat <- ivar(age, drop=c(3,4))  # level S1 and S2 are combined and are the reference

# compare above to 
ivar( c(1,1,2,2,3,3), 5 )

}

\keyword{ models }
