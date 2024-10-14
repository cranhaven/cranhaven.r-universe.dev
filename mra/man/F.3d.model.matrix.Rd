\name{F.3d.model.matrix}
\alias{F.3d.model.matrix}

\title{ 3-Dimensional capture-recapture model matrices }
\description{
  	Returns a 3D model matrix for capture-recapture modeling in the form
	of a (giant) 2D matrix.
}
\usage{
F.3d.model.matrix(formula, d1, d2)
}
\arguments{
  \item{formula}{ A formula object specifying covariates in a capture-recapture 
    model.  Must not have a response, i.e., ~, followed by the names 
    of 2-D arrays or 1-D vectors contained inside calls to \code{ivar} or 
    \code{tvar}.  See \code{help(F.cjs.estim)} for examples. }
  \item{d1}{ Magnitude of dimension 1 of the returned matrix.  This is 
    always number of rows in the returned matrix. Usually, \code{d1} = number 
    of individuals = number of rows in capture history matrix. }
  \item{d2}{ Magnitude of dimension 2 of the returned matrix.  This is 
    number of columns in the capture history matrix. }
}

\details{
  This routine is intended to be called internally by the routines
	of MRA.  General users should never have to call this routine.

  This routine uses a call to \code{eval} with a model frame, and calls the
  R internal \code{model.matrix} to
  resolve the matrices in the formula. All matrices specified in the models
  should be in the current scope and accessible to both \code{eval} and \code{model.matrix}.
  
  See \code{help(F.cjs.estim)} for examples of ways to specify models. 
  
}
\value{
    A (giant) 2-d matrix containing covariate values suitable for passing to 
    the Fortran code that does the estimation for MRA.  This matrix has all the 
    2-d matrices of the model \code{cbind}-ed together.  It's dimension is NAN x 
    NS*(number of coefficients).  A convenient way to view the matrix is to assign 
    a 3-d dimension.  I.e., if \code{x} is the result of a call to this function 
    and there are NX coefficients in the model, 
    then \code{ dim(x) <- c(NAN,NS,NX) } makes a 3-d matrix with NAN rows, NS columns, 
    and NX pages. View the covariates for a single animal with \code{x[3,,]} or similar
    statement. 

    Names of variables in the model are returned as attribute "variables".  Whether 
    the model has an intercept is returned as attribute "intercept". 
}
\author{ Trent McDonald, WEST-INC, tmcdonald@west-inc.com }
\seealso{ \code{\link{F.cr.model.matrix}}, \code{\link{tvar}}, \code{\link{ivar}},
\code{\link{model.matrix}}, \code{\link{eval}} }
\examples{


# Synthetic example with 10 animals and 5 occasions
nan <- 10
ns <- 5
sex <- as.factor(as.numeric(runif( nan ) > 0.5))
attr(sex,"ns") <- ns
x <- matrix( runif( nan*ns ) , nrow=nan, ncol=ns )
F.3d.model.matrix( ~ ivar(sex) + x, nan, ns )


}

\keyword{ models }
