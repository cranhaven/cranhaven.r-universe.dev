\name{F.cr.model.matrix}
\alias{F.cr.model.matrix}

\title{ Capture-Recapture model matrix }
\description{ 
  	Returns two model matrices for capture-recapture modeling. Both are 
    in the form 
	of (giant) 2D matrices. 
}
\usage{ 
F.cr.model.matrix(capture, survival, nan, ns)
}
\arguments{
  \item{capture}{ Formula for the capture model.  Must be a formula object with 
	no response, then ~, followed by the names of 2-D arrays of covariates to fit in 
	the capture model. 
	For example: capture = ~ age + sex, where age and sex are matrices.  }
  \item{survival}{ Formula for the survival model. Must be a formula object with 
	no response, then ~, followed by the names of 2-D arrays of covariates to fit in 
	the survival model. 
	For example: capture = ~ age + sex, where age and sex are matrices.  }
  \item{nan}{ Number of individuals in the model. This is necessary for the 
    \code{ivar} and \code{tvar} functions to work.  Normally, \code{nan} = 
    number of rows in capture history matrix.  No default value. }
  \item{ns}{ Number of sampling occasions.  Normally, \code{ns} = number of 
    columns in the capture history matrix. }
}
\details{
  This routine is intended to be called internally by model fitting routines 
	of MRA.  General users should never have to call this routine.

  This routine uses a call to \code{eval} with a model frame, and calls the 
  R internal \code{model.matrix} to 
  resolve the matrices in the formula. All matrices specified in the models 
  should be in the current scope and accessible to both \code{eval} and \code{model.matrix}.  
  
  This routine calls \code{F.3d.model.matrix} twice.  \code{F.3d.model.matrix} 
  does all the work. 
  
}
\value{
  A list containing the following components:

  \item{capX}{ A NAN by IX+(NX*NS) matrix containing covariate values for the capture 
	model.  Matrices specified in the model are column appended together.  
	NAN = \code{nrow(x)} where \code{x} is a 2-D matrix in the model (i.e., 
	number of animals). NS = \code{ncol(x)} (i.e., number of capture occasions). 
	NX = number of matrices specified in the model.  IX = 1 if an intercept is included, 
	0 otherwise.  The j-th covariate matrix specified in the model can be accessed directly with 
	\code{capX[, IX+1+(NS*(j-1)):(NS*j) ]}.  }

  \item{surX}{ A NAN by IY+(NY*NS) matrix containing covariate values for the survival 
	model.  Matrices specified in the model are column appended together.  
	NAN = \code{nrow(x)} where \code{y} is a 2-D matrix in the model (i.e., 
	number of animals). NS = \code{ncol(y)} (i.e., number of capture occasions). 
	NY = number of matrices specified in the model.  IY = 1 if an intercept is included, 
	0 otherwise.  The j-th covariate matrix specified in the model can be accessed directly with 
	\code{capY[, IY+1+(NS*(j-1)):(NS*j) ]}.} 

  \item{n.cap.covars}{Number of matrices specified in the capture model (NX above).}
  \item{n.sur.covars}{Number of matrices specified in the survival model (NY above).}
  \item{cap.intercept}{TRUE or FALSE depending on whether an intercept was included in the 
	capture model}
  \item{sur.intercept}{TRUE or FALSE depending on whether an intercept was included in the 
	survival model}
  \item{cap.vars}{Vector of names for the NX covariates in the capture model.}
  \item{sur.vars}{Vector of names for the NY covariates in the survival model.} 
}
\author{ Trent McDonald, WEST-INC, tmcdonald@west-inc.com }
\seealso{ \code{\link{F.cjs.estim}}, \code{\link{model.matrix}}, \code{\link{eval}} }
\examples{ 


# Synthetic example with 10 animals and 5 occasions
nan <- 10
ns <- 5
sex <- matrix( as.numeric(runif( nan ) > 0.5), nrow=nan, ncol=ns )
x <- matrix( runif( nan*ns ) , nrow=nan, ncol=ns )
F.cr.model.matrix( capture= ~ sex + x, survival= ~ -1 + x, nan, ns )


}

\keyword{ models }
