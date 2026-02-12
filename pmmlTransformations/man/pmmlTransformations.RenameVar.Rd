\name{RenameVar}
\alias{RenameVar}
\title{
Renames a variable in the WrapData transform object 
}
\description{
Renames a variable inside a WrapData object 
}
\usage{
RenameVar(boxdata, xformInfo=NA, ...)
}
\arguments{
  \item{boxdata}{wrapper object obtained by using the WrapData function on the raw data.}
  \item{xformInfo}{specification of details of the renaming.}
  \item{\dots}{
    further arguments passed to or from other methods. 
  }
}
\details{
 Once input data is wrapped by the \bold{WrapData} function, it is somewhat involved to rename a variable
inside. This function makes it easier to do so.
 Given an variable named \bold{InputVar} and the name one wishes to rename it to, \bold{OutputVar}, the 
rename command options are:
	
	xformInfo="InputVar -> OutputVar"

There are two methods in which the variables can be referred to. The first method is to use its 
column number; given the \bold{data} attribute of the \bold{boxData} object, this would be the order at 
which the variable appears. This can be indicated in the format "column#". The second method is to 
refer to the variable by its name. This method will work even if the renamed value already exists; in 
which case there will be two variables with the same name.

If no input variable name is provided, the original object is returned with no renamings performed.

}
\value{
R object containing the raw data, the transformed data and data statistics.
}

\author{
Tridivesh Jena, Zementis, Inc.
}

\seealso{
\code{\link{WrapData}}.
}
\examples{
# Load the standard iris dataset, already built into R
   data(iris)

# First wrap the data
   irisBox <- WrapData(iris)

# We wish to refer to the variables "Sepal.Length" and 
# "Sepal.Width" as "SL" and "SW"
  irisBox <- RenameVar(irisBox,"column1->SL")
  irisBox <- RenameVar(irisBox,"Sepal.Width->SW")

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{utilities}
\keyword{methods}
