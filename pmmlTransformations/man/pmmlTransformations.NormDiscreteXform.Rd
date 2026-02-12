\name{NormDiscreteXform}
\alias{NormDiscreteXform}
\title{
Normalize discrete values in accordance to the PMML element:\cr \bold{NormDiscrete}
}
\description{
Define a new derived variable for each possible value of a categorical variable. Given a categorical 
variable \bold{catVar} with possible discrete values \bold{A} and \bold{B}, this will create 2 
derived variables \bold{catVar_A} and \bold{catVar_B}. If, for example, the input value of \bold{catVar}
is \bold{A} then \bold{catVar_A} equals 1 and 
\bold{catVar_B} equals 0.  
}
\usage{
NormDiscreteXform(boxdata, xformInfo=NA, 
                  inputVar=NA, mapMissingTo=NA, ...)
}
\arguments{
  \item{boxdata}{
    the wrapper object obtained by using the WrapData function on the raw data.
   }
  \item{xformInfo}{
   specification of details of the transformation: the name of the input variable to be transformed. 
  }
  \item{inputVar}{
   the input variable name in the data on which the transformation is to be applied 
  }
  \item{mapMissingTo}{value to be given to the transformed variable if the value of the input variable is missing.}
  \item{\dots}{
    further arguments passed to or from other methods.
  }
}
\details{
Given an input variable, \bold{InputVar} and \bold{missingVal}, the desired value of the transformed variable 
if the input variable value is missing, the NormDiscreteXform command including all 
optional parameters is in the format:

	xformInfo="inputVar=input_variable, mapMissingTo=missingVal"

There are two methods in which the input variable can be referred to. The first method is to use its
column number; given the \bold{data} attribute of the \bold{boxData} object, this would be the order at
which the variable appears. This can be indicated in the format "column#". The second method is to refer 
to the variable by its name.

The \bold{xformInfo} and \bold{inputVar} parameters provide the same information. While either one may be used
when using this function, at least one of them is required. If both parameters are given, the \bold{inputVar}
parameter is used as the default. 
 
The output of this transformation is a set of transformed variables, one for each possible value of 
the input variable. For example, given possible values of the input variable \bold{val1}, \bold{val2}, ... these 
transformed variables are by default named \bold{InputVar_val1}, \bold{InputVar_val2}, ...

}
\value{
R object containing the raw data, the transformed data and data statistics.
}

\author{
Tridivesh Jena, Zementis, Inc.  
}

\seealso{
  \code{\link{WrapData}}
}
\examples{
# Load the standard iris dataset, already available in R
   data(iris)

# First wrap the data
   irisBox <- WrapData(iris)

# Discretize the "Species" variable. This will find all possible 
# values of the "Species" variable and define new variables. The 
# parameter name used here should be replaced by the new preferred 
# parameter name as shown in the next example below.
#
# 	"Species_setosa" such that it is 1 if 
#      "Species" equals "setosa", else 0;
# 	"Species_versicolor" such that it is 1 if 
#      "Species" equals "versicolor", else 0;
# 	"Species_virginica" such that it is 1 if 
#      "Species" equals "virginica", else 0

  irisBox <- NormDiscreteXform(irisBox,inputVar="Species")
  
# Exact same operation performed with a different parameter name. 
# Use of this new parameter is the preferred method as the previous 
# parameter will be deprecated soon.

  irisBox <- WrapData(iris)
  irisBox <- NormDiscreteXform(irisBox,xformInfo="Species")
  
}
\keyword{ manip }
