\name{MinMaxXform}
\alias{MinMaxXform}
\title{
Normalizes continuous values in accordance to the PMML element: \bold{NormContinuous}
}
\description{
Given input data in a WrapData format, normalize the given data values to lie between provided limits
}
\usage{
MinMaxXform(boxdata, xformInfo=NA, mapMissingTo=NA, ...)
}
\arguments{
  \item{boxdata}{
   the wrapper object obtained by using the WrapData function on the raw data. 
  }
  \item{xformInfo}{
   specification of details of the transformation. 
  }
  \item{mapMissingTo}{value to be given to the transformed variable if the value of the input variable is missing.}
  \item{\dots}{
    further arguments passed to or from other methods.
  }
}

\details{
  Given an input variable named \bold{InputVar}, the name of the transformed variable \bold{OutputVar}, the 
desired minimum value the transformed variable may have \bold{low_limit}, the desired maximum value 
the transformed variable may have \bold{high_limit}, and the desired value of the transformed variable 
if the input variable value is missing \bold{missingVal}, the \bold{MinMaxXform} command including all the 
optional parameters is in the format:

xformInfo="InputVar -> OutputVar[low_limit,high_limit], mapMissingTo="missingVal"

There are two methods in which the variables can be referred to. The first method is to use its 
column number; given the \bold{data} attribute of the \bold{boxData} object, this would be the order at 
which the variable appears. This can be indicated in the format "column#". The second method is to 
refer to the variable by its name.
 
The name of the transformed variable is optional; if the name is not provided, the transformed 
variable is given the name: "derived_" + \emph{original_variable_name} \cr
Similarly, the low and high limit values are optional; they have the default values of 0 and 1 
respectively. \bold{missingValue} is an optional parameter as well. It is the value of the derived variable  
if the input value is missing.

 If no input variable names are provided, by default all numeric variables are transformed. Note 
that in this case a replacement value for missing input values cannot be specified; the same applies 
to the \bold{low_limit} and \bold{high_limit} parameters.

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
   library(pmmlTransformations)

# First wrap the data
   irisBox <- WrapData(iris)

# Normalize all numeric variables of the loaded iris dataset to lie 
# between 0 and 1. These would normalize "Sepal.Length", "Sepal.Width", 
# "Petal.Length", "Petal.Width" to the 4 new derived variables named 
# derived_Sepal.Length, derived_Sepal.Width, derived_Petal.Length, 
# derived_Petal.Width.
   MinMaxXform(irisBox)

# Normalize the 1st column values of the dataset (Sepal.Length) to lie 
# between 0 and 1 and give the derived variable the name "dsl" 
   MinMaxXform(irisBox,xformInfo="column1 -> dsl")

# Repeat the above operation; adding the new transformed variable to 
# the irisBox object
   irisBox <- MinMaxXform(irisBox,xformInfo="column1 -> dsl")

# Transform Sepal.Width(the 2nd column)
# The new transformed variable will be given the default name 
# "derived_Sepal.Width" 
   MinMaxXform(irisBox,xformInfo="column2")

# Repeat the same operation as above, this time using the variable name
   MinMaxXform(irisBox,xformInfo="Sepal.Width")

# Repeat the same operation as above, assign the transformed variable, 
# "derived_Sepal.Width". the value of 0.5 if the input value of the 
# "Sepal.Width" variable is missing 
   MinMaxXform(irisBox,xformInfo="Sepal.Width", "mapMissingTo=0.5")

# Transform Sepal.Width(the 2nd column) to lie between 2 and 3. 
# The new transformed variable will be given the default name 
# "derived_Sepal.Width"
   MinMaxXform(irisBox,xformInfo="column2->[2,3]")

# Repeat the above transformation, this time the transformed variable 
# lies between 0 and 10
   irisBox <- MinMaxXform(irisBox,xformInfo="column2->[,10]")
}
\keyword{ manip }
