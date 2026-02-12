\name{MapXform}
\alias{MapXform}
\title{
Implements a map between discrete values in accordance to the PMML element: \bold{MapValues}
}
\description{
Maps discrete values of an input variable to a discrete value of the transformed variable.
The map can be given in an external table file referred to in the transform command or as a
list of data frames; each data frame defining a map transform for one variable. 
}
\usage{
MapXform(boxdata, xformInfo, table, 
        defaultValue=NA, mapMissingTo=NA, ...)
}
\arguments{
  \item{boxdata}{
   the wrapper object obtained by using the WrapData function on the raw data. 
  }
  \item{xformInfo}{specification of details of the transformation. It can be a text giving the external
		file name or a list of data frames. Even if only 1 variable is to be transformed, the 
		information for that map should be given as a list with 1 element.}
  \item{table}{name of external CSV file containing the map from input to output values.}
  \item{defaultValue}{the default value to be given to the transformed variable. If 'xformInfo' is a
		list, this is a vector with each element corresponding to the corresponding list element.}
  \item{mapMissingTo}{value to be given to the transformed variable if the value of the input variable is missing.
		If 'xformInfo' is a list, this is a vector with each element corresponding to the corresponding 
		list element.}
  \item{\dots}{
    further arguments passed to or from other methods.
  }
}

\details{
 Given a map from the combination of variables \bold{InVar1}, \bold{InVar2}, ... to the transformed variable 
\bold{OutVar}, where the variables have the data types \bold{InType1}, \bold{InType2}, ... and \bold{OutType}, the 
 map command is in the format:

 xformInfo = "[InVar1,InVar2,...->OutVar][InType1,InType2,...->OutType]",\cr
 table="TableFileName", defaultValue="defVal", mapMissingTo="missingVal"

where \bold{TableFileName} is the name of the CSV file containing the map. The map can be a N to 1 map where
N is greater or equal to 1. The data types of the variables can be any of the ones defined in the PMML 
format including integer, double or string. \bold{defVal} is the default value of the transformed variable
and if any of the map input values are missing, \bold{missingVal} is the value of the transformed variable. 

The arguments InType, OutType, defaultValue and mapMissingTo 
are optional. The CSV file containing the table should not have any row and column identifiers, and the 
values given must be in the same order as in the map command. If the data types of the variables are not 
given, the data types of the input variables are attempted to be determined from the \bold{boxData} argument. 
If that is not possible, the data type is assumed to be string.

It is also possible to give the maps to be implemented without an external file using a list of data frames. Each
data frame defines a map for 1 input variable. Given a data frame with N+1 columns, it is assumed that the map is
a N to 1 map where the last column of the data frame corresponds to the derived field. The 1st row is assumed to be 
the names of the fields and the second row the data types of the fields. The rest of the rows define the map; each
combination of the input values in a row is mapped to the value in the last column of that row. The second row with
the data types of the fields is not required. If not given, all fields are assumed to be strings. In this input format,
the 'defaultValue' and 'mapMissingTo' parameters should be vectors. The first element of each vector will correspond 
to the derived field defined in the 1st element of the 'xformInfo' list etc. These are made clearer in the example below. 
}
\value{
R object containing the raw data, the transformed data and data statistics.
}

\author{
Tridivesh Jena, Zementis, Inc.
}

\seealso{
 \code{\link{WrapData}},
 \code{\link[pmml]{pmml}}
}
\examples{
# Load the standard audit dataset, part of the pmml package
   library(pmml)
   library(pmmlTransformations)
   data(audit)

# First wrap the data
   auditBox <- WrapData(audit)

\dontrun{
# One of the variables, "Sex", has 2 possible values: "Male" 
# and "Female". If these string values have to be mapped to a 
# numeric value, a file has to becreated, say "MapGender.csv" 
# whose content is, for example:
#
#  Male,1
#  Female,2
#
# Transform the variable "Gender" to a variable "d_gender" 
# such that:
#    if Sex = "Male" then d_sex = "1"
#    if Sex = "Female" then d_sex = "0"
#
# Give "d_sex" the value 0 if the input variable value is 
# missing.
  auditBox <- MapXform(auditBox,
                      xformInfo="[Sex -> d_sex][string->integer]", 
	                    table="MapGender.csv",mapMissingTo="0")
}
# same as above, with an extra variable, but using data frames.
# The top 2 rows gives the variable names and their data types. 
# The rest represent the map. So for example, the third row 
# indicates that when the input variable "Sex" has the value 
# "Male" and the input variable "Employment" has
# the value "PSLocal", the output variable "d_sex" should have 
# the value 1. 
   t <- list()
   m <- data.frame(c("Sex","string","Male","Female"),
                  c("Employment","string","PSLocal","PSState"),
	c("d_sex","integer",1,0))
   t[[1]] <- m

# give default value as a vector and missing value as a string,
# this is only possible as there is only one map defined. If 
# default values is not given, it will simply not be given in 
# the PMML file as well. In general, the default values and the 
# missing values should be given as a vector, each element of 
# the vector corresponding to the element at the same index in 
# the list. If these values are not given as a vector, they will 
# be used for the first list element only.
   auditBox<-MapXform(auditBox,xformInfo=t,defaultValue=c(3),
                      mapMissingTo="2")

# check what the pmml looks like
  fit<-lm(Adjusted~.,data=auditBox$data)
  # pmml(fit,transforms=auditBox)

}
\keyword{ manip }
