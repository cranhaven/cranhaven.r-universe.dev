\name{DiscretizeXform}
\alias{DiscretizeXform}
\title{
Discretizes a continuous variable to a discrete one as indicated by interval mappings.
This is in accordance to the PMML element:\cr \bold{Discretize}
}
\description{
Creates a discrete variable from a continuous one. The discrete variable value depends on which interval
the continuous variable value lies in. The mapping from intervals to discrete values can be given 
in an external table file referred to in the transform command or as a list of data frames.
}
\usage{
DiscretizeXform(boxdata, xformInfo, table, defaultValue=NA, 
                mapMissingTo=NA, ...)
}
\arguments{
  \item{boxdata}{
   the wrapper object obtained by using the WrapData function on the raw data. 
  }
  \item{xformInfo}{specification of details of the transformation. This may be a name of an external file 
		or a list of data frames. Even if only 1 variable is to be transformed, the
                information for that transform should be given as a list with 1 element.}
  \item{table}{name of external CSV file containing the map from input to output values.}
  \item{defaultValue}{value to be given to the transformed variable if the value of the input variable 
		does not lie in any of the defined intervals. If 'xformInfo' is a
                list, this is a vector with each element corresponding to the corresponding list element.}
  \item{mapMissingTo}{value to be given to the transformed variable if the value of the input variable is missing.
		If 'xformInfo' is a list, this is a vector with each element corresponding to the corresponding list element.}
  \item{\dots}{
    further arguments passed to or from other methods.
  }
}

\details{
Given a list of intervals and the discrete value each interval is linked to, a discrete variable is defined 
with the value indicated by the interval where it lies in. If a continuous variable \bold{InVar} of data type 
\bold{InType} is to be converted to a variable \bold{OutVar} of data type \bold{OutType}, the transformation command 
is in the format:

   xformInfo = "[InVar->OutVar][InType->OutType]", table="TableFileName", \cr
   defaultValue="defVal", mapMissingTo="missingVal"

where \bold{TableFileName} is the name of the CSV file containing the interval to discrete value map. 
The data types of the variables can be any of the ones defined in the PMML format including integer, 
double or string. \bold{defVal} is the default value of the transformed variable and if any of the input
values are missing, \bold{missingVal} is the value of the transformed variable.

The arguments InType, OutType, defaultValue and mapMissingTo 
are optional. The CSV file containing the table should not have any row and column identifiers, and the 
values given must be in the same order as in the map command. If the data types of the variables are not 
given, the data types of the input variables are attempted to be determined from the \bold{boxData} argument. 
If that is not possible, the data types are assumed to be string.

Intervals are either given by the left or right limits, in which case the other limit is considered as 
infinite. It may also be given by both the left and right limits separated by the character ":". An example 
of how intervals should be defined in the external file are:

   rightVal1),outVal1 \cr 
   rightVal2],outVal2 \cr
   [leftVal1:rightVal3),outVal3 \cr
   (leftVal2:rightVal4],outVal4 \cr
   (leftVal,outVal5
   
which, given an input value \bold{inVal} and the output value to be calculated \bold{out}, means that:

	if(inVal <  rightVal1) out=outVal1 \cr
	if(inVal <= rightVal2) out=outVal2 \cr
	if( (inVal >= leftVal1) and (inVal < rightVal3) ) out=outVal3 \cr
	if( (inVal >  leftVal2) and (inVal <= rightVal4) ) out=outVal4 \cr
	if(inVal > leftVal) out=outVal5

It is also possible to give the information about the transforms without an external file, using a list of data frames. 
Each data frame defines a discretization operation for 1 input variable. The first row of the data frame gives the 
original field name, the derived field name, the left interval, the left value, the right interval and the right value.
The second row gives the data type of the values as listed in the first row. The second row with
the data types of the fields is not required. If not given, all fields are assumed to be strings. In this input format,
the 'defaultValue' and 'mapMissingTo' parameters should be vectors. The first element of each vector will correspond
to the derived field defined in the 1st element of the 'xformInfo' list etc. Although somewhat more complicated, this
method is designed to not require any external features. Further, once the intial list is constructed, modifying it is 
a simple operation; making this a better method to use if the parameters of the transformation are to be modified frequently
and/or automatically. This is made more clear in the example below.
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
# Load the pmmlTransformations package
    library(pmmlTransformations)
    library(pmml)
# First wrap the data
    irisBox <- WrapData(iris)

\dontrun{
# We wish to convert the continuous variable "Sepal.Length" to a discrete
# variable "dsl". The intervals to be used for this transformation is 
# given in a file, "intervals.csv", whose content is, for example,:
#
#  5],val1
#  (5:6],22
#  (6,val2
#
# This will be used to create a discrete variable named "dsl" of dataType
# "string" such that:
#    if(Sepal.length <= 5) then dsl = "val1"  
#    if((Sepal.Lenght > 5) and (Sepal.Length <= 6)) then dsl = "22"
#    if(Sepal.Length > 6) then dsl = "val2" 
#
# Give "dsl" the value 0 if the input variable value is missing.
  irisBox <- DiscretizeXform(irisBox,
              xformInfo="[Sepal.Length -> dsl][double -> string]", 
              table="intervals.csv",mapMissingTo="0")
}

# A different transformation using a list of data frames, of size 1:
  t <- list()
  m <- data.frame(rbind(
                  c("Petal.Length","dis_pl","leftInterval","leftValue",
                  "rightInterval","rightValue"),
                  c("double","integer","string","double","string",
                  "double"),
                  c("0)",0,"open",NA,"Open",0),
                  c(NA,1,"closed",0,"Open",1),
                  c(NA,2,"closed",1,"Open",2),
                  c(NA,3,"closed",2,"Open",3), 
                  c(NA,4,"closed",3,"Open",4),
                  c("[4",5,"closed",4,"Open",NA)))

# Give column names to make it look nice; not necessary!
  colnames(m) <- c("Petal.Length","dis_pl","leftInterval","leftValue",
                  "rightInterval","rightValue")

# a textual representation of the data frame is:
#   Petal.Length  dis_pl leftInterval leftValue rightInterval rightValue
# 1 Petal.Length  dis_pl leftInterval leftValue rightInterval rightValue
# 2       double integer       string    double        string     double
# 3           0)       0         open      <NA>          Open          0
# 4         <NA>       1       closed         0          Open          1
# 5         <NA>       2       closed         1          Open          2
# 6         <NA>       3       closed         2          Open          3
# 7         <NA>       4       closed         3          Open          4
# 8           (4       5       closed         4          Open       <NA>
#
# This is a transformation that defines a derived field 'dis_pl' 
# which has the integer value '0' if the original field 
# 'Petal.Length' has a value less than 0. The derived field has a 
# value '1' if the input is greater than or equal to 0 and less 
# than 1. Note that the values of the 1st column after row 2 have 
# been deliberately given NA values in the middle. This is to 
# show that that column is meant for a textual representation of 
# the transformation as defined for the method involving external 
# files; however in this methodtheir values are not used.

# Add the data frame to a list. The default values and the missing
# values should be given as a vector, each element of the vector 
# corresponding to the element at the same index in the list. If 
# these values are not given as a vector, they will be used for the 
# first list element only.
  t[[1]] <- m
  def <- c(11)
  mis <- c(22)
  irisBox<-DiscretizeXform(irisBox,xformInfo=t,defaultValue=def,
                          mapMissingTo=mis)

# Make a simple model to see the effect.
  fit<-lm(Petal.Width~.,irisBox$data[,-5])
  # pmml(fit,transforms=irisBox)
}
\keyword{ manip }
