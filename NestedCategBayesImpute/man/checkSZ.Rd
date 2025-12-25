\name{checkSZ}
\alias{checkSZ}
\title{
The new implementation of checkconstraints and will evently replace checkconstraints.
}
\description{
Checking a data matrix of households for the possible/impossible status under a predefined set of structural zeros.
}
\usage{
checkSZ(Data_to_check, h)
}
\arguments{
  \item{Data_to_check}{The household data matrix that is to be checked for structure zero constriants.}
  \item{h}{The household size for the households to be checked.}
}

\details{
Given an input household data matrix, these functions will check the possible/impossible status of each household and also output the desired number of possible and impossible households separately. \verb{checkconstraints} checks constraints when the household head is included as an individual within the household.

The predefined list of structural zeros currently included should be viewed as an example of a system of constraints. It was derived by treating a subset of the 2012 American Community Survey as a population, and identifying combinations involving the relationship variable that do not appear in the data.  This list should not be interpreted as a ``true'' list of impossible combinations in the target population. We force the combinations of variables in this list to have zero probability to be consistent with the 2012 ACS public use file that we used in the example.

The structural zeros included are:
\itemize{
\item Each household must contain exactly one head and he/she must be at least 16 years old.
\item Each household cannot contain more than one spouse and he/she must be at least 16 years old.
\item Married couples are of opposite sex, and age difference between individuals in the couples cannot exceed 49.
\item The household head must be older than the oldest child by at least 7.
%\item The household head must be older than the oldest child-in-law by at least 10.
\item The youngest parent must be older than the household head by at least 10.
\item The youngest parent-in-law must be older than the household head by at least 4.
\item The age difference between the household head and siblings cannot exceed 37.
%\item Valid Siblings-in-Law: The age difference between the household head and siblings-in-law cannot exceed 33.
\item The household head must be at least 34 years old. Also, the household head must be older than the oldest grandchild by at least 26.
}
Users can modify the list of structural zeros by downloading the package source, making changes only to the checkconstraints_imp.cpp file and re-building the package. Please note that the structural zeros have been specified according to the structure of our example data so that the specific column indexes and levels of age, gender and relationship to household head variables in subsequent data sets must match those in our example data. For more information on the structure of the data, see the documentation of the \verb{RunModel} function.
}
\value{
A list containing information on checking result.
  \item{outcome }{An indicator vector for the possible/impossible household status under constraints.}
  \item{Households }{A data matrix for impossible households.}
  \item{Index }{A vector for the original indexes of households when possible households are found. Generally not to be used.}
  \item{synHouseholds }{A data matrix for possible households.}
  \item{possible }{The actual number of possible households returned.}
}

\author{
Quanli Wang, Olanrewaju Akande
}

\keyword{constraints}
\keyword{impossible household}
\keyword{possible household}
