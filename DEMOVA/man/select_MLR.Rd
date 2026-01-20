\name{select_MLR}
\alias{select_MLR}
\title{
Development of the model (multi linear regression)
}
\description{
From a list of descriptors and responses values, this function choose the best compromise between correlation and robustness to select the best model.
}
\usage{
select_MLR(y, desc, n, method = "forward")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
Vector with values of the property/response
}
  \item{desc}{
Dataframe containing the names of desciptors and their values
}
  \item{n}{
Maximal number (integer) of desciptors for the final equation
}
  \item{method}{
Determine the method used to build the regression. Can be: "backward", "forward" (by default) or "seqrep". For more info see leaps package.
}
}


\value{
Return the list of selected variables for the choosen MLR.
}

\examples{
# First run Select_variables to remove descriptors with missing or constant values.
# MLR<-select_MLR(y,desc,5)
}
