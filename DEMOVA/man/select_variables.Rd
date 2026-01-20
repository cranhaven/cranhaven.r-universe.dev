\name{select_variables}
\alias{select_variables}
\title{
Selection of descriptors
}
\description{
This function allow the user to select wanted descriptors between both that are intercorrelated with a correlation coefficent higher that ThresholdInterCor. The selection can also be automatic based on the correlation with the property of each variables.
}
\usage{
select_variables(id, y, d, ThresholdInterCor, auto = FALSE)
}

\arguments{
  \item{id}{
List of the names of observations
}
  \item{y}{
List of the values of the property/response
}
  \item{d}{
Dataframe containing the names of desciptors and their values (without missing or constant values)
}
  \item{ThresholdInterCor}{
Threshold value (double) of the accepted intercorrelation between descriptors (should be between 0 and 1)
}
  \item{auto}{
Two possible values: TRUE or FALSE (by default). The selection of descriptors is done automatically based on the correlation between descriptor and property (auto=TRUE) or is done manually by user (auto=FALSE)
}
}

\value{
return a dataframe containing only of non intercorrelated variables
}

\examples{
# Run after Preselection : d<-Preselection(desc)

# desc<-select_variables(id,y,d,0.95)

}
