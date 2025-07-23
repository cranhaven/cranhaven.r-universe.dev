\name{splitMedian}
\alias{splitMedian}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Discretise continuous data in multiple granularities
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Discretise continuous co-data by making groups of covariates of various size. The first group is the group with all covariates.
Each group is then recursively split in two at the median co-data value, until some user-specified minimum group size is reached.
The discretised groups are used for adaptive discretisation of continuous co-data.
}
\usage{
splitMedian(values, index=NULL, depth=NULL, minGroupSize = 50, first = TRUE, 
  split = c("both","lower","higher"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{values}{
%%     ~~Describe \code{values} here~~
Vector with the continuous co-data values to be discretised.
}
  \item{index}{
%%     ~~Describe \code{index} here~~
Index of the covariates corresponding to the values supplied. Useful if part of the continuous co-data is missing and only the non-missing part should be discretised.
}
  \item{depth}{
%%     ~~Describe \code{depth} here~~
(optional): if given, a discretisation is returned with 'depth' levels of granularity.
}
  \item{minGroupSize}{
%%     ~~Describe \code{minGroupSize} here~~
Minimum group size that each group of covariates should have.
}
  \item{split}{
%%     ~~Describe \code{split} here~~
"both", "lower" or "higher": should both split groups of covariates be further split, or only the group of covariates that corresponds to the lower or higher continuous co-data group?
}
  \item{first}{
%%     ~~Describe \code{first} here~~
Do not change, recursion help variable.
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A list with groups of covariates, which may be used as group set in ecpc.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
Use \code{\link{obtainHierarchy}} to obtain a group set on group level defining the hierarchy for adaptive discretisation of continuous co-data.
}
\examples{

cont.codata <- seq(0,1,length.out=20) #continuous co-data
#full tree with minimum group size 5
groupset1 <- splitMedian(values=cont.codata,minGroupSize=5) 
#only split at lower continous co-data group
groupset2 <- splitMedian(values=cont.codata,split="lower",minGroupSize=5) 

part <- sample(1:length(cont.codata),15) #discretise only for a part of the continuous co-data
cont.codata[-part] <- NaN #suppose rest is missing
#make group set of non-missing values
groupset3 <- splitMedian(values=cont.codata[part],index=part,minGroupSize=5) 
groupset3 <- c(groupset3,list(which(is.nan(cont.codata)))) #add missing data group


}