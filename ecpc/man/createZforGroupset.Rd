\name{createZforGroupset}
\alias{createZforGroupset}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Create a co-data matrix Z for a group set
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Create a co-data matrix Z for a group set as obtained for instance with \link{createGroupset}.
}
\usage{
createZforGroupset(groupset,p=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{groupset}{
  A list with G elements that contain the indices of the variables belonging to each of the groups.
}
\item{p}{
Number of covariates in total. If not given, taken as maximum index in `groupset'. But in cases where some covariates are left unpenalised, the total number of covariates may be larger.
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A (pxG)-dimensional co-data matrix.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{createGroupset}}
}
\examples{
#Group set: G random groups
G <- 5 #number of groups
p <- 300 #number of covariates from which last 10 left unpenalised
#sample random categorical co-data:
categoricalRandom <- as.factor(sample(1:G,(p-10),TRUE))
#make group set, i.e. list with G groups
groupsetRandom <- createGroupset(categoricalRandom)
Zcat <- createZforGroupset(groupsetRandom,p=p)
}

