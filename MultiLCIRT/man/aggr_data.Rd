\name{aggr_data}
\alias{aggr_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Aggregate data}
\description{Given a matrix of configurations (covariates and responses) unit-by-unit, this
	function finds the corresponding matrix of distinct configurations and the corresponding 
	vector of frequencies (it does not work properly with missing data).}
\usage{
aggr_data(data, disp=FALSE, fort=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{matrix of covariate and unit-by-unit response configurations}
  \item{disp}{to display partial results}
  \item{fort}{to use fortran routines when possible}
}

%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}

\value{
  \item{data_dis}{matrix of distinct configurations}
  \item{freq}{vector of corresponding frequencies}
  \item{label}{the index of each provided response configuration among the distinct ones}
}

%\references{
%%% ~put references to the literature/web site here ~
%}

\author{Francesco Bartolucci - University of Perugia (IT)}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{
# draw a matrix of random responses and find distinct responses
X = matrix(sample(5,100,replace=TRUE),50,2)
out = aggr_data(X)

# find the distinct responses and the corresponding vector of frequencies
# for naep data
data(naep)
X = as.matrix(naep)
out = aggr_data(X)
length(out$freq)
}

%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
