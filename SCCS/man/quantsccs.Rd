\name{quantsccs}
\alias{quantsccs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Quantitative exposures in self controlled case series method
 %%  ~~function to do ... ~~
}
\description{
This function fits the starndard SCCS model where the exposures are measured on a continuous scale.
}
\usage{
quantsccs(formula, indiv, event, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
model formula. The dependent variale should always be "event" e.g. event ~ expo, where expo is an exposure measured at each time unit.
}
 
\item{indiv}{
a vector of individual identifiers of cases %%     ~~Describe \code{adrug} here~~
}


\item{event}{
number of events occuring at each time unit.
}

\item{data}{
a data frame containing the input data. Data are assembled one line per time unit of observation.
}
}
\details{
In this method exposures are measured at successive time points within the observation period for each case. And number of events experienced by each case at each time point are recorded. 

}

\value{
Relative incidence estimates along with their 95\% confidence intervals.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Farrington P., Whitaker H., and Ghebremichael-Weldeselassie Y. (2018). Self-controlled Case Series Studies: A modelling Guide with R. Boca Raton: Chapman & Hall/CRC Press.

%% ~put references to the literature/web site here ~
}
\author{
Yonas Ghebremichael-Weldeselassie, Heather Whitaker, Paddy Farrington. %%  ~~who you are~~
}

%\note{
%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{semisccs}}
}
\examples{

# Headaches and blood pressure data. sys and dia (continuoous exposures) are systolic and
# diastolic blood pressure measurements respectively

 bp.mod <- quantsccs(event~sys+dia, indiv=case, event=head,
                      data=bpdat)

 bp.mod
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Parametric}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
