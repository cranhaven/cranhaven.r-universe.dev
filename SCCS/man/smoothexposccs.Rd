\name{smoothexposccs}
\alias{smoothexposccs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Spline-based semiparametric SCCS, smooth exposure
 %%  ~~function to do ... ~~
}
\description{
Fits a spline-based SCCS model where the exposure-related relative incidence function is represented by a spline function, that is a linear combination of M-splines, and the age effects are represented by a piecewise constant function. %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
smoothexposccs(indiv, astart, aend, aevent, adrug, aedrug, agegrp, kn=12, 
               sp = NULL, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{


\item{indiv}{
a vector of individual identifiers of cases. %%     ~~Describe \code{adrug} here~~
}

\item{astart}{
a vector of ages at which the observation periods start. 
}


\item{aend}{
a vector of ages at end of observation periods. 
}


\item{aevent}{
a vector of ages at event (outcome of interest), an individual can experience multiple events.
}

\item{adrug}{
a vector of ages at which exposure related risk period starts.
}
  \item{aedrug}{
a vector of ages at which exposure related risk period ends.
}

  \item{agegrp}{
a vector of cut points for the age groups where each value represents the start of an age catagory. The first element in the vector is the start of the second age group. The first age group starts at the minimum of \code{astart}, the start of the observation period.
}
  \item{kn}{
 number of interior knots >=5 used to define the M-spline basis functions, usually between 8 and 12 knots is sufficient. The default is 12. %%     ~~Describe \code{formula} here~~
}

\item{sp}{
smoothing parameter value. It defaults to "auto" where the smoothing paramter is obtained automatically using a cross validation method. The value of "sp" must be a number greater or equal to 0.
}

  \item{data}{
a data frame containing the input data.
}
}
\details{
The \code{\link{standardsccs}}, \code{\link{semisccs}} and \code{\link{smoothagesccs}} use piecewise constant step functions to model the exposure effect. However mis-specification of exposure group cut points might result in biased estimates. This method represents exposure related relative incidence function by a spline function.
}
\value{
Relative incidence estimates along with their 95\% confidence limits. Varaince-covariance matrix can also be obtained. 
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{estimates}{exposure related relative incidence estimates at each point of time since start of exposure until the maximum duration of exposure.}
  \item{lci }{lower confidence limits of the exposure-related relative incidence estimates.}
  \item{uci }{upper confidence limits of the exposure-related relative incidence estimates.}
%% ...
}
\references{

% Farrington, C.P. (1995). Relative incidence estimation from case series for vaccine safety evaluation. Biometrics 51, 228--235.

Ghebremichael-Weldeselassie, Y., Whitaker, H. J., Farrington, C. P. (2015). Flexible modelling of vaccine effects in self-controlled case series models 25, 1768--1797.


Farrington, P., Whitaker, H., and Ghebremichael-Weldeselassie, Y. (2018). Self-controlled Case Series Studies: A modelling Guide with R. Boca Raton: Chapman & Hall/CRC Press.

%% ~put references to the literature/web site here ~
}
\author{
Yonas Ghebremichael-Weldeselassie, Heather Whitaker, Paddy Farrington. %%  ~~who you are~~
}
%\note{
%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{smoothagesccs}}, \code{\link{nonparasccs}}
}
\examples{

library(SCCS)

# Fit smooth exposure SCCS to MMR vaccine and itp 

 itp.mod1 <- smoothexposccs(sp=10, indiv=case, astart=sta, aend=end,
                           aevent=itp, adrug=mmr, aedrug=mmr+42,
                           agegrp=c(427, 488, 549, 610, 671), data=itpdat)
 
 itp.mod1
 
 plot(itp.mod1)
 
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Splines}
\keyword{Semi-parametric SCCS}% __ONLY ONE__ keyword per line
