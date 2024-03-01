\name{smoothagesccs}
\alias{smoothagesccs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Spline-based semiparametric SCCS, smooth age
 %%  ~~function to do ... ~~
}
\description{
Fits a semiparametric SCCS model with smooth age effect, where the age related relative incidence function is represented by spline function; that is, linear combinations of M-splines. The exposure related relative incidence function is represented by step functions. One exposure group can be included.%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
smoothagesccs(indiv, astart, aend, aevent, adrug, aedrug, expogrp = 0,
              washout = NULL, kn=12, sp = NULL, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

\item{indiv}{
a vector of individual identifiers of cases. %%     ~~Describe \code{adrug} here~~
}

\item{astart}{
a vector of ages at which observation periods start. 
}


\item{aend}{
a vector of ages at end of observation periods. 
}


\item{aevent}{
a vector of ages at event, an individual can experience multiple events.
}

\item{adrug}{
a vector of ages at which exposure starts, only a single exposure type can be included.  %%     ~~Describe \code{adrug} here~~
}
  \item{aedrug}{
a vector of ages at which the exposure-related risk periods end.
}
  \item{expogrp}{
a vector of days to the start of exposure-related risk, counted from \code{adrug}. E.g if the risk period is [\code{adrug}+c,\code{aedrug}], use expogrp = c. To define multiple risk windows, \code{expogrp} is a vector of days on start of risk periods counted from \code{adrug}. The DEFAULT is  zero where the exposure-related risk periods are [\code{adrug}, \code{aedrug}].
}
  \item{washout}{
a vector of days to start of washout periods counted from \code{aedrug}. The default is  NULL, no washout periods. %%     ~~Describe \code{washout} here~~
}

\item{kn}{
an integer >=5 representing the number of interior knots used to define the M-spline basis functions which are related to the age specific relative incidence function, usually between 8 and 12 knots is sufficient. It defaults to 12 knots. %%     ~~Describe \code{formula} here~~
}

\item{sp}{
smoothing parameter value. It defaults to "auto" where the smoothing paramter is obtained automatically using a cross-validation method. The value of "sp" must be a number greater or equal to 0.
}


\item{data}{
a data frame containing the input data. The data are assembled one line per event.
}

}
\details{
The standard SCCS represents the age and exposure effects by piecewise constant step functions, however mis-specification of age group cut points might lead to biased estimates of the exposure related relative incidences. The semiparametric SCCS model, \code{semisccs}, has numerical challenges when the number of cases is large. This splined-based semiparametric SCCS model with smooth age effect avoids these limitations of the standard and semiparametric SCCS models. The smoothing parameter for the age-related relative incidence function is chosen by an approximate cross-validation method. The method is outlined in Ghebremichael-Weldeselassie et al (2014). 
}
\value{
Relative incidence estimates along with their 95\% confidence limits. 
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{coef}{log of the exposure related relative incidence estimates.}
  
  \item{se}{standard errors of the log of exposure related relative incidence estimates.}
  
  
  \item{age}{age related relative incidences at each day between the minimum age at start of observation and maximum age at end of observation periods.}
   
   \item{ageaxis}{sequence of ages between the minimum age at start of observations and maximum age at end of observation periods corresponding to the age related relative incidences.}
   
   \item{smoothingpara}{smoothing parameter chosen by maximizing an approximate cross-validation score or given as an argument in the function}
   
    \item{cv}{cross-validation score}
  
%% ...
}
\references{
%Farrington, C.P. (1995). Relative incidence estimation from case series for vaccine safety evaluation. Biometrics 51, 228--235.

Ghebremichael-Weldeselassie, Y., Whitaker, H. J., Farrington, C. P. (2015). Self-controlled case series method with smooth age effect. Statistics in Medicine, 33(4), 639-649. 


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
\code{\link{smoothexposccs}}
}
\examples{

# Fit the SCCS model with smooth age effect to the itp data and plot age effect.  

itp.mod <- smoothagesccs(indiv=case, astart=sta,aend=end, aevent=itp,
                           adrug=mmr, aedrug=mmr+42, expogrp=c(0,15,29), sp=2800,
                           data=itpdat)

itp.mod

plot(itp.mod)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Splines}
\keyword{Semi-parametric }% __ONLY ONE__ keyword per line
